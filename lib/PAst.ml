open Mlsem.Common
open Mlsem.Types
module MVariable = Mlsem.Lang.MVariable

module Position = struct
  type t = Position.t
  let pp fmt pos = if pos = Position.dummy then Format.fprintf fmt "dummy"
        else  
          let start = Position.start_of_position pos in 
          let end_ = Position.end_of_position pos in
          Format.fprintf fmt "(%d,%d)-(%d,%d)" 
          start.pos_lnum (start.pos_cnum - start.pos_bol) end_.pos_lnum (end_.pos_cnum - start.pos_bol)
end

type const =
 | CStr of string 
 | CChar of char
 | CFloat of float 
 | CInt of int 
 | CBool of bool
 | CNull
 | CNa
 | CArray of const list
 [@@deriving show]

 type global_linkage =
  | Extern
  | Static
  | Definition
  [@@deriving show]

 type top_level_unit' =
  | Fundef of Ast.ctype * string * param list * e
  | MacroDef of string * param list * e
    (** A function-like macro, [#define NAME(params) body], typed as a
        function whose parameters and result are [Ast.Any]. It lives in its
        own namespace ([Name.Macro]): C keeps a macro apart from a variable
        or function of the same name, which only a [MacroCall] reaches. *)
  | TypeDecl of string * Ast.ctype
    (** Binds a named type in the decl map. Covers struct/union/enum
        declarations (payload is the corresponding [Ast.Struct]/[Ast.Union]/
        [Ast.Enum] ctype) as well as typedef aliases (payload is the aliased
        ctype). Both flow through a single arm downstream because they behave
        identically: add [name -> ctype] to [DeclMap]. *)
  | GlobalVar of global_linkage * string * Ast.ctype
    (** File-scope variable declaration or definition. Payload is the
        linkage, identifier, and declared type. Any initializer is dropped
        here; only the declared type seeds the identifier's binding in
        [Runner.infer_def]. *)
  | Define of string * const
  | Include of top_level_unit list (* items parsed from an included external header *)
  [@@deriving show]
 and top_level_unit = Position.t * top_level_unit'
  [@@deriving show]
 and e' = 
  | Const of const 
  | Id of string
  | Unop of string * e
  | FieldAccess of e * string (* For struct field access, e.g. x.f *)
  | Binop of string * (e * e) 
  | VarDeclare of Ast.ctype * e
  | VarAssign of e * e
  | Call of e * e list 
  | MacroCall of string * e list
    (** [NAME(args)] where [NAME] is a function-like macro defined before
        this point: an expansion of the macro, not a call of a variable or
        function of the same name. Emitted by the parser, which alone sees
        whether the callee was parenthesized: [(NAME)(args)] is a [Call]. *)
  | If of e * e * e option
  | Ite of e * e * e (* For ternary conditions *)
  | While of e * e
  | For of e * e option * e option * e (*init, condition, incr, body *)
  | Return of e option
  | Break
  | Next
  | Goto of string
    (** [goto label]. Not modeled by the typer yet: lowered to a no-op. *)
  | Seq of e list
  | Block of e list
    (** C compound statement [{ ... }]. Introduces a new lexical scope:
        declarations inside the [Block] are visible only until its end. Emitted
        by the parser for every [{ ... }] in the source, including function
        bodies. Distinct from [Seq], which is used for synthesised, non-scoping
        statement concatenations (for-loop body+incr, switch case bodies,
        multi-declarator [int a, b;]). *)
  | Comma of e * e
  | Case of e * e
  | Default of e
  | Switch of e * e list  (* expression on which to switch, list of cases/default *)
  | Cast of Ast.ctype * e (* Type cast expression *)
  | CoerceNarrow of Ast.ctype * e
    (** Implicit narrowing conversion synthesised at a typed pointer
        initialization (C's [void* -> T*] rule). Unlike [Cast] it lowers to an
        intersecting cast, so it only narrows the initializer's type toward the
        declared pointer type and never widens nor de-refines a more precise
        initializer. *)
  [@@deriving show]
 and param =
   | Param of Ast.ctype * string
   | Vararg (** Marks the variadic [...] position in a C function signature.
                Detected by [C_interface.is_variadic_params] and friends to
                type the function as [any -> ret]. *)
  [@@deriving show]
 and e = Position.t * e'
  [@@deriving show]

let top_level_unit_name top =
  match top with
  | _, Fundef (_, name, _, _) -> name
  | _, MacroDef (name, _, _) -> name
  | _, TypeDecl (name, _) -> name
  | _, GlobalVar (_, name, _) -> name
  | _, Define (name, _) -> name
  | _, Include _ -> ""


type definitions = top_level_unit list
[@@deriving show]

module StrSet = Set.Make(String)

(** Extract parameter names from a function definition.
    Types will be added back to the Ast.Function. This is just to check if some
    variables in the body are defined as parameters. [Vararg] binds no name. *)
let bv_params params =
  List.filter_map (function
    | Param (_, name) -> Some name
    | Vararg -> None) params
  |> StrSet.of_list

(** The immediate subexpressions of an expression, in source order. *)
let children (_, e) =
  match e with
  | Const _ | Id _ | Break | Next | Goto _ | Return None -> []
  | Unop (_, e1) | VarDeclare (_, e1) | FieldAccess (e1, _) | Default e1
  | Cast (_, e1) | CoerceNarrow (_, e1) | Return (Some e1) -> [e1]
  | Binop (_, (e1, e2)) | VarAssign (e1, e2) | Case (e1, e2) | Comma (e1, e2)
  | While (e1, e2) -> [e1; e2]
  | Call (f, args) -> f :: args
  | MacroCall (_, args) -> args
  | If (cond, then_, else_) -> cond :: then_ :: Option.to_list else_
  | Ite (cond, then_, else_) -> [cond; then_; else_]
  | For (init, cond, incr, body) ->
      (init :: Option.to_list cond) @ Option.to_list incr @ [body]
  | Switch (e1, cases) -> e1 :: cases
  | Seq es | Block es -> es

(** Extract variables from an expression*)
let rec bv_e in_lhs_assign (_,e) = 
  match e with 
  | Break | Next | Goto _ | Const _ -> StrSet.empty
  | Id s when in_lhs_assign -> StrSet.singleton s
  | Id _ -> StrSet.empty
  | Unop (("pre++" | "pre--" | "post++" | "post--"), e) -> bv_e true e
  | Unop (_, e) -> bv_e in_lhs_assign e
  | Binop (_, (e1,e2)) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | VarAssign ((_,Id s), e2) -> (StrSet.singleton s) |> StrSet.union (bv_e in_lhs_assign e2)
  | FieldAccess (e, _) -> bv_e in_lhs_assign e
  | VarAssign (e,_) -> bv_e true e (* Gross overapproximation! Left side of assignment, we want to collect identifiers in the lhs. Args will be detected as been "assigned". *)
  | Call (f, args) -> List.fold_left (fun acc arg -> StrSet.union acc (bv_e in_lhs_assign arg)) (bv_e  false f) args
  | MacroCall (_, args) -> List.fold_left (fun acc arg -> StrSet.union acc (bv_e in_lhs_assign arg)) StrSet.empty args
  | If (cond, then_, else_) -> 
      let acc = bv_e in_lhs_assign cond |> StrSet.union (bv_e in_lhs_assign then_) in
      begin match else_ with 
      | None -> acc
      | Some e -> StrSet.union acc (bv_e in_lhs_assign e)
      end
  | Ite (cond, then_, else_) -> 
      let acc = bv_e in_lhs_assign cond |> StrSet.union (bv_e in_lhs_assign then_) in
      StrSet.union acc (bv_e in_lhs_assign else_)
  | While (cond, body) -> 
      let acc = bv_e in_lhs_assign cond |> StrSet.union (bv_e in_lhs_assign body) in
      acc
  | For (init, cond, incr, body) ->
      let acc = bv_e in_lhs_assign init |> StrSet.union (match cond with 
        | None -> StrSet.empty
        | Some e -> bv_e in_lhs_assign e) in
      let acc = StrSet.union acc (match incr with 
        | None -> StrSet.empty
        | Some e -> bv_e in_lhs_assign e) in
      StrSet.union acc (bv_e in_lhs_assign body)
  | Return None -> StrSet.empty
  | Return (Some e) -> bv_e in_lhs_assign e
  | Case (e1, e2) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | Default e -> bv_e in_lhs_assign e
  | Switch (e, cases) ->
      let acc = bv_e in_lhs_assign e in
      List.fold_left (fun acc case -> StrSet.union acc (bv_e in_lhs_assign case)) acc cases
  | Seq exprs -> List.fold_left (fun acc e -> StrSet.union acc (bv_e in_lhs_assign e)) StrSet.empty exprs
  | Block exprs -> List.fold_left (fun acc e -> StrSet.union acc (bv_e in_lhs_assign e)) StrSet.empty exprs
  | Comma (e1, e2) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | Cast (_, e) -> bv_e in_lhs_assign e
  | CoerceNarrow (_, e) -> bv_e in_lhs_assign e
  | VarDeclare (_, (_, Id s)) -> StrSet.singleton s
  | VarDeclare (_, _) -> failwith "Declaration must have an identifier" (*Should be unreachable*)

module StrMap = Map.Make(String)

(* Object-like identifier-alias macros: [#define NAME TARGET] where [TARGET]
   is a bare C identifier. Recorded by the parser, resolved at use-site in
   [var]. Keeping the table here (rather than in [Parser]) avoids a module
   cycle: [Parser] depends on [PAst], and [var] needs to consult the table. *)
let define_aliases : string StrMap.t ref = ref StrMap.empty

let reset_define_aliases () = define_aliases := StrMap.empty

let remove_define_alias name =
  define_aliases := StrMap.remove name !define_aliases

let remember_define_alias name target =
  define_aliases := StrMap.add name target !define_aliases

(* Follow the alias chain transitively. A visited set terminates self-aliases
   and cycles on the last name in the chain. *)
let resolve_alias name =
  let rec loop seen n =
    if StrSet.mem n seen then n
    else
      match StrMap.find_opt n !define_aliases with
      | None -> n
      | Some target -> loop (StrSet.add n seen) target
  in
  loop StrSet.empty name

(** The namespaces a name is bound in. C keeps a function-like macro apart
    from an ordinary identifier (variable, function, enum constant) of the same
    name: rlang has both a function pointer [r_stop_internal] and a macro
    [r_stop_internal(...)] that calls it as [(r_stop_internal)(...)]. *)
module Name = struct
  type t = Ident of string | Macro of string
  let compare = compare
  let to_string = function Ident s | Macro s -> s
end
module NameMap = Map.Make (Name)

(** The identifier environment is a stack of scope frames, innermost first.
    Each frame is a [Variable.t NameMap.t] mapping C identifiers to their
    [Variable.t]. Lookup walks frames head-to-tail (inner -> outer), so an
    inner declaration shadows outer bindings (parameters, globals, .ty
    definitions). The outermost frame holds globals and file-scope names
    that survive across the whole translation unit; new frames are pushed
    on entry to a [Block] (and to a parameter scope at function entry) and
    fall out naturally when [transform]/[aux_e] returns. *)
type env = {
  id: Variable.t NameMap.t list;
  decl: Ast.ctype Ast.DeclMap.t;
  ctypes: Ast.ctype Ast.VarMap.t;
    (** The declared (resolved) C types of the parameters and locals. *)
  cells: bool StrMap.t;
    (** The integer variables of the function being lowered that get an
        annotated cell, with whether it is [c_int] (see [annotated_cells]). *)
}

(* [resolve_ctype] expands Typerefs and empty-struct forward references by
   looking them up in [decl]. [visited] records struct/typedef names currently
   being expanded so that self-referential types terminate: back-edges are left
   as [Typeref name] / [Struct (name, [])] for [Ast.typeof_ctype] to close via
   Sstt's recursive-type machinery. *)
let rec resolve_ctype_aux visited decl ty =
  match ty with
  | Ast.Typeref name ->
      if StrSet.mem name visited then ty
      else (
        match Ast.DeclMap.find_opt name decl with
        | Some t -> resolve_ctype_aux (StrSet.add name visited) decl t
        | None -> ty
      )
  | Ast.Struct (name, []) ->
      if StrSet.mem name visited then ty
      else (
        match Ast.DeclMap.find_opt name decl with
        | Some (Ast.Struct (_, fields)) ->
            let visited = StrSet.add name visited in
            Ast.Struct (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
        | _ -> ty
      )
  | Ast.Struct (name, fields) ->
      let visited = StrSet.add name visited in
      Ast.Struct (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
  | Ast.Enum (name, []) -> (
      match Ast.DeclMap.find_opt name decl with
      | Some (Ast.Enum (_, enumerators)) -> Ast.Enum (name, enumerators)
      | _ -> ty
    )
  | Ast.Enum (name, enumerators) -> Ast.Enum (name, enumerators)
  | Ast.Ptr t ->
      let t' = resolve_ctype_aux visited decl t in
      (* R's canonical definition is [typedef struct SEXPREC *SEXP;]. Packages
         that vendor their own alias (e.g. rlang's [typedef struct SEXPREC
         r_obj;] and then [r_obj*]) would otherwise leave us with [*struct { }]
         — an opaque pointer that no base.ty signature accepts. Collapse any
         pointer that resolves to [struct SEXPREC] down to [SEXP] here so the
         rest of the pipeline sees the familiar form regardless of the typedef
         name the package chose. *)
      (match t' with
       | Ast.Struct ("SEXPREC", _) -> Ast.SEXP
       | _ -> Ast.Ptr t')
  | Ast.Array (t, len) -> Ast.Array (resolve_ctype_aux visited decl t, len)
  | Ast.Union (name, []) ->
      if StrSet.mem name visited then ty
      else (
        match Ast.DeclMap.find_opt name decl with
        | Some (Ast.Union (_, fields)) ->
            let visited = StrSet.add name visited in
            Ast.Union (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
        | _ -> ty
      )
  | Ast.Union (name, fields) ->
      let visited = StrSet.add name visited in
      Ast.Union (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
  | Ast.FunPtr (ret, params, variadic) ->
      Ast.FunPtr (resolve_ctype_aux visited decl ret,
                  List.map (resolve_ctype_aux visited decl) params,
                  variadic)
  | _ -> ty

let resolve_ctype decl ty = resolve_ctype_aux StrSet.empty decl ty

let rec has_struct_type ty =
  match ty with
  | Ast.Struct _ | Ast.Union _ -> true
  | Ast.Ptr t -> has_struct_type t
  | Ast.Array (t, _) -> has_struct_type t
  | _ -> false

(* The statements [rest] following the declaration of the struct variable [s]
   (fields [fields]), adjusted to what C does with its initializer. The
   declaration itself gives every field its declared type (a sound
   over-approximation of C's zero value, see [aux_block]); the parser then
   lowers the initializer without knowing the fields:

   - a designated list [{ .a = 1 }] is seeded with an empty record, which
     would drop the fields it does not name: the seed is removed;
   - a literal [0] written to a pointer field ([.p = 0]) is a NULL pointer, not
     an integer;
   - a positional list of constants ([{ 0 }], [{ 1, 2 }]) is assigned as an
     array: its constants go to the fields in declaration order instead. *)
let adjust_struct_init s fields rest =
  let is_pointer f =
    match List.find_map (fun (ty, n) -> if n = f then Some ty else None) fields with
    | Some (Ast.Ptr _ | Ast.FunPtr _) -> true
    | _ -> false
  in
  let field_value f (p, e) =
    match e with
    | Const (CInt 0) when is_pointer f -> (p, Const CNull)
    | _ -> (p, e)
  in
  let rec designated = function
    | (p, VarAssign ((pl, FieldAccess ((pb, Id s'), f)), rhs)) :: tl when s' = s ->
        (p, VarAssign ((pl, FieldAccess ((pb, Id s'), f)), field_value f rhs)) :: designated tl
    | stmts -> stmts
  in
  match rest with
  | (_, VarAssign ((_, Id s'), (_, Cast (Ast.Struct ("", []), (_, Const CNull))))) :: tl
    when s' = s -> designated tl
  | (p, VarAssign ((_, Id s'), (_, Const (CArray cs)))) :: tl when s' = s ->
      let rec positional fields cs =
        match fields, cs with
        | (_, f) :: fields, c :: cs ->
            (match c with
             | CArray _ -> positional fields cs (* a nested aggregate: left at its declared type *)
             | c ->
                 (p, VarAssign ((p, FieldAccess ((p, Id s), f)), field_value f (p, Const c)))
                 :: positional fields cs)
        | _ -> []
      in
      positional fields cs @ tl
  | _ -> rest

(* Full lookup chain for [str]: scope frames (innermost first), builtin op,
   builtin var, .ty bindings. Returns [Some v] if [str] resolves to a
   concrete binding, [None] if it would fall through to the fresh-variable
   case. *)
let rec lookup_in_frames name = function
  | [] -> None
  | frame :: rest ->
      (match NameMap.find_opt name frame with
       | Some _ as r -> r
       | None -> lookup_in_frames name rest)

let lookup_binding env str =
  match lookup_in_frames (Name.Ident str) env.id with
  | Some _ as r -> r
  | None ->
    (match Defs.BuiltinOp.find_builtin str with
     | Some _ as r -> r
     | None ->
       (match Defs.BuiltinVar.find_builtin_var str with
        | Some _ as r -> r
        | None -> Defs.StrMap.find_opt str Defs.defs_map))

(* True when [str] is a variable declared in a *block* scope of the function
   body currently being lowered (an upper scope frame), as opposed to a
   file-scope global or a parameter. The base frame (always the last in
   [env.id]) holds globals — [Runner.infer_def] threads them through [idenv],
   which seeds that frame — and the function's parameters; block-local
   declarations are pushed into frames above it. The distinction gates the
   field-write rebind below: rebinding a global struct would clobber its
   declared record type with the narrow record accumulated from one function's
   writes. *)
let is_block_local env str =
  match env.id with
  | [] | [_] -> false
  | frames ->
      let upper = List.filteri (fun i _ -> i < List.length frames - 1) frames in
      Option.is_some (lookup_in_frames (Name.Ident str) upper)

(* Number of named parameters of each variadic definition translated so far
   (C functions [f(const char *fmt, ...)] and variadic macros alike). A
   definition is typed from its named parameters only, so a call passing more
   arguments has them evaluated but not passed (see [process_call]). Callees
   are typed before their callers, so the entry exists when a call is
   translated. *)
let variadic_arity : (Name.t, int) Hashtbl.t = Hashtbl.create 64

let var env str =
  (* Resolve identifier-alias macros ([#define NAME TARGET]) before looking
     up. Keep the resolution only if [TARGET] is itself bound; otherwise the
     alias gives us nothing and we preserve the original name so inference
     sees the same fresh variable as before aliases were supported. *)
  let effective =
    let resolved = resolve_alias str in
    if resolved = str then str
    else match lookup_binding env resolved with
      | Some _ -> resolved
      | None -> str
  in
  match lookup_binding env effective with
  | Some v -> v
  | None ->
    Printf.printf "Creating fresh variable: %s\n" effective;
    Defs.BuiltinVar.register_dynamic effective Ty.any

(** Push an empty scope frame on top of the env. The previous frames remain
    reachable through [lookup_binding] for shadowing-fall-through. *)
let push_scope env = {env with id = NameMap.empty :: env.id}

(** Bind [str] to a fresh [Variable.t] in the topmost scope frame of [env].
    Returns the updated env and the new variable. *)
let add_var_top ?ty env str =
  let kind =
    match ty, StrMap.find_opt str env.cells with
    | Some Ast.Int, Some never_na ->
        let cell = if never_na then Rstt.Cint.any else Rstt.Cint.any_na in
        MVariable.AnnotMut (GTy.mk cell)
    | _ -> MVariable.Mut
  in
  let v = MVariable.create kind (Some str) in
  let id = match env.id with
    | [] -> [NameMap.singleton (Name.Ident str) v]
    | top :: rest -> NameMap.add (Name.Ident str) v top :: rest
  in
  let ctypes = match ty with
    | Some ty -> Ast.VarMap.add v (resolve_ctype env.decl ty) env.ctypes
    | None -> env.ctypes
  in
  ({env with id; ctypes}, v)

let mk_e env eid expr =
  (eid, env.decl, Ast.VarMap.empty, expr)

let fresh_e env expr =
  mk_e env (Eid.unique ()) expr

(* Functions whose first positional argument is a SEXPTYPE tag. When the
   first arg is a literal int (or an enum constant the registered lookup
   knows the value of), [process_call] rewrites it to the corresponding
   SEXPTYPE [Id] so the call matches the [prim]-tag domain of the
   signatures in [types/base.ty]. Includes the [Rf_]-prefixed alias
   variants because the call site may use either name. *)
let sexptype_first_arg_callees =
  StrSet.of_list
    ["allocVector"; "Rf_allocVector"; "R_allocResizableVector"]

(* Lookup hook for resolving an identifier to its integer value (typically
   an enum constant). [Const_eval] installs the real implementation at
   library init time; [process_call] consults it when rewriting SEXPTYPE
   arguments. Kept as a ref to avoid a [PAst] -> [Const_eval] -> ... ->
   [PAst] module cycle. *)
let enum_int_lookup : (string -> int option) ref = ref (fun _ -> None)
let set_enum_int_lookup f = enum_int_lookup := f

let rec aux_const c =
  match c with 
  | CChar c -> Ast.CChar c
  | CStr s -> Ast.CStr s 
  | CFloat s -> Ast.CDbl s
  | CInt i -> Ast.CInt i
  | CNull -> Ast.CNull
  | CNa -> Ast.CNa
  | CBool b -> Ast.CBool b
  | CArray lst -> Ast.CArray (List.map aux_const lst)



(* A fresh local holding an intermediate value of a lowering. '#' cannot occur
   in a C identifier, so its name never clashes with the program's. *)
(* The integer variables of a function body that get an annotated cell
   ([MVariable.AnnotMut]) instead of an unannotated one, with whether they are
   never NA. An unannotated mutable variable assigned in a loop has a cell
   typed by an inference variable, and each overloaded operation on its value
   ([i + 1], [i < n]) splits on that variable: inference becomes exponential in
   the number of such operations. A variable that a loop assigns, declared
   outside it (so its value reaches the next iteration), gets a cell of fixed
   type instead: [c_int] when every assignment to it gives an integer that
   cannot be NA (constants, comparisons, arithmetic on such variables),
   [c_int_na] when it may be NA and is computed from itself. Variables are
   identified by name: shadowing only makes the analysis more conservative. *)
let annotated_cells params body =
  (* The names assigned in a loop and declared outside it (innermost loop). *)
  let carried = ref StrSet.empty in
  let assigned_in name in_loop local =
    if in_loop && not (StrSet.mem name local) then carried := StrSet.add name !carried
  in
  let rec walk in_loop local ((_, e) as node) =
    match e with
    | While (c, b) ->
        ignore (List.fold_left (fun local e -> walk true local e) StrSet.empty [ c ; b ]) ;
        local
    | For (init, c, incr, b) ->
        let local = walk in_loop local init in
        let loop_parts = Option.to_list c @ Option.to_list incr @ [ b ] in
        ignore (List.fold_left (fun local e -> walk true local e) StrSet.empty loop_parts) ;
        local
    | VarDeclare (_, (_, Id s)) -> if in_loop then StrSet.add s local else local
    | VarAssign ((_, Id s), e2) -> assigned_in s in_loop local ; walk in_loop local e2
    | Unop (("pre++" | "pre--" | "post++" | "post--"), (_, Id s)) ->
        assigned_in s in_loop local ; local
    | _ -> List.fold_left (walk in_loop) local (children node)
  in
  ignore (walk false StrSet.empty body) ;
  (* The declared [int]s among them. *)
  let ints = Hashtbl.create 16 in
  let rec decls (_, e as node) =
    (match e with
     | VarDeclare (Ast.Int, (_, Id s)) -> Hashtbl.replace ints s ()
     | _ -> ()) ;
    List.iter decls (children node)
  in
  decls body ;
  let params_ints = List.filter_map (function Param (Ast.Int, n) -> Some n | _ -> None) params in
  List.iter (fun n -> Hashtbl.replace ints n ()) params_ints ;
  let cells = StrSet.filter (Hashtbl.mem ints) !carried in
  (* What is assigned to each of them, anywhere in the function. A parameter
     also receives the caller's value, which may be NA. *)
  let rhs = Hashtbl.create 16 in
  let rec collect (_, e as node) =
    (match e with
     | VarAssign ((_, Id s), e2) when StrSet.mem s cells -> Hashtbl.add rhs s (Some e2)
     | _ -> ()) ;
    List.iter collect (children node)
  in
  collect body ;
  List.iter (fun n -> if StrSet.mem n cells then Hashtbl.add rhs n None) params_ints ;
  let rec never_na nn (_, e) =
    match e with
    | Const (CInt _ | CBool _) -> true
    | Id x -> StrSet.mem x nn
    | Unop ("-", e) -> never_na nn e
    | Unop ("!", _) -> true
    | Unop (("pre++" | "pre--" | "post++" | "post--"), (_, Id x)) -> StrSet.mem x nn
    | Binop (("<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||"), _) -> true
    | Binop (("+" | "-" | "*" | "/" | "%" | "<<" | ">>" | "&" | "|" | "^"), (a, b)) ->
        never_na nn a && never_na nn b
    | Ite (_, a, b) -> never_na nn a && never_na nn b
    | Comma (_, b) -> never_na nn b
    | VarAssign (_, e) -> never_na nn e
    | _ -> false
  in
  let rec fix nn =
    let nn' =
      StrSet.filter (fun s ->
        Hashtbl.find_all rhs s
        |> List.for_all (function Some e -> never_na nn e | None -> false)) nn
    in
    if StrSet.equal nn nn' then nn else fix nn'
  in
  let nn = fix cells in
  (* The others keep an unannotated cell, whose value inference can refine
     (from the contents of a parameter, say), unless the variable is computed
     from itself ([n = n - 1], [len--]): that is what makes inference
     exponential. *)
  let rec mentions s (_, e as node) =
    match e with
    | Id x -> x = s
    | _ -> List.exists (mentions s) (children node)
  in
  let self_dependent s =
    Hashtbl.find_all rhs s |> List.exists (function Some e -> mentions s e | None -> false)
  in
  let incremented = ref StrSet.empty in
  let rec incs (_, e as node) =
    (match e with
     | Unop (("pre++" | "pre--" | "post++" | "post--"), (_, Id x)) ->
         incremented := StrSet.add x !incremented
     | _ -> ()) ;
    List.iter incs (children node)
  in
  incs body ;
  StrSet.fold (fun s acc ->
    if StrSet.mem s nn then StrMap.add s true acc
    else if StrSet.mem s !incremented || self_dependent s then StrMap.add s false acc
    else acc) cells StrMap.empty

let tmp_counter = ref 0
let fresh_tmp env =
  incr tmp_counter ;
  let name = "tmp#" ^ string_of_int !tmp_counter in
  let env', v = add_var_top env name in
  (env', v, name)

let rec aux_e env (pos,e) = 
  let eid = Eid.unique_with_pos pos in
  let e = match e with 
  | Const c -> Ast.Const (aux_const c)
  | Id s -> Ast.Id (var env s)
  | Binop (("&&" | "||") as op, (e1, e2)) ->
      (* C's [&&] and [||] short-circuit: [b] is evaluated only when [a] does
         not decide. Lowered to conditionals, [a ? (b ? 1 : 0) : 0] and
         [a ? 1 : (b ? 1 : 0)], which also narrow [a] where [b] is evaluated:
         in [p && *p], [*p] is typed with [p] non-NULL. *)
      let one = (pos, Const (CInt 1)) and zero = (pos, Const (CInt 0)) in
      let b = (pos, Ite (e2, one, zero)) in
      let e = if op = "&&" then Ite (e1, b, zero) else Ite (e1, one, b) in
      let (_, _, _, e) = aux_e env (pos, e) in
      e
  | Binop (op, (e1,e2)) -> Ast.Binop (var env (op ^ "__2"), aux_e env e1, aux_e env e2)
  | VarAssign (lhs, e2) -> aux_assign_value env pos lhs e2
  | Unop (("pre++" | "pre--") as op, lhs) ->
      (* [++x]: [x = ++x], whose value is the new one. *)
      aux_assign_value env pos lhs (pos, Unop (String.sub op 3 2, lhs))
  | Unop (("post++" | "post--") as op, lhs) ->
      (* [x++]: the old value, kept in a temporary, then [x = ++old]. *)
      let env', old, old_name = fresh_tmp env in
      let assign = aux_assign env' pos lhs (pos, Unop (String.sub op 4 2, (pos, Id old_name))) in
      Ast.Let (old, aux_e env lhs,
        fresh_e env' (Ast.Seq (fresh_e env' assign, fresh_e env' (Ast.Id old))))
  | Unop (op, e) -> Ast.Unop (var env (op ^ "__1"), aux_e env e)
  | FieldAccess (e, field) -> Ast.FieldRead (aux_e env e, field)
  | Call (f, args) -> process_call env f args
  | MacroCall (name, args) ->
      (* The expansion of a macro bound in the [Name.Macro] namespace. One
         that is not bound (its definition was not reached, or failed without
         a fallback) is called like a function of that name. *)
      (match lookup_in_frames (Name.Macro name) env.id with
       | Some v -> call_variadic_aware env (Name.Macro name) (fresh_e env (Ast.Id v)) args
       | None -> process_call env (pos, Id name) args)
  | If (cond, then_, else_) -> 
      Ast.If (aux_e env cond, aux_e env then_, Option.map (aux_e env) else_)
  | Ite (cond, then_, else_) -> 
      Ast.Ite (aux_e env cond, aux_e env then_, aux_e env else_) 
  | While (cond, body) -> Ast.While (aux_e env cond, aux_e env body)
  | For (init, cond, incr, body) ->
      (* Transform For into While. A C99 [for (int i = 0; ...; ...)] declares
         [i] in a scope local to the loop; the parser emits the init as
         [VarDeclare] (no initializer) or [Seq [VarDeclare; VarAssign]]
         (with initializer), possibly with several declarators. We scan the
         init for declarations, push a fresh frame, register each declared
         name there, then translate init/cond/incr/body normally (the inner
         [VarDeclare] arm of [aux_e] will look up the name and reuse the
         pre-added [Variable.t]). The whole lowered while is wrapped in
         [Ast.Declare]s so the locals stay visible only inside the for. *)
      let rec collect_decls acc = function
        | _, VarDeclare (ty, (_, Id s)) -> (s, ty) :: acc
        | _, Seq inner -> List.fold_left collect_decls acc inner
        | _ -> acc
      in
      let decls = List.rev (collect_decls [] init) in
      let env_for, declared_vs =
        List.fold_left
          (fun (env, vs) (name, ty) ->
            let env', v = add_var_top ~ty env name in
            (env', v :: vs))
          (push_scope env, [])
          decls
      in
      let declared_vs = List.rev declared_vs in
      let init_e = aux_e env_for init in
      let cond_e = match cond with
        | None -> fresh_e env_for (Ast.Const (Ast.CBool true))
        | Some e -> aux_e env_for e
      in
      let incr_e = match incr with
        | None -> fresh_e env_for (Ast.Const Ast.CNull)
        | Some e -> aux_e env_for e
      in
      let while_body =
        let body_e = aux_e env_for body in
        fresh_e env_for (Ast.Seq (body_e, incr_e))
      in
      let while_e = fresh_e env_for (Ast.While (cond_e, while_body)) in
      let for_seq = fresh_e env_for (Ast.Seq (init_e, while_e)) in
      List.fold_right
        (fun v acc -> fresh_e env (Ast.Declare (v, acc)))
        declared_vs
        for_seq
      |> (fun (_, _, _, e) -> e)
  | Return None -> Ast.Return None
  | Return (Some e) -> Ast.Return (Some (aux_e env e))
  | Break -> Ast.Break
  | Next -> Ast.Next
  | Goto _ -> Ast.Const (aux_const CNull)
  | Switch  (e, cases) -> 
      (*Detects break and return; remove break.
        Recurses into nested Seq nodes so that a break inside a compound
        statement block (e.g. `case 1: { ...; break; }`) is properly detected
        and removed rather than leaking as an orphan Ret(BLoop) in the MLsem
        transform. We do NOT recurse into While/For/Switch bodies, so a break
        that targets an inner loop is left alone. *)
      let rec remove_break body =
        let scan sts =
          List.fold_left (fun (had_break, acc) stmt ->
            if had_break then (true, acc)
            else match stmt with
            | _, Break -> (true, acc)
            | _, Return _ -> (true, acc @ [stmt])
            | pos, (Seq _ | Block _) ->
                let has_brk, body' = remove_break (snd stmt) in
                (has_brk, acc @ [(pos, body')])
            | _ -> (false, acc @ [stmt])
          ) (false, []) sts
        in
        match body with
        | Seq sts ->
            let has_break_or_return, sts = scan sts in
            (has_break_or_return, Seq sts)
        | Block sts ->
            let has_break_or_return, sts = scan sts in
            (has_break_or_return, Block sts)
        | Break -> (true, Const CNull)
        | Return _ -> (true, body)
        | e -> (false, e)
      in
      (* The body of a [switch] is one block: a variable declared in a case
         without braces is in scope in the following cases. Its declarations
         are registered in a frame for the whole [switch], as for a [for]. *)
      let rec collect_decls acc = function
        | _, VarDeclare (ty, (_, Id s)) -> (s, ty) :: acc
        | _, Seq inner -> List.fold_left collect_decls acc inner
        | _, (Case (_, body) | Default body) -> collect_decls acc body
        | _ -> acc
      in
      let decls = List.rev (List.fold_left collect_decls [] cases) in
      let env_sw, declared_vs =
        List.fold_left
          (fun (env, vs) (name, ty) ->
            let env', v = add_var_top ~ty env name in
            (env', v :: vs))
          (push_scope env, [])
          decls
      in
      let aux_cases = function
        | _,Case (case_e, body_e) ->
          let pos,b = body_e in
          let has_break,b = remove_break b in
           (aux_e env_sw case_e, aux_e env_sw (pos,b), has_break)
        | _,Default body_e ->
          let pos,b = body_e in
          let has_break,b = remove_break b in
          (fresh_e env_sw Ast.Noop, aux_e env_sw (pos, b), has_break)
        | _ -> failwith "Invalid case in switch"
      in
      let switch = fresh_e env (Ast.Switch (aux_e env e, List.map aux_cases cases)) in
      List.fold_right (fun v acc -> fresh_e env (Ast.Declare (v, acc))) (List.rev declared_vs) switch
      |> (fun (_, _, _, e) -> e)
  | Case _ | Default _ -> failwith "Case and Default should be inside a switch"
  | Seq [] -> Ast.Const Ast.CNull
  | Seq (e::es) ->
      let seq_e =
        List.fold_left
          (fun acc e2 -> fresh_e env (Ast.Seq (acc, aux_e env e2)))
          (aux_e env e)
          es
      in
      let _, _, _, e = seq_e in
      e
  | Block stmts ->
      (* Compound statement [{ ... }]: introduce a fresh scope frame, then
         walk [stmts] left-to-right, emitting [Ast.Declare] at each
         [VarDeclare] so the binding scopes only to the rest of the block. *)
      let env' = push_scope env in
      let block_e = aux_block env' stmts in
      let _, _, _, e = block_e in
      e
  | Comma (e1, e2) -> Ast.Seq (aux_e env e1, aux_e env e2)
  | Cast (ty, e) -> Ast.Cast (resolve_ctype env.decl ty, aux_e env e)
  | CoerceNarrow (ty, e) -> Ast.CoerceNarrow (resolve_ctype env.decl ty, aux_e env e)
  | VarDeclare (typ, (_, Id s)) ->
      (* Reached when a [VarDeclare] sits inside a [Seq] processed by [aux_e]
         directly — most commonly the for-init [Seq [VarDeclare; VarAssign]].
         The enclosing arm ([For] for for-init, [aux_block] for block items)
         is responsible for first registering [s] in the env's top frame.
         Here we just emit the struct-zeroing initializer (if any) using
         [var env s], which reuses the pre-registered [Variable.t]. *)
      let typ = resolve_ctype env.decl typ in
      if has_struct_type typ then
        let null_e = fresh_e env (Ast.Const Ast.CNull) in
        Ast.VarAssign (var env s, fresh_e env (Ast.Cast (typ, null_e)))
      else
        Ast.Noop
  | VarDeclare (_, _) -> failwith "Declaration must have an identifier" (*Should be unreachable*)
  in
    mk_e env eid e
(* An assignment, as an expression: C gives it the value assigned, which
   conditions use ([if ((err = f(x)))], [while ((n = read(...)) > 0)]). For a
   variable, the value is the variable read back (after the [void* -> T*]
   narrowing); otherwise, the right-hand side, evaluated once into a
   temporary. *)
and aux_assign_value env pos lhs e2 =
  match lhs with
  | (_, Id s) ->
      Ast.Seq (fresh_e env (aux_assign env pos lhs e2), fresh_e env (Ast.Id (var env s)))
  | _ ->
      let env', v, v_name = fresh_tmp env in
      Ast.Let (v, aux_e env e2,
        fresh_e env' (Ast.Seq (fresh_e env' (aux_assign env' pos lhs (pos, Id v_name)),
                               fresh_e env' (Ast.Id v))))
(* The assignment [lhs = e2] itself, as a statement. *)
and aux_assign env pos lhs e2 =
  let e = VarAssign (lhs, e2) in
  match e with
  | VarAssign ((_,Id s), e2) ->
      (* C's implicit [void* -> T*] conversion, as at an initialization (see
         the parser's [CoerceNarrow]): assigned to a variable declared [T*],
         [malloc]'s [c_ptr] is narrowed to [T*]. *)
      let v = var env s in
      let e2 = aux_e env e2 in
      let na_cell =
        match MVariable.kind v with
        | MVariable.AnnotMut g -> not (Ty.leq (GTy.ub g) Rstt.Cint.any)
        | _ -> false
      in
      (match Ast.VarMap.find_opt v env.ctypes, e2 with
       | Some (Ast.Int as ty), _ when na_cell ->
           (* An annotated [c_int_na] cell: C converts the value to [int]. *)
           Ast.VarAssign (v, fresh_e env (Ast.Cast (ty, e2)))
       | _, (_, _, _, Ast.Const _) -> Ast.VarAssign (v, e2) (* already exact *)
       | Some (Ast.Ptr t as ty), _ when t <> Ast.Void ->
           Ast.VarAssign (v, fresh_e env (Ast.CoerceNarrow (ty, e2)))
       | _, _ -> Ast.VarAssign (v, e2))
  | VarAssign ((loc1, Call ((_,Id "[]"), ((_, Id base_name) :: _ as args))) ,e2) ->
      (* Model arr[i] = v as a functional update arr = []<-(arr, i, v) so
         subsequent reads can use the updated value. *)
      Ast.VarAssign (
        var env base_name,
        fresh_e env (Ast.Call (
          aux_e env (loc1, Id "[]<-"),
          (List.map (aux_e env) args) @ [aux_e env e2]
        ))
      )
  | VarAssign ((loc1, Call ((_,Id "[]"), args)) ,e2) ->
      Ast.Call (
        aux_e env (loc1, Id "[]<-"),
        (List.map (aux_e env) args) @ [aux_e env e2]
      )
  | VarAssign ((_loc1, Unop (_op, _)) as lhs ,e2) ->
      (* For [*p = e2] (and [&p = e2]), lower to [let tmp = *p in tmp := e2].
         The read forces [p] to be a pointer to a type carrying e2's value;
         the assignment lands on a fresh local so [p] itself is not rebound.
         Imprecise (the new pointee value is not tracked at later reads),
         but type-safe. *)
      let read_e = aux_e env lhs in
      let tmp = MVariable.create MVariable.Mut (Some "_deref") in
      let assign_e = fresh_e env (Ast.VarAssign (tmp, aux_e env e2)) in
      Ast.Let (tmp, read_e, assign_e)
  | VarAssign ((_, FieldAccess (((_, Id s) as e1), field)) ,e2)
    when is_block_local env s ->
      (* Model [b.field = v] on a local struct variable as a functional update
         rebinding the variable: [b = b with field = v]. Mirrors the [arr[i] =
         v] case above. Without the rebind the [FieldUpdate] produces a fresh
         record that is discarded, so later reads of [b] (and the field-by-field
         lowering of a designated initializer [struct T b = { .field = v }]) see
         the pre-update record and the field-read fails as an untypeable
         projection.

         Restricted to block-local variables ([is_block_local], i.e. declared
         in a scope frame above the base frame). A file-scope global struct
         ([struct syms syms; ... syms.f = v]) lives in the base frame; rebinding
         it would replace its full declared record with the narrow record
         accumulated from one function's writes, breaking later width-subtyping
         uses (e.g. vctrs' [globals.c] / [vctrs_init_*]). Globals fall through to
         the non-rebinding general arm below and keep their declared type.
         Pointer writes [p->field = v] also fall through — they arrive with a
         dereference ([Unop "*"]) as the base. *)
      Ast.VarAssign (var env s,
        fresh_e env (Ast.FieldUpdate (aux_e env e1, field, aux_e env e2)))
  | VarAssign ((_, FieldAccess (e1, field)) ,e2) ->
      Ast.FieldUpdate (aux_e env e1, field, aux_e env e2)
  | VarAssign (_,_) -> failwith ("Unexpected left-hand side in assignment. Got: " ^ show_e (pos,e))
  | _ -> assert false
and aux_block env stmts =
  (* Translate the statements of a [Block] in their scope frame. Each
     [VarDeclare] extends the topmost frame for the rest of the block and
     produces an [Ast.Declare] whose body is the remaining translated
     statements, so the lexical lifetime of every local matches its C scope.
     A nested [Seq] (only emitted for multi-declarator declarations like
     [int a, b;]) is flattened into its parent block: its declarators land
     in the same scope as their siblings. *)
  let rec loop env = function
    | [] -> fresh_e env (Ast.Const Ast.CNull)
    | [stmt] -> aux_block_stmt env stmt
    | stmt :: rest ->
        (match stmt with
         | _, VarDeclare (typ, (_, Id s)) ->
             let env', v = add_var_top ~ty:typ env s in
             let typ = resolve_ctype env.decl typ in
             let init_e =
               if has_struct_type typ then
                 fresh_e env (Ast.VarAssign (v,
                   fresh_e env (Ast.Cast (typ, fresh_e env (Ast.Const Ast.CNull)))))
               else
                 fresh_e env Ast.Noop
             in
             let rest =
               match typ with
               | Ast.Struct (_, fields) -> adjust_struct_init s fields rest
               | _ -> rest
             in
             let rest_e = loop env' rest in
             fresh_e env (Ast.Declare (v, fresh_e env (Ast.Seq (init_e, rest_e))))
         | _, VarDeclare (_, _) ->
             failwith "Declaration must have an identifier"
         | _, Seq inner ->
             (* Multi-declarator [int a, b;] from parser.ml emits a [Seq] of
                [VarDeclare]s.  Splice it into the surrounding block so all
                declarators share its scope. *)
             loop env (inner @ rest)
         | _ ->
             let stmt_e = aux_e env stmt in
             let rest_e = loop env rest in
             fresh_e env (Ast.Seq (stmt_e, rest_e)))
  in
  loop env stmts
and aux_block_stmt env stmt =
  (* Tail position: a single declaration as the last block statement has no
     subsequent uses, so we just emit its initializer (if any) and return. *)
  match stmt with
  | _, VarDeclare (typ, (_, Id s)) ->
      let v = MVariable.create MVariable.Mut (Some s) in
      let typ = resolve_ctype env.decl typ in
      let init_e =
        if has_struct_type typ then
          fresh_e env (Ast.VarAssign (v,
            fresh_e env (Ast.Cast (typ, fresh_e env (Ast.Const Ast.CNull)))))
        else
          fresh_e env Ast.Noop
      in
      fresh_e env (Ast.Declare (v, init_e))
  | _, VarDeclare (_, _) ->
      failwith "Declaration must have an identifier"
  | _, Seq inner ->
      (* Trailing multi-declarator: treat as a sub-block of declarators. *)
      aux_block env inner
  | _ ->
      aux_e env stmt
and process_call env f args =
  (* Set calls modify in place in the R C API, but for typing reason,
  we make it create a new value and then assign to the original variable. *)
  (* The R C API "set" functions mutate their first argument in place. We model
     the mutation as [v = f(v, ...)] (a VarAssign that refines v's type, so a
     later use of v -- e.g. returning the parameter -- sees the new
     attribute/element). The whole expression must then yield what the function
     actually *returns* in R, so value-position uses ([return f(...)],
     [y = f(...)]) stay faithful:
       - setAttrib / Rf_setAttrib return the object (vec, the 1st arg) -> yield v
       - SET_VECTOR_ELT returns the element (3rd arg)                  -> yield it
       - SET_STRING_ELT returns void                                  -> yield unit
         (a bare VarAssign yields unit, so no trailing read). *)
  let set_call loc1 = mk_e env (Eid.unique_with_pos loc1)
      (Ast.Call (aux_e env f, List.map (aux_e env) args)) in
  let e = match (f, args) with
  | (loc1,Id ("setAttrib" | "Rf_setAttrib")),(_, Id v)::_ ->
   let vv = var env v in
   Ast.Seq (fresh_e env (Ast.VarAssign (vv, set_call loc1)), fresh_e env (Ast.Id vv))
  | (loc1,Id "SET_VECTOR_ELT"),((_, Id v)::_::elt::_) ->
   let vv = var env v in
   Ast.Seq (fresh_e env (Ast.VarAssign (vv, set_call loc1)), aux_e env elt)
  | (loc1,Id "SET_STRING_ELT"),(_, Id v)::_ ->
   Ast.VarAssign (var env v, set_call loc1)
  | (_, Id "sizeof"), [(arg_pos, Id name)]
    when Ast.DeclMap.mem name env.decl
      && Option.is_none (lookup_binding env name) ->
      (* Tree-sitter has no symbol table, so it parses [sizeof(AP)] as
         [sizeof(EXPR(AP))] even when AP is a typedef-name. Rewrite to
         [sizeof((AP)0)] — the shape the parser builds for the parens-around-
         type form — so the typedef flows through resolve_ctype instead of
         being looked up as a value and reported as "unbound variable: AP". *)
      let zero = (arg_pos, Const (CInt 0)) in
      let cast = (arg_pos, Cast (Ast.Typeref name, zero)) in
      Ast.Call (aux_e env f, [aux_e env cast])
  | (_, Id fname), first :: rest
    when StrSet.mem fname sexptype_first_arg_callees ->
      (* The first argument of these allocator/coerce functions is a SEXPTYPE
         tag. Source code often passes the numeric value directly (a literal
         or an enum constant like vctrs's [R_TYPE_integer = 13]), which the
         type system sees as a singleton integer ([c(13)]) and rejects
         against the [prim] domain of [allocVector]'s signature. Rewrite
         that first arg to the corresponding [INTSXP]/[REALSXP]/... [Id]
         when we recognise the integer; leave it alone otherwise so the
         downstream type error still surfaces. *)
      let arg_as_int = function
        | (_, Const (CInt n)) -> Some n
        | (_, Id name) -> !enum_int_lookup name
        | _ -> None
      in
      let first =
        match arg_as_int first with
        | None -> first
        | Some n ->
            (match Defs.sexptype_name_of_int n with
             | None -> first
             | Some sxname -> (fst first, Id sxname))
      in
      Ast.Call (aux_e env f, List.map (aux_e env) (first :: rest))
  | (_, Id fname), _ -> call_variadic_aware env (Name.Ident fname) (aux_e env f) args
  | _ -> Ast.Call (aux_e env f, List.map (aux_e env) args) in
  e
(* A call of the definition bound as [key], whose callee is already
   translated. [args] beyond the named parameters of a variadic definition are
   evaluated before the call but not passed: the definition is typed from its
   named parameters only (see [variadic_arity]). *)
and call_variadic_aware env key callee args =
  match Hashtbl.find_opt variadic_arity key with
  | Some n when List.length args > n ->
      let passed = List.filteri (fun i _ -> i < n) args in
      let extra = List.filteri (fun i _ -> i >= n) args in
      let call = fresh_e env (Ast.Call (callee, List.map (aux_e env) passed)) in
      (match List.fold_right (fun a acc -> fresh_e env (Ast.Seq (aux_e env a, acc))) extra call with
       | (_, _, _, e) -> e)
  | _ -> Ast.Call (callee, List.map (aux_e env) args)
(* [self]: for a recursive function typed as an MLsem [LambdaRec] (see
   [Runner.infer_def]), the variable its recursive calls refer to. *)
and transform ?self env (pos, topl_unit) = 
  let eid = Eid.unique_with_pos pos in
  let (decl, e) = match topl_unit with 
  | Fundef (ret_ty, name, params, body) ->
    translate_def ?self env (Name.Ident name) ret_ty name params body
  | MacroDef (name, params, body) ->
    translate_def env (Name.Macro name) Ast.Any name params body
  | TypeDecl (name, ty) -> (Ast.DeclMap.add name ty env.decl, Ast.Noop)
  | GlobalVar _ -> (env.decl, Ast.Noop) (* handled by infer_def directly *)
  | Define (_name, _value) -> (env.decl, Ast.Noop)
  | Include _ -> (env.decl, Ast.Noop) (* handled by infer_def directly *)
  in
  (eid, decl, Ast.VarMap.empty, e)
(* A function or macro definition, recorded under [key] in [variadic_arity]
   when variadic. In the body of a recursive function typed as a [LambdaRec],
   its own name is [self]. *)
and translate_def ?self env key ret_ty name params body =
    let env =
      match self with
      | None -> env
      | Some v -> { env with id = NameMap.singleton (Name.Ident name) v :: env.id }
    in
    let env = { env with cells = annotated_cells params body } in
    (* Bind every parameter into the topmost scope frame. The body is emitted
       by the parser as a [Block] for actual definitions and as [Seq []] for
       forward declarations; treat [Block] specially so the function body's
       outer node carries the inner statement's eid (matching the pre-scope
       translator) rather than a synthetic eid from the [{}] span. The block
       still introduces its own scope frame so locals declared inside cannot
       leak out. *)
    let env =
      List.fold_left
        (fun env -> function
          | Param (ty, name) -> fst (add_var_top ~ty env name)
          | Vararg -> env)
        env
        params
    in
    (* C converts an argument to a [bool] parameter: any scalar is accepted,
       and the callee sees 0 or 1. So a [bool] parameter is typed as an [int]
       and converted, [b = (b != 0)], on entry. Typed as [c_bool] instead, it
       made every caller passing an ordinary [int] fail. *)
    let is_bool ty = resolve_ctype env.decl ty = Ast.Bool in
    let to_bool =
      List.filter_map (function
          | Param (ty, n) when is_bool ty ->
            let p = fst body in
            Some (p, VarAssign ((p, Id n), (p, Binop ("!=", ((p, Id n), (p, Const (CInt 0)))))))
          | _ -> None) params
    in
    let e = match body with
      | _, Block stmts -> aux_block (push_scope env) (to_bool @ stmts)
      | _ -> aux_e env body
    in
    if List.mem Vararg params then
      Hashtbl.replace variadic_arity key
        (List.length (List.filter (function Param _ -> true | Vararg -> false) params));
    let params =
      List.filter_map (function
        | Param (ty, name) when is_bool ty -> Some (Ast.Int, var env name)
        | Param (ty, name) -> Some (resolve_ctype env.decl ty, var env name)
        | Vararg -> None) params
    in
    let ret_ty = resolve_ctype env.decl ret_ty in
    (env.decl, Ast.Function (name, ret_ty, params, e, self))

(** [f] applied to the immediate subexpressions of an expression. *)
let map_children f (pos, e) =
  let e = match e with
    | Const _ | Id _ | Break | Next | Goto _ -> e
    | Unop (op, e1) -> Unop (op, f e1)
    | Binop (op, (e1, e2)) -> Binop (op, (f e1, f e2))
    | VarDeclare (ty, e1) -> VarDeclare (ty, f e1)
    | VarAssign (e1, e2) -> VarAssign (f e1, f e2)
    | FieldAccess (e, field) -> FieldAccess (f e, field)
    | Call (g, args) -> Call (f g, List.map f args)
    | MacroCall (name, args) -> MacroCall (name, List.map f args)
    | If (cond, then_, else_) -> If (f cond, f then_, Option.map f else_)
    | Ite (cond, then_, else_) -> Ite (f cond, f then_, f else_)
    | While (cond, body) -> While (f cond, f body)
    | For (init, cond, incr, body) ->
        For (f init, Option.map f cond, Option.map f incr, f body)
    | Return e -> Return (Option.map f e)
    | Case (e1, e2) -> Case (f e1, f e2)
    | Default e1 -> Default (f e1)
    | Switch (e1, cases) -> Switch (f e1, List.map f cases)
    | Seq exprs -> Seq (List.map f exprs)
    | Block exprs -> Block (List.map f exprs)
    | Comma (e1, e2) -> Comma (f e1, f e2)
    | Cast (ty, e) -> Cast (ty, f e)
    | CoerceNarrow (ty, e) -> CoerceNarrow (ty, f e)
  in
  (pos, e)

(** Bottom-up: [f] is applied to a node after its subexpressions. *)
let map f e =
  let rec aux e = f (map_children aux e) in
  aux e

(** Extract function call names from an expression.
    [fun_names] is the set of known function names; bare Id references
    matching a function name in call arguments are treated as function
    pointer dependencies. *)
let rec extract_calls_from_expr ?(fun_names=StrSet.empty) (_pos, e') =
  let extract = extract_calls_from_expr ~fun_names in
  let rec extract_arg (_pos, e') =
    match e' with
    | Id name when StrSet.mem name fun_names -> [name]
    | Cast (_, inner) -> extract_arg inner
    | CoerceNarrow (_, inner) -> extract_arg inner
    (* [&fn] is the canonical "function as value" syntax (callback
       registration like [r_attrib_map(x, &r_attrib_get_cb, &tag)]). Peel the
       address-of so the bare [Id] inside reaches the [fun_names] check —
       otherwise the static [fn] is unreachable from any entry point and gets
       pruned by [Call_graph.keep_reachable], leading to spurious "unbound
       variable" when [fn]'s caller is typed. *)
    | Unop ("&", inner) -> extract_arg inner
    | _ -> extract (_pos, e')
  in
  match e' with
  (* A bare function name used as a *value* (not as a call target) is a real
     reference — e.g. a callback stored in a struct field
     [g->print = &yajl_buf_print] or [yaf->malloc = yajl_internal_malloc]. The
     [extract_arg] handling above only fires for call arguments, so without
     this the static [fn] is unreachable from any entry point, gets pruned by
     [Call_graph.keep_reachable], and its caller reports a spurious "unbound
     variable". The [fun_names] membership check keeps this to genuine
     function references ([&fn] reaches the [Id] via the [Unop] case below). *)
  | Id name when StrSet.mem name fun_names -> [name]
  | Const _ | Id _ | Break | Next | Goto _ -> []
  | Unop (_, e) -> extract e
  | Binop (_, (e1, e2)) -> extract e1 @ extract e2
  | FieldAccess (e, _) -> extract e
  | VarDeclare (_, e) -> extract e
  | VarAssign (e1, e2) -> extract e1 @ extract e2
  | Call ((_, Id fname), args) | MacroCall (fname, args) ->
      fname :: List.concat_map extract_arg args
  | Call (f, args) ->
      extract f @ List.concat_map extract_arg args
  | If (cond, then_, else_opt) ->
      let acc = extract cond @ extract then_ in
      (match else_opt with
       | None -> acc
       | Some e -> acc @ extract e)
  | Ite (cond, then_, else_) ->
      extract cond @ extract then_ @ extract else_
  | While (cond, body) -> extract cond @ extract body
  | For (init, cond_opt, incr_opt, body) ->
      let acc = extract init in
      let acc = match cond_opt with None -> acc | Some e -> acc @ extract e in
      let acc = match incr_opt with None -> acc | Some e -> acc @ extract e in
      acc @ extract body
  | Return e_opt ->
      (match e_opt with None -> [] | Some e -> extract e)
  | Case (e1, e2) -> extract e1 @ extract e2
  | Default e -> extract e
  | Switch (e, cases) -> extract e @ List.concat_map extract cases
  | Seq exprs -> List.concat_map extract exprs
  | Block exprs -> List.concat_map extract exprs
  | Comma (e1, e2) -> extract e1 @ extract e2
  | Cast (_, e) -> extract e
  | CoerceNarrow (_, e) -> extract e

(* Inline tests for extract_calls_from_expr *)
let%test "simple function call" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let expr = mk_e (Call (mk_e (Id "foo"), [])) in
  let calls = extract_calls_from_expr expr in
  calls = ["foo"]

let%test "function call with arguments" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let expr = mk_e (Call (mk_e (Id "bar"), [mk_e (Const (CInt 42)); mk_e (Id "x")])) in
  let calls = extract_calls_from_expr expr in
  calls = ["bar"]

let%test "function pointer passed as argument" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let fun_names = StrSet.of_list ["R_ToplevelExec"; "check_interrupt_fn"] in
  let expr = mk_e (Call (mk_e (Id "R_ToplevelExec"),
    [mk_e (Id "check_interrupt_fn"); mk_e (Const CNull)])) in
  let calls = extract_calls_from_expr ~fun_names expr in
  calls = ["R_ToplevelExec"; "check_interrupt_fn"]

let%test "cast function pointer passed as argument" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let fun_names = StrSet.of_list ["set_user_option"; "R_curl_callback_ssl_ctx"] in
  let expr = mk_e (Call (mk_e (Id "set_user_option"),
    [mk_e (Id "CURLOPT_SSL_CTX_FUNCTION");
     mk_e (Cast (Ast.Typeref "curl_ssl_ctx_callback",
                 mk_e (Id "R_curl_callback_ssl_ctx")))])) in
  let calls = extract_calls_from_expr ~fun_names expr in
  calls = ["set_user_option"; "R_curl_callback_ssl_ctx"]

let%test "non-function id argument not extracted" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let fun_names = StrSet.of_list ["bar"] in
  let expr = mk_e (Call (mk_e (Id "bar"), [mk_e (Const (CInt 42)); mk_e (Id "x")])) in
  let calls = extract_calls_from_expr ~fun_names expr in
  calls = ["bar"]

let%test "nested function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let inner_call = mk_e (Call (mk_e (Id "inner"), [])) in
  let expr = mk_e (Call (mk_e (Id "outer"), [inner_call])) in
  let calls = extract_calls_from_expr expr in
  calls = ["outer"; "inner"]

let%test "binary operation with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call1 = mk_e (Call (mk_e (Id "func1"), [])) in
  let call2 = mk_e (Call (mk_e (Id "func2"), [])) in
  let expr = mk_e (Binop ("+", (call1, call2))) in
  let calls = extract_calls_from_expr expr in
  calls = ["func1"; "func2"]

let%test "if-then-else with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let cond = mk_e (Call (mk_e (Id "is_ready"), [])) in
  let then_branch = mk_e (Call (mk_e (Id "process"), [])) in
  let else_branch = mk_e (Call (mk_e (Id "skip"), [])) in
  let expr = mk_e (If (cond, then_branch, Some else_branch)) in
  let calls = extract_calls_from_expr expr in
  calls = ["is_ready"; "process"; "skip"]

let%test "while loop with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let cond = mk_e (Call (mk_e (Id "has_next"), [])) in
  let body = mk_e (Call (mk_e (Id "process_next"), [])) in
  let expr = mk_e (While (cond, body)) in
  let calls = extract_calls_from_expr expr in
  calls = ["has_next"; "process_next"]

let%test "for loop with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let init = mk_e (Call (mk_e (Id "init_loop"), [])) in
  let cond = mk_e (Call (mk_e (Id "check_cond"), [])) in
  let incr = mk_e (Call (mk_e (Id "increment"), [])) in
  let body = mk_e (Call (mk_e (Id "process"), [])) in
  let expr = mk_e (For (init, Some cond, Some incr, body)) in
  let calls = extract_calls_from_expr expr in
  calls = ["init_loop"; "check_cond"; "increment"; "process"]

let%test "switch statement with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let switch_expr = mk_e (Call (mk_e (Id "get_value"), [])) in
  let case1 = mk_e (Case (mk_e (Const (CInt 1)), mk_e (Call (mk_e (Id "handle_one"), [])))) in
  let default = mk_e (Default (mk_e (Call (mk_e (Id "handle_default"), [])))) in
  let expr = mk_e (Switch (switch_expr, [case1; default])) in
  let calls = extract_calls_from_expr expr in
  calls = ["get_value"; "handle_one"; "handle_default"]

let%test "sequence with multiple function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call1 = mk_e (Call (mk_e (Id "step1"), [])) in
  let call2 = mk_e (Call (mk_e (Id "step2"), [])) in
  let call3 = mk_e (Call (mk_e (Id "step3"), [])) in
  let expr = mk_e (Seq [call1; call2; call3]) in
  let calls = extract_calls_from_expr expr in
  calls = ["step1"; "step2"; "step3"]

let%test "return with function call" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call = mk_e (Call (mk_e (Id "compute"), [])) in
  let expr = mk_e (Return (Some call)) in
  let calls = extract_calls_from_expr expr in
  calls = ["compute"]

let%test "no function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let expr = mk_e (Binop ("+", (mk_e (Const (CInt 1)), mk_e (Const (CInt 2))))) in
  let calls = extract_calls_from_expr expr in
  calls = []

let%test "assignment with function call" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call = mk_e (Call (mk_e (Id "getValue"), [])) in
  let expr = mk_e (VarAssign (mk_e (Id "x"), call)) in
  let calls = extract_calls_from_expr expr in
  calls = ["getValue"]
