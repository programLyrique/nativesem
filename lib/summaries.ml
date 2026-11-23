(* What a function or a function-like macro of the package may do to the
   attributes of the R values it is given: one summary per definition, with,
   for each parameter, how far its value's classes and attributes may change
   during a call (and after it, when the value escapes), and whether the
   result may be that value itself.

   The analysis is syntactic, on the parsed [PAst], before lowering, and
   flow-insensitive: a local variable that is ever assigned (a copy of) a
   parameter's value counts as that value everywhere in the body. That only
   over-approximates. Unknown code is assumed to do anything: a function
   without a body, or neither the package's nor of types/base.ty, a function
   pointer, an argument beyond the parameters.

   It follows the escape rules of the lowering (see [PAst]): the same
   protection, storing and copying functions, and the functions of
   types/base.ty only read their arguments, but for the setters. *)

open PAst

(* Ordered: each level includes the ones before. *)
type level =
  | Untouched  (** neither the classes nor the attributes change *)
  | Attrs      (** attributes other than the class may change *)
  | Class      (** the class may change too (or any attribute) *)
  | Escapes    (** may change at any later time: stored, or given to unknown code *)

let join (a : level) (b : level) = if compare a b >= 0 then a else b

let string_of_level = function
  | Untouched -> "none" | Attrs -> "attrs" | Class -> "class" | Escapes -> "escapes"

type summary = {
  params : string list;
  r_params : bool list;
    (** Whether each parameter can hold an R value: not when its C type is a
        number, a character, an enum, a struct other than [SEXPREC], a pointer
        to one of these, or a function pointer. The others' levels are computed all the same (a [void*] may
        be a [SEXP]), but only matter for an R value. *)
  levels : level list;      (** one per parameter, in order *)
  returned : bool list;     (** whether the result may be that parameter's value *)
  free : (string * level) list;
    (** For a macro, the names its body uses without binding them (variables
        of the code it is expanded in), with what it may do to their
        values. Empty for a function: its free names are globals. *)
}

type kind = Fun | Macro

module Key = struct
  type t = kind * string
  let compare = compare
end
module KeyMap = Map.Make (Key)

type t = summary KeyMap.t

let find (t : t) kind name = KeyMap.find_opt (kind, name) t

(* ===== What the C API's functions do ===== *)

(* The level of the change of attributes that the attribute [sym] denotes,
   as [Ast.attr_target_of_symarg] reads it (unknown symbols count as the
   class); also rlang's [r_syms.<field>]. *)
let level_of_symbol (_, e) =
  let of_name = function
    | "class" -> Class
    | _ -> Attrs
  in
  match e with
  | Id ("R_NamesSymbol" | "R_DimSymbol" | "R_DimNamesSymbol" | "R_LevelsSymbol"
       | "R_RowNamesSymbol") -> Attrs
  | Call ((_, Id ("install" | "Rf_install")), [ (_, Const (CStr s)) ]) -> of_name s
  | FieldAccess ((_, Id "r_syms"), field) ->
    (match field with
     | "names" | "dim" | "dim_names" | "levels" | "row_names" -> Attrs
     | _ -> Class)
  | _ -> Class

(* The setters of types/base.ty: the argument whose attributes change, and
   how (given the arguments). *)
let setter_target name args =
  let nth i = List.nth_opt args i in
  match name with
  | "setAttrib" | "Rf_setAttrib" ->
    (match nth 1 with Some sym -> Some (0, level_of_symbol sym) | None -> Some (0, Class))
  | "classgets" | "Rf_classgets" | "SET_ATTRIB" | "SET_OBJECT" | "CLEAR_ATTRIB"
  | "DUPLICATE_ATTRIB" | "SHALLOW_DUPLICATE_ATTRIB" -> Some (0, Class)
  | "namesgets" | "Rf_namesgets" | "dimgets" | "Rf_dimgets"
  | "dimnamesgets" | "Rf_dimnamesgets" -> Some (0, Attrs)
  | "copyMostAttrib" | "Rf_copyMostAttrib" -> Some (1, Class)
  | _ -> None

(* The functions of the C API that may return their first argument itself:
   the protection functions, the setters that return their target, and the
   conversions that leave an argument already of the right type alone. *)
let returning_callees =
  StrSet.union protection_callees
    (StrSet.of_list
       (with_rf [ "classgets"; "namesgets"; "dimgets"; "dimnamesgets";
                  "coerceVector"; "lengthgets"; "xlengthgets" ]))

(* What a callee is, for the analysis. *)
type callee =
  | Summarized of summary
  | Api of string  (** a function of the C API (types/base.ty) or a C operator *)
  | Unknown

(* The integer types of the C library and of R's headers, which the parser
   does not always see the definition of. *)
let integer_typedefs =
  StrSet.of_list
    ([ "size_t"; "ssize_t"; "ptrdiff_t"; "intptr_t"; "uintptr_t"; "off_t";
       "R_xlen_t"; "R_len_t"; "Rbyte"; "Rboolean"; "R_size_t" ]
     @ List.concat_map (fun n -> [ "int" ^ n ^ "_t"; "uint" ^ n ^ "_t" ]) [ "8"; "16"; "32"; "64" ])

(* Whether a parameter of C type [ty] can hold an R value, with the
   typedefs [typedefs] (a typedef not found may be one of [SEXP]). *)
let may_be_r_value typedefs ty =
  let rec aux seen (ty : Ast.ctype) =
    match ty with
    | Int | Float | Char | Bool | Enum _ | FunPtr _ | NoReturn | Void -> false
    | Ptr Void -> true
    | Ptr t | Array (t, _) -> aux seen t
    | Struct (name, _) -> name = "SEXPREC"
    | Union _ -> false
    | Typeref name when StrSet.mem name integer_typedefs -> false
    | Typeref name when not (StrSet.mem name seen) ->
      (match StrMap.find_opt name typedefs with
       | Some ty -> aux (StrSet.add name seen) ty
       | None -> true)
    | SEXP | Any | Typeref _ -> true
  in
  aux StrSet.empty ty

let r_params typedefs params =
  List.filter_map (function Param (ty, _) -> Some (may_be_r_value typedefs ty) | Vararg -> None)
    params

(* ===== The analysis of one definition ===== *)

(* What a value may be: some of the definition's parameters (by position),
   or, in a macro, some free names. *)
type target = P of int | F of string

module TSet = Set.Make (struct type t = target let compare = compare end)

let analyse ~resolve ~is_api ~typedefs ~is_macro params body =
  let param_names =
    List.filter_map (function Param (_, n) -> Some n | Vararg -> None) params in
  let index_of = List.mapi (fun i n -> (n, i)) param_names in
  let locals = local_names params body in
  (* The targets each name may denote: a parameter itself, a local through
     copies (computed below), a free name of a macro itself. *)
  let aliases = Hashtbl.create 16 in
  List.iter (fun (n, i) -> Hashtbl.replace aliases n (TSet.singleton (P i))) index_of;
  let lookup s =
    match Hashtbl.find_opt aliases s with
    | Some ts -> ts
    | None ->
      if is_macro && not (StrSet.mem s locals) && not (is_api s) then TSet.singleton (F s)
      else TSet.empty
  in
  (* The targets the value of [e] may be. *)
  let rec denotes (_, e') =
    match e' with
    | Id s -> lookup s
    | Cast (_, x) | CoerceNarrow (_, x) | Comma (_, x) | VarAssign (_, x) -> denotes x
    | Ite (_, a, b) -> TSet.union (denotes a) (denotes b)
    | Call ((_, Id f), args) -> let c, f = resolve Fun f in returned_by c f args
    | MacroCall (m, args) -> let c, m = resolve Macro m in returned_by c m args
    | _ -> TSet.empty
  and returned_by callee name args =
    match callee with
    | Summarized s ->
      (* (A macro returning one of its free names is not followed.) *)
      List.mapi (fun i a -> (i, a)) args
      |> List.fold_left (fun acc (i, a) ->
          if Option.value ~default:false (List.nth_opt s.returned i)
          then TSet.union acc (denotes a) else acc) TSet.empty
    | Api _ when StrSet.mem name returning_callees ->
      (match args with a :: _ -> denotes a | [] -> TSet.empty)
    | Api _ -> TSet.empty
    | Unknown -> TSet.empty
  in
  (* The copies into locals, to a fixpoint. *)
  let rec assignments acc ((_, e') as e) =
    let acc = match e' with
      | VarAssign ((_, Id s), rhs) when StrSet.mem s locals ->
        (s, rhs) :: acc
      | _ -> acc
    in
    List.fold_left assignments acc (children e)
  in
  let copies = assignments [] body in
  let rec fix () =
    let changed = ref false in
    List.iter (fun (s, rhs) ->
        let old = Option.value ~default:TSet.empty (Hashtbl.find_opt aliases s) in
        let nw = TSet.union old (denotes rhs) in
        if not (TSet.equal old nw) then (Hashtbl.replace aliases s nw ; changed := true))
      copies;
    if !changed then fix ()
  in
  fix ();
  (* The effects. *)
  let effects = Hashtbl.create 16 in
  let returned = ref TSet.empty in
  let touch level ts =
    TSet.iter (fun t ->
        let old = Option.value ~default:Untouched (Hashtbl.find_opt effects t) in
        Hashtbl.replace effects t (join old level)) ts
  in
  let call callee name args =
    List.iteri (fun i a ->
        let level =
          match callee with
          | Summarized s ->
            (match List.nth_opt s.levels i with Some l -> l | None -> Escapes)
          | Api _ ->
            (match setter_target name args, List.assoc_opt name storing_callees with
             | Some (j, l), _ when j = i -> l
             | _, Some stored when stored i -> Escapes
             | _ -> Untouched)
          | Unknown -> Escapes
        in
        touch level (denotes a)) args;
    (* A macro's effects on the free names of its body. *)
    match callee with
    | Summarized s -> List.iter (fun (n, l) -> touch l (lookup n)) s.free
    | _ -> ()
  in
  let rec walk ((_, e') as e) =
    (match e' with
     | Call ((_, Id f), args) -> let c, f = resolve Fun f in call c f args
     | Call (_, args) -> call Unknown "" args
     | MacroCall (m, args) -> let c, m = resolve Macro m in call c m args
     | VarAssign ((_, Id s), _) when StrSet.mem s locals -> ()
     | VarAssign (_, rhs) -> touch Escapes (denotes rhs)
     | Unop ("&", x) -> touch Escapes (denotes x)
     | Return (Some x) -> returned := TSet.union !returned (denotes x)
     | _ -> ());
    List.iter walk (children e)
  in
  walk body;
  (* A macro's body is an expression: its value is its result. *)
  if is_macro then returned := TSet.union !returned (denotes body);
  let level_of t = Option.value ~default:Untouched (Hashtbl.find_opt effects t) in
  let n = List.length param_names in
  let free =
    Hashtbl.fold (fun t l acc -> match t with F s when l <> Untouched -> (s, l) :: acc | _ -> acc)
      effects []
    |> List.sort compare
  in
  { params = param_names;
    r_params = r_params typedefs params;
    levels = List.init n (fun i -> level_of (P i));
    returned = List.init n (fun i -> TSet.mem (P i) !returned);
    free }

(* ===== The package ===== *)

(* The definitions with a body, the package's and its headers'. *)
let definitions (pasts : (string * top_level_unit list) list) =
  let rec of_unit acc = function
    | _, Fundef (_, _, _, (_, Seq [])) -> acc (* a declaration *)
    | _, Fundef (_, name, params, body) -> ((Fun, name), (params, body)) :: acc
    | _, MacroDef (name, params, body) -> ((Macro, name), (params, body)) :: acc
    | _, Include items -> List.fold_left of_unit acc items
    | _ -> acc
  in
  List.fold_left (fun acc (_, units) -> List.fold_left of_unit acc units) [] pasts
  |> List.rev

let join_summary a b =
  { a with
    levels = List.map2 join a.levels b.levels;
    returned = List.map2 ( || ) a.returned b.returned;
    free =
      List.fold_left (fun acc (n, l) ->
          let old = Option.value ~default:Untouched (List.assoc_opt n acc) in
          (n, join old l) :: List.remove_assoc n acc) a.free b.free
      |> List.sort compare }

(* The summaries of the package's definitions, by iteration from "touches
   nothing" to a fixpoint. Definitions of the same name (static functions of
   different files) share their joined summary; if their numbers of
   parameters differ, a call of that name is unknown code. [is_api] says
   whether a name is bound by types/base.ty. *)
let compute ~is_api pasts : t =
  let defs = definitions pasts in
  let rec typedefs_of acc = function
    | _, TypeDecl (name, ty) -> StrMap.add name ty acc
    | _, Include items -> List.fold_left typedefs_of acc items
    | _ -> acc
  in
  let typedefs =
    List.fold_left (fun acc (_, units) -> List.fold_left typedefs_of acc units) StrMap.empty pasts in
  let arity params = List.length (List.filter (function Param _ -> true | Vararg -> false) params) in
  let arities =
    List.fold_left (fun acc (key, (params, _)) ->
        KeyMap.update key (function
            | None -> Some [ arity params ]
            | Some l -> Some (List.sort_uniq compare (arity params :: l))) acc)
      KeyMap.empty defs
  in
  let conflicting key = match KeyMap.find_opt key arities with Some (_ :: _ :: _) -> true | _ -> false in
  let defs = List.filter (fun (key, _) -> not (conflicting key)) defs in
  let known t kind name =
    KeyMap.mem (kind, name) t || conflicting (kind, name)
    || (kind = Fun && (is_api name || StrSet.mem name protection_callees))
  in
  (* The callee [name] denotes, with its name through the aliases of
     object-like macros ([#define NAME TARGET], when [TARGET] is known, as
     the lowering resolves them). *)
  let resolve t kind name =
    let name =
      if kind = Macro then name else
      let target = resolve_alias name in
      if target <> name && known t Fun target then target else name in
    let callee =
      if conflicting (kind, name) then Unknown else
      match KeyMap.find_opt (kind, name) t with
      | Some s -> Summarized s
      | None ->
        if is_api name || StrSet.mem name protection_callees || name = "[]" || name = "sizeof"
        then Api name
        else Unknown
    in
    (callee, name)
  in
  let step t =
    List.fold_left (fun acc (key, (params, body)) ->
        let s = analyse ~resolve:(resolve t) ~is_api ~typedefs ~is_macro:(fst key = Macro) params body in
        KeyMap.update key (function None -> Some s | Some p -> Some (join_summary p s)) acc)
      KeyMap.empty defs
  in
  let rec fix t =
    let t' = KeyMap.union (fun _ a b -> Some (join_summary a b)) t (step t) in
    if KeyMap.equal ( = ) t t' then t else fix t'
  in
  let bottom params =
    let names = List.filter_map (function Param (_, n) -> Some n | Vararg -> None) params in
    { params = names; r_params = r_params typedefs params; levels = List.map (fun _ -> Untouched) names;
      returned = List.map (fun _ -> false) names; free = [] }
  in
  fix (List.fold_left (fun t (key, (params, _)) -> KeyMap.add key (bottom params) t) KeyMap.empty defs)

let pp_summary fmt s =
  let params =
    List.map2 (fun ((p, rv), l) r ->
        if rv then Printf.sprintf "%s: %s%s" p (string_of_level l) (if r then " (returned)" else "")
        else p ^ ": -")
      (List.combine (List.combine s.params s.r_params) s.levels) s.returned
  in
  Format.fprintf fmt "(%s)" (String.concat ", " params);
  if s.free <> [] then
    Format.fprintf fmt " free: %s"
      (String.concat ", "
         (List.map (fun (n, l) -> Printf.sprintf "%s: %s" n (string_of_level l)) s.free))

(* The summaries of the definitions the predicate accepts, in order. *)
let print ?(visible = fun _ -> true) pasts t =
  let seen = Hashtbl.create 16 in
  List.iter (fun (key, _) ->
      if visible (snd key) && not (Hashtbl.mem seen key) then begin
        Hashtbl.add seen key ();
        match KeyMap.find_opt key t with
        | Some s ->
          Format.printf "%s%s %a@." (if fst key = Macro then "macro " else "") (snd key)
            pp_summary s
        | None -> ()
      end) (definitions pasts)

(* The names types/base.ty binds. *)
let is_api name = Defs.StrMap.mem name (Defs.defs_map ())

(* Parse the C files [filenames], and print the summaries of their
   definitions (not of the headers'). *)
let print_for_files filenames =
  let pasts = List.map (fun f -> (f, Parser.to_ast (Parser.parse_file f))) filenames in
  let t = compute ~is_api pasts in
  let own =
    List.concat_map (fun (_, units) ->
        List.filter_map (function
            | _, Fundef (_, n, _, _) -> Some n
            | _, MacroDef (n, _, _) -> Some n
            | _ -> None) units) pasts
    |> StrSet.of_list
  in
  print ~visible:(fun n -> StrSet.mem n own) pasts t

(* The summaries [t] as the lowering looks them up ([PAst.env.summaries]). *)
let callees (t : t) (name : Name.t) : callee_summary option =
  let key = match name with Name.Ident n -> (Fun, n) | Name.Macro n -> (Macro, n) in
  let widening = function
    | Untouched -> Keep
    | Attrs -> Widen_attrs
    | Class | Escapes -> Widen_class
  in
  Option.map (fun (s : summary) ->
      { widenings = List.map widening s.levels; returns = s.returned;
        free = List.map (fun (n, l) -> (n, widening l)) s.free })
    (KeyMap.find_opt key t)

(* The summaries of the definitions of the C files [pasts], to look up. *)
let of_pasts pasts = callees (compute ~is_api pasts)
