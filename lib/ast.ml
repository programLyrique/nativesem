open Mlsem.Common
open Mlsem.Types
module MVariable = Mlsem.Lang.MVariable
module A = Mlsem.Lang.Ast
module SA = Mlsem.System.Ast

type label = string
[@@deriving show]

type const =
| CChar of char
| CStr of string
| CDbl of float
| CInt of int
| CBool of bool
| CNull
| CNa
| CArray of const list
[@@deriving show]


(* Names to build names for a list with mkNamed look like {"a", "b", ""}*)
let rec extract_names l = 
  match l with 
  | [CStr ""] -> []
  | (CStr s)::s2::l -> s :: (extract_names (s2::l))
  | [CStr _ ] -> failwith "expected \"\" at the end "
  | _ -> failwith "expected string constant"

type ctype =
 | Void
 | Int
 | Float
 | Char
 | Bool
 | Ptr of ctype
 | Array of ctype * int option
 | Struct of string * (ctype * string) list
 | Union of string * (ctype * string) list
 | Enum of string * (string * int option) list
 | Typeref of string (*Refer to another named type, e.g. a named struct or a typedef*)
 (* Function pointer: return type, parameter types, variadic flag.
    Represents the C-level shape `RET (PTR_NAME)(PARAMS)`. It translates to
    an [Arrow] type wrapped with [Attr.mk_content], matching how [infer_cfun]
    types ordinary C functions so that the [pfun] projection accepts the
    value as callable. *)
 | FunPtr of ctype * ctype list * bool
 (* SEXPs *)
 | SEXP
 | Any (* Do we actually need that?*)
 [@@deriving show]

(*level is the number of pointer indirections*)
let rec build_ptr level ty =
  if level <= 0 then ty
  else build_ptr (level - 1) (Ptr ty)

module VarMap = struct
  include Map.Make(Variable)
  let pp(pp_v : Format.formatter -> 'v -> unit)
      (fmt : Format.formatter)
      (m : 'v t) : unit =
    let pp_binding fmt (k, v) =
      Format.fprintf fmt "%a -> %a" Variable.pp k pp_v v
    in
    Format.fprintf fmt "{@[<hov 1>%a@]}"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
        pp_binding)
      (bindings m)
  end

(* For struct declarations, typedefs and so on*)
module DeclMap = struct 
  include Map.Make(String)
  let pp (pp_v : Format.formatter -> 'v -> unit)
      (fmt : Format.formatter)
      (m : 'v t) : unit =
    let pp_binding fmt (k, v) =
      Format.fprintf fmt "%s -> %a" k pp_v v
    in
    Format.fprintf fmt "{@[<hov 1>%a@]}"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
        pp_binding)
      (bindings m)
end

type kind =
  | C of const
  | L of string list (* list of possible labels (VECSXP field names, mkNamed) *)
  | S of string list (* ordered STRSXP contents by index, "" = unset/empty slot
                        (allocVector(STRSXP,n) + SET_STRING_ELT build-up) *)
[@@deriving show]


let add_const m v c = 
  VarMap.add v (C c) m

type e' =
| Const of const
| Id of Variable.t
| Declare of Variable.t * e
| Let of Variable.t * e * e
| VarAssign of Variable.t * e
| FieldRead of e * string 
| FieldUpdate of e * string * e
| Unop of Variable.t * e
| Binop of Variable.t * e * e
| Call of e * e list
| If of e * e * e option
| Ite of e * e * e (* For ternary conditions *)
| While of e * e
| TyCheck of e * Ty.t (* Test between an expression and a constant *)
| Cast of ctype * e (* Type cast expression *)
| CoerceNarrow of ctype * e
  (* Implicit narrowing conversion (C's [void* -> T*] at a typed pointer
     initialization). Lowers to [TypeCast] (intersecting cast: result is
     [target & source]) rather than [Cast]'s [TypeCoerce] (replacement), so it
     only narrows a too-wide initializer (e.g. [malloc]'s [c_ptr]) and never
     widens nor de-refines an already-precise one (e.g. [CHAR]'s non-null
     [c_string('a)]). *)
| Function of string * ctype * (ctype * Variable.t) list * e
| Switch of e * (e * e * bool) list (* expression on which to switch, and then case/default (Noop), their bodies and whether it had a break*)
| Seq of e * e
| Noop (* Hack for the declarations. We should rather use the declarations directly rather than doing the imprecise analysis of used variables in bv_e*)
| Return of e option | Break | Next
[@@deriving show]
and e = Eid.t * (ctype DeclMap.t) * (kind VarMap.t) * e'
[@@deriving show]

type funcs = e list (* list of function definitions *)
[@@deriving show]


let rec typeof_const c = 
  let open Rstt in 
  match c with 
  | CChar _ ->  Cenums.char
  | CStr v -> Cptr.singl_string v
  | CDbl _ -> Cenums.double
  | CInt v -> Cint.singl v
  | CBool v -> if v then Cint.tt else Cint.ff
  | CNull -> Cptr.null
  | CNa -> Cint.na
  | CArray [] -> Cptr.mk_nonstring Defs.any_c
  | CArray (elem::_) -> Cptr.mk_nonstring (typeof_const elem)


module StrMap = Map.Make(String)

(* Cache of recursive struct types keyed by their syntactic form so that
   equivalent self-referential structs reuse the same Sstt encoding instead
   of re-allocating a fresh recursive type (and a fresh alias) at every
   occurrence. Keyed by [show_ctype] to distinguish structs with the same
   name but different field layouts from different translation units. *)
let recursive_struct_cache : (string, Sstt.Ty.t) Hashtbl.t = Hashtbl.create 32

(* [typeof_ctype] translates a [ctype] to an Sstt [Ty.t]. Self-referential
   structs (e.g. [struct node { struct node *next; }]) are encoded using
   Sstt's equirecursive machinery: when a struct is first entered, a fresh
   [Sstt.Var.t] is bound to its name; recursive occurrences re-use that var,
   and the full type is tied together with [Sstt.Ty.of_eqs]. *)
let typeof_ctype ct =
  let open Rstt in
  let rec aux resolving ct =
    match ct with
    | Void -> Cenums.void
    | Int -> Cint.any_na
    | Float -> Cenums.double
    | Char -> Cenums.char
    | Bool -> Cint.bool
    | Ptr Char -> Cptr.string
    | Ptr ty -> Cptr.mk_nonstring (aux resolving ty)
    | SEXP -> Defs.any_sexp
    | Any -> Ty.any
    | Typeref name ->
        (match StrMap.find_opt name resolving with
         | Some v -> Sstt.Ty.mk_var v
         | None -> Ty.any)
    | Struct (name, _) when StrMap.mem name resolving ->
        Sstt.Ty.mk_var (StrMap.find name resolving)
    | Struct (_, []) -> Record.mk_closed []
    | Struct (name, fields) ->
        let key = show_ctype ct in
        (match Hashtbl.find_opt recursive_struct_cache key with
         | Some t -> t
         | None ->
           let v = Sstt.Var.mk name in
           let resolving = StrMap.add name v resolving in
           let body = Record.mk_closed
             (List.map (fun (fty, fname) -> (fname, (aux resolving fty, false))) fields) in
           if Sstt.VarSet.mem v (Sstt.Ty.vars body) then
             (* Self-referential struct: tie the knot with Sstt's equirecursive
                encoding, and register the type under "struct <name>" so the
                printer doesn't unfold the recursion (Rstt's printer can't
                render recursive [defs]). *)
             let t = match Sstt.Ty.of_eqs [(v, body)] with
               | (_, t) :: _ -> t
               | [] -> body
             in
             Hashtbl.add recursive_struct_cache key t ;
             t
           else
             body)
    | Enum (_, enumerators) ->
        if List.for_all (fun (_, value) -> Option.is_some value) enumerators && enumerators <> [] then
          enumerators
          |> List.filter_map (fun (_, value) -> Option.map Cint.singl value)
          |> Ty.disj
        else
          Cint.any_na
    | FunPtr (ret, params, variadic) ->
        (* Mirrors [C_interface.infer_cfun]: variadic functions accept [Ty.any]
           for their argument tuple; fixed-arity ones use the explicit tuple.
           A C function pointer is a bare arrow (no [Attr] wrap) — see
           [Ast.build_call]'s [pfun] projection, which accepts bare arrows. *)
        let ret_ty = aux resolving ret in
        let arg_ty =
          if variadic then Ty.any
          else Tuple.mk (List.map (aux resolving) params)
        in
        Arrow.mk arg_ty ret_ty
    | _ -> failwith ("Type not supported yet in typeof_ctype: " ^ show_ctype ct)
  in
  aux StrMap.empty ct



module AttrProj = struct
  (* A callable C value is a function arrow. With rstt's arrow split, C
     functions and C function pointers are bare arrows (written [-->] in the
     [.ty] files, built without an [Attr] wrap); R closures are arrows wrapped
     in an [Attr] container (written [->]). [build_call] only ever applies C
     callables, but we stay robust to both spellings: a bare arrow is already
     callable, an attr-wrapped one is unwrapped to its content. *)
  let arrow_any = Rstt.(Arrows.any |> Descr.mk_arrows |> Ty.mk_descr)
  let pdom ty = Rstt.Ty.cup ty (Rstt.Attr.mk_content ty)
  let proj ty =
    if Rstt.Ty.leq ty arrow_any then ty
    else try Rstt.Attr.proj_content ty with _ -> ty
end

module AttrConstr = struct
  open Rstt
  let cons classes tys =
    match tys with
    | [ty] -> Attr.mk { content=ty ; classes ; attrs=Lst.any }
    | _ -> assert false
  let cdom classes ty =
    try
      Attr.destruct ty
      |> List.filter_map (fun (ps,_) ->
        let content = ps |> List.map (fun a -> a.Attr.content) |> Ty.conj in
        let ty' = Attr.mk { content ; classes ; attrs=Lst.any } in
        if Ty.leq ty' ty then Some [content] else None
      )
    with Invalid_argument _ -> []
end

let build_call f args =
  (* State how arguments of a function are encoded: here, they are in a tuple*)
   let args = (Eid.unique (), A.Constructor 
      (SA.Tuple (List.length args), args)) in
      (* Handle the attributes *)
      let f = Eid.unique (), A.Projection
      (PCustom { pname="pfun" ; pgen=true ; pdom=AttrProj.pdom ; proj=AttrProj.proj }, f) in
      A.App (f, args)

(* Does variable [v] name the R C API function [name] in either spelling -- the
   bare macro [name] or the real [Rf_name] entry point? Rinternals exposes both
   unless R_NO_REMAP is set, and packages mix them freely (e.g. curl uses
   [Rf_allocVector], others [allocVector]). *)
let is_rfun name v =
  match Variable.get_name v with
  | Some n -> String.equal n name || String.equal n ("Rf_" ^ name)
  | None -> false

(* Resolve the symbol argument of a getAttrib/setAttrib call to the attribute it
   targets. The [R_*Symbol] constants and [install("lit")]/[Rf_install("lit")]
   (string recovered from a literal or the const-prop [vars] map, as
   SET_VECTOR_ELT does for its index) give a statically-known attribute name;
   anything else is dynamic. [Class] has a dedicated model (the [classes] field);
   [Names] is deferred to the list-field-names modeling (mkNamed/SET_VECTOR_ELT),
   so callers only warn for it. *)
let attr_target_of_symarg (symarg : e) =
  let str_of_arg (_, _, vars, e') =
    match e' with
    | Const (CStr s) -> Some s
    | Id v -> (match VarMap.find_opt v vars with Some (C (CStr s)) -> Some s | _ -> None)
    | _ -> None
  in
  let of_name = function
    | "class" -> `Class
    | "names" -> `Names
    | s -> `Named s
  in
  match symarg with
  | (_, _, _, Id label) ->
    (match Variable.get_name label with
     | Some "R_ClassSymbol" -> `Class
     | Some "R_NamesSymbol" -> `Names
     | Some "R_DimSymbol" -> `Named "dim"
     | Some "R_DimNamesSymbol" -> `Named "dimnames"
     | Some "R_LevelsSymbol" -> `Named "levels"
     | Some "R_RowNamesSymbol" -> `Named "row.names"
     | _ -> `Dynamic)
  | (_, _, _, Call ((_, _, _, Id inst), [ strarg ])) when is_rfun "install" inst ->
    (match str_of_arg strarg with Some s -> of_name s | None -> `Dynamic)
  | _ -> `Dynamic

(* If [arg] is a variable whose const-prop kind is a tracked STRSXP (built by
   allocVector(STRSXP,n) + SET_STRING_ELT), return its ordered string contents. *)
let strsxp_of_arg (arg : e) =
  match arg with
  | (_, _, vars, Id v) ->
    (match VarMap.find_opt v vars with Some (S strs) -> Some strs | _ -> None)
  | _ -> None

(* Transformation to MLsem ast
  GTy is used for gradual types, Ty for "normal" types
*)
let rec aux_e (eid, _decl, vars, e) =
  let int_const_of_expr expr =
    match expr with
    | (_, _, _, Const (CInt n)) -> Some n
    | (_, _, _, Id v) ->
        (match VarMap.find_opt v vars with
        | Some (C (CInt n)) -> Some n
        | _ -> None)
    | _ -> None
  in
  (* inherits(x, "name") / Rf_inherits(x, "name") used as an if/ternary
     condition: recognise it so the whole conditional becomes a type-case
     testing x directly against [Defs.inherits_test_ty name] ("attr value
     carrying class name"). mlsem's occurrence typing then refines x in both
     branches. Returns [Some (scrutinee, tested_type, negated)] for the caller
     to feed into [A.If]/[A.Ite] directly (no [tobool] wrapper, which would hide
     x from the refinement); [negated] is set for a leading [!] so the caller
     swaps the branches. A non-literal class argument is not recognised and
     falls through to the general [tobool] path (base.ty's [inherits], no
     refinement -- unlike the predicates typed in base.ty, [inherits] takes the
     class as a runtime argument, so the refinement must be installed here). *)
  let inherits_typecase cond =
    let negated, (_, _, _, c) =
      match cond with
      | (_, _, _, Unop (op, inner)) when Variable.get_name op = Some "!__1" -> true, inner
      | _ -> false, cond
    in
    match c with
    | Call ((_,_,_, Id f), [v_arg; (_,_,_, Const (CStr name))])
        when is_rfun "inherits" f ->
        Some (aux_e v_arg, GTy.mk (Defs.inherits_test_ty name), negated)
    | _ -> None
  in
  let rec aux e =
    match e with
    | Const c -> A.Value (typeof_const c |> GTy.mk )
    | Id v -> A.Var v
    | Declare (v,e) -> A.Declare (v, aux_e e)
    | Let (v,e1,e2) -> A.Let ([], v, aux_e e1, aux_e e2)
    | VarAssign (v,e2) -> A.VarAssign (v, aux_e e2)
    | Unop (op,e) -> aux (Call ((Eid.unique (), DeclMap.empty, vars, Id op), [e]))
    | Binop (op,e1,e2) -> aux (Call ((Eid.unique (), DeclMap.empty, vars, Id op), [e1; e2]))
    | FieldRead (e, field) -> A.Projection (SA.PiField field, aux_e e)
    | FieldUpdate (e, field, value) -> 
        A.Operation (SA.RecUpd field, (Eid.unique (), A.Constructor (SA.Tuple 2, [aux_e e; aux_e value])))
    (* Some special cases for some calls*)
    | Call ((_,_,_, Id v), [(_, _,_,Id vty); n_expr ])
      when is_rfun "allocVector" v &&
           (Variable.get_name vty = Some "VECSXP") &&
           Option.is_some (int_const_of_expr n_expr) ->
        let n =
          match int_const_of_expr n_expr with
          | Some n -> n
          | None -> assert false
        in
        let ty = Defs.allocVector_vecsxp_ty n in
        A.Value (GTy.mk ty)
    | Call ((_,_,_,Id v1), [(_, _,_,Id vty); ( _ ,_,_, Id v2) ])
      when is_rfun "mkNamed" v1 &&
           (Variable.get_name vty = Some "VECSXP") ->
        Format.eprintf "%a@." (VarMap.pp pp_kind) vars;
        let kind = VarMap.find_opt v2 vars in 
        (* print the content of vars *)
        let names = match kind with 
            | None -> failwith (Format.asprintf "No variable %a for names for mkNamed" Variable.pp v2)
            | Some (C (CArray names)) -> extract_names names
            | Some _ -> failwith "Expected an array of strings."
           in
        let ty = Defs.mkNamed_vecsxp_ty names in
        A.Value (GTy.mk ty)
    (* getAttrib(v, sym): route by the attribute the symbol denotes.
       - Class: dedicated projection refining to the class-name vector
         when v's classes are concretely known (see [Defs.getAttrib_class_ty]).
       - Named name: project that label of v's attrs (value | NULL).
       - Dynamic: project the union of all of v's attributes (value | NULL).
       - Names: read the content list's field labels back as a names vector. *)
    | Call((_,_,_,Id f), [v_arg; symarg])
        when is_rfun "getAttrib" f ->
        (match attr_target_of_symarg symarg with
         | `Class ->
             A.Projection
               (SA.PCustom { pname="getClassAttrib"; pgen=true;
                             pdom=Defs.getAttrib_class_pdom; proj=Defs.getAttrib_class_ty },
                aux_e v_arg)
         | `Named name ->
             A.Projection
               (SA.PCustom { pname="getAttrib:" ^ name; pgen=true;
                             pdom=Defs.getAttrib_attr_pdom; proj=Defs.getAttrib_named_ty name },
                aux_e v_arg)
         | `Dynamic ->
             A.Projection
               (SA.PCustom { pname="getAttrib:?"; pgen=true;
                             pdom=Defs.getAttrib_attr_pdom; proj=Defs.getAttrib_dynamic_ty },
                aux_e v_arg)
         | `Names ->
             (* names live in the content list's field labels (mkNamed /
                SET_VECTOR_ELT); read them back as a names vector. *)
             A.Projection
               (SA.PCustom { pname="getNamesAttrib"; pgen=true;
                             pdom=Defs.getAttrib_attr_pdom; proj=Defs.getAttrib_names_ty },
                aux_e v_arg))
    (* setAttrib(v, sym, val): dual of getAttrib.
       - Class: keep v's content/attrs, install val's classes.
       - Named name: set that attrs label to val's type (content/classes/other
         attrs preserved).
       - Dynamic: widen the attrs tail with val's type.
       - Names: deferred; warn and type generically.
       (The R-internal [classgets(v, val)] 2-arg alias is not in the corpus, so
       we do not special-case it.) *)
    | Call(((_,_,_,Id f) as fnode), ([v_arg; symarg; val_arg] as args))
        when is_rfun "setAttrib" f ->
        (match attr_target_of_symarg symarg with
         | `Class ->
             (* if val's STRSXP contents are const-prop'd, take the exact class
                names (no type-read, no "" seed); else the general type-level path. *)
             (match strsxp_of_arg val_arg with
              | Some strs ->
                  A.Constructor
                    (SA.CCustom { cname="setClassAttrib!"; cgen=true;
                                  cdom=Defs.setAttrib_one_cdom;
                                  cons=Defs.setAttrib_class_cons_known strs },
                     [aux_e v_arg])
              | None ->
                  A.Constructor
                    (SA.CCustom { cname="setClassAttrib"; cgen=true;
                                  cdom=Defs.setAttrib_class_cdom; cons=Defs.setAttrib_class_cons },
                     [aux_e v_arg; aux_e val_arg]))
         | `Named name ->
             A.Constructor
               (SA.CCustom { cname="setAttrib:" ^ name; cgen=true;
                             cdom=Defs.setAttrib_class_cdom; cons=Defs.setAttrib_named_cons name },
                [aux_e v_arg; aux_e val_arg])
         | `Dynamic ->
             A.Constructor
               (SA.CCustom { cname="setAttrib:?"; cgen=true;
                             cdom=Defs.setAttrib_class_cdom; cons=Defs.setAttrib_dynamic_cons },
                [aux_e v_arg; aux_e val_arg])
         | `Names ->
             (* if val's STRSXP contents are const-prop'd, relabel the list's
                slots with the ordered names; else defer (warn + generic). *)
             (match strsxp_of_arg val_arg with
              | Some strs ->
                  A.Constructor
                    (SA.CCustom { cname="setNamesAttrib!"; cgen=true;
                                  cdom=Defs.setAttrib_one_cdom;
                                  cons=Defs.setAttrib_names_cons_known strs },
                     [aux_e v_arg])
              | None ->
                  Format.eprintf
                    "Warning: setAttrib on R_NamesSymbol; names are modeled via list field \
                     bindings, not the attrs field@.";
                  build_call (aux_e fnode) (List.map aux_e args)))
    (* Named lists*)
    | Call ((eid,_,_,Id v1), [(_, _,_,Id v2) as id2; ( _ ,_,_, idx); value])
      when (Variable.get_name v1 = Some "SET_VECTOR_ELT") &&
           (match VarMap.find_opt v2 vars with Some (L _) -> true | _ -> false) ->
        let idx = match idx with
          | Const (CInt n) -> n
          | Id v -> (match VarMap.find_opt v vars with
              | Some (C (CInt n)) -> n
              | _ -> failwith "Expected an integer constant for the index in SET_VECTOR_ELT")
          | _ -> failwith "Expected an integer constant for the index in SET_VECTOR_ELT"
        in
        let name = match VarMap.find_opt v2 vars with
            | Some (L names) -> List.nth names idx
            | _ -> assert false (* guaranteed by the when guard *)
           in
        let f = Defs.set_vector_elt_ty name in
       build_call (eid, A.Value (GTy.mk f)) [aux_e id2; aux_e value]
    (* General case for calls *)
    | Call (f,args) -> 
        let es = List.map aux_e args in 
        let f = aux_e f in 
        build_call f es
    | If (cond, then_, else_) ->
        (match inherits_typecase cond with
         | Some (scrut, tau, false) ->
             A.If (scrut, tau, aux_e then_, Option.map aux_e else_)
         | Some (scrut, tau, true) ->
             (* !inherits: the original then runs when x is NOT in tau (the
                else-slot); the in-tau slot runs the original else, or nothing. *)
             let in_tau = match else_ with Some e -> aux_e e | None -> (Eid.unique (), A.Void) in
             A.If (scrut, tau, in_tau, Some (aux_e then_))
         | None ->
             let cond = aux_e cond in
             let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
             A.If (cond, GTy.mk Rstt.Cint.tt, aux_e then_, Option.map aux_e else_))
    | Ite (cond, then_, else_) ->
        (match inherits_typecase cond with
         | Some (scrut, tau, false) ->
             A.Ite (scrut, tau, aux_e then_, aux_e else_)
         | Some (scrut, tau, true) ->
             A.Ite (scrut, tau, aux_e else_, aux_e then_)
         | None ->
             let cond = aux_e cond in
             let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
             A.Ite (cond, GTy.mk Rstt.Cint.tt, aux_e then_, aux_e else_))
    | While (cond, body) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.While (cond, GTy.mk Rstt.Cint.tt, aux_e body)
    | Seq (e1,e2) -> A.Seq (aux_e e1, aux_e e2)
    | Return e -> A.Return (match e with 
        | None -> (Eid.unique (), A.Void)
        | Some e -> aux_e e)
    | TyCheck (e, ty) -> 
        let e = aux_e e in 
        let tt = Eid.unique (), A.Value (GTy.mk Rstt.Cint.tt) in
        let ff = Eid.unique (), A.Value (GTy.mk Rstt.Cint.ff) in
        A.Ite (e, GTy.mk ty, tt, ff)
    | Cast (ty, e) -> let e = aux_e e in
        A.TypeCoerce (e, typeof_ctype ty |> GTy.mk , SA.NoCheck)
    | CoerceNarrow (ty, e) -> let e = aux_e e in
        (* Intersecting cast: result is [typeof(ty) & source], so an
           already-precise initializer is preserved and only a too-wide one is
           narrowed to the declared pointer type. *)
        A.TypeCast (e, typeof_ctype ty |> GTy.mk , SA.NoCheck)
    | Function (_name, _ret_type, params, body) ->
      (* Create lets in the body for each argument: match parameter names with
       type variable in the domain *)
      let arg_types = List.map 
        (function (cty, _) -> 
          (* Special case of SEXPs. we coul restrict it to actual SEXP but
           typeof_ctype currently does not return all possible sexp types for SEXP. *)
          match cty with 
          | SEXP -> Ty.cap Defs.any_sexp (TVar.typ (TVar.mk KInfer None))
          | _ -> Ty.cap (typeof_ctype cty) (TVar.typ (TVar.mk KInfer None))
        )  params
      in  
      let add_let body (p, ty) = 
        Eid.unique(), A.Let ([], p, (Eid.unique (), A.Value (GTy.mk ty)), body)
      in
      let body = List.fold_left add_let (aux_e body) (List.combine (List.map snd params) arg_types) in
      (* Suggested type decomposition, domain, type variable, body*)
      let lambda = A.Lambda ([], Some (GTy.mk @@ Tuple.mk arg_types), 
        MVariable.create Immut None, body) in
        (* we could give a more precise type for the attributes: noclass But then it displays <> after the closures, which is
          too much noise! *)
        A.Constructor
          (CCustom { cname="cattr" ; cgen=true ; cdom=AttrConstr.cdom Rstt.Classes.any ; cons=AttrConstr.cons Rstt.Classes.any },
           [Eid.unique(), lambda])
    | Switch (e, cases) ->
        let e = aux_e e in
        let make_pattern_case acc (case_e, body_e, has_break) =
          let case_ty = match case_e with 
            | _,_,_,Const c -> typeof_const c 
            | _,_,_,Id v -> (match Defs.BuiltinVar.find_builtin v with
                | Some ty -> ty
                | None -> Rstt.Cint.any_na) (* unknown constant: fall back, don't crash *)
            | _,_,_,Noop -> Ty.any (* Default case *)
            | _ -> failwith "Invalid switch case expression"
          in
          let body = aux_e body_e in
          if has_break || List.is_empty acc then 
            (A.PType case_ty, body) :: acc
          else 
            let last_case, last_body = List.hd acc in
            (* 2  when there are no breaks:
              - current body is empty (i.e. a const NULL in the body) : we can merge ( = or) the patterns  
              - current body is not empty: current case will also execute the 
              body of the next case(s) until reaching a break, so we merge those bodies.
              Note: the empty body optimization does not seem to bring much in terms of perf. 
              A better optimization might be to encode it as a series of if, else if else. *)
             match body_e with 
            | _,_,_,Const CNull -> 
              (A.POr (A.PType case_ty, last_case), last_body) :: (List.tl acc)
            | _ -> let new_body = Eid.unique (), A.Seq (last_body, body) in 
              (A.PType case_ty, new_body) :: acc
        in
        (* We start from the reverse list because we want to merge bodies of cases with the one 
        before when there is a break. So we start with the default case, then the one before
         and so on *)
        A.PatMatch (e, List.fold_left make_pattern_case [] (List.rev cases))
    | Break -> A.Break
    | Next -> A.Break
    (* Lambda *)
    | _ -> A.Value (GTy.mk Rstt.Cenums.void)  (* Placeholder for other expressions *)
    
  in
  (eid, aux e)



  let map f e = 
    let rec aux (eid, decl, vars, e) = 
      let e = match e with 
      | Const _ | Id _ -> e
      | Declare (v,e) -> Declare (v, aux e)
      | Let (v,e1,e2) -> Let (v, aux e1, aux e2)
      | VarAssign (v,e2) -> VarAssign (v, aux e2)
      | Unop (op,e) -> Unop (op, aux e)
      | Binop (op,e1,e2) -> Binop (op, aux e1, aux e2)
      | FieldRead (e, field) -> FieldRead (aux e, field)
      | FieldUpdate (e, field, value) -> FieldUpdate (aux e, field, aux value)
      | Call (f,args) -> Call (aux f, List.map aux args)
      | If (cond, then_, else_) -> 
          If (aux cond, aux then_, Option.map aux else_)
      | Ite (cond, then_, else_) ->
          Ite (aux cond, aux then_, aux else_)
      | While (cond, body) -> While (aux cond, aux body)
      | Seq (e1,e2) -> Seq (aux e1, aux e2)
      | Return e -> Return (Option.map aux e)
      | TyCheck (e, ty) -> TyCheck (aux e, ty)
      | Cast (ty, e) -> Cast (ty, aux e)
      | CoerceNarrow (ty, e) -> CoerceNarrow (ty, aux e)
      | Function (name, ret_type, params, body) ->
          Function (name, ret_type, params, aux body)
      | Switch (e, cases) ->
          let cases = List.map (fun (case_e, body_e, has_break) -> (aux case_e, aux body_e, has_break)) cases in
          Switch (aux e, cases)
      | Break -> Break
      | Next -> Next
      | Noop -> Noop
    in
    f (eid, decl, vars, e)
  in aux e


  let recognize_const_comparison e =
    let f expr = match expr with 
    | id, decl, vars, (Binop (op, e, (_,_,_,Const c)) | Binop (op, (_,_,_,Const c), e))
    when Variable.equal op Defs.BuiltinOp.eq -> id, decl, vars, TyCheck (e, typeof_const c)
    | id, decl, vars, (Binop (op, e, (_,_,_,Id v)) | Binop (op, (_,_,_,Id v), e))
    when Variable.equal op Defs.BuiltinOp.eq -> 
      (match Defs.BuiltinVar.find_builtin v with 
      | Some built -> id, decl, vars, TyCheck (e, built)
      | None -> expr)
    | id, decl, vars, (Binop (op, e, (_,_,_,Const c)) | Binop (op, (_,_,_,Const c), e))
    when Variable.equal op Defs.BuiltinOp.neq -> id, decl, vars, TyCheck (e, Ty.neg (typeof_const c))
    | id, decl, vars, (Binop (op, e, (_,_,_,Id v)) | Binop (op, (_,_,_,Id v), e))
    when Variable.equal op Defs.BuiltinOp.neq -> 
      (match Defs.BuiltinVar.find_builtin v with 
      | Some built -> id, decl, vars, TyCheck (e, Ty.neg built)
      | None -> expr)
    | id, decl, vars, (Binop (op, e, (_, _,_,Const (CInt c))) | Binop (op, (_, _,_,Const (CInt c)), e))
    when Variable.equal op Defs.BuiltinOp.inf_strict -> 
      id, decl, vars, TyCheck (e, Rstt.Cint.interval (None, Some (c-1))) (* interval is closed by < is open on the right*)
    | id, decl, vars, (Binop (op, e, (_, _,_,Const (CInt c))) | Binop (op, (_, _,_,Const (CInt c)), e))
    when Variable.equal op Defs.BuiltinOp.sup_strict -> 
      id, decl, vars, TyCheck (e, Rstt.Cint.interval (Some (c+1), None)) (* interval is closed by > is open on the left*)
    | e -> e
    in
    map f e

  let propagate_const e =
    (* Traverse the AST while carrying an environment of known constants.
       Each node gets its `vars` field updated to the current constant env. *)

    let unop_const (op : Variable.t) (c : const) : const option =
      match Variable.get_name op, c with
      | Some "-__1", CInt n -> Some (CInt (-n))
      | _ -> None
    in

    let binop_const (op : Variable.t) (c1 : const) (c2 : const) : const option =
      match Variable.get_name op, c1, c2 with
      (* Int arithmetic *)
      | Some "+__2", CInt a, CInt b -> Some (CInt (a + b))
      | Some "-__2", CInt a, CInt b -> Some (CInt (a - b))
      | Some "*__2", CInt a, CInt b -> Some (CInt (a * b))
      | Some "/__2", CInt a, CInt b -> if b = 0 then None else Some (CInt (a / b))
      | Some "%__2", CInt a, CInt b -> if b = 0 then None else Some (CInt (a mod b))
      | _ -> None
    in

    (* Returns (transformed_expr, updated_env, value_of_expr) where value_of_expr
       is the known kind (constant or label list) of the expression, if any. *)
    let rec aux ((id, decl, _vars, expr) : e) (env : kind VarMap.t) : e * kind VarMap.t * kind option =
      match expr with
      | Const c -> ((id, decl, env, expr), env, Some (C c))
      | Id v -> ((id, decl, env, expr), env, VarMap.find_opt v env)
      | Noop | Break | Next -> ((id, decl, env, expr), env, None)

      | Declare (v, e1) ->
          let e1', env', _ = aux e1 env in
          ((id, decl, env, Declare (v, e1')), env', None)

      | Let (v, e1, e2) ->
          let e1', env1, k1 = aux e1 env in
          let env_for_body =
            match k1 with
            | Some k -> VarMap.add v k env1
            | None -> env1
          in
          let e2', env2, k2 = aux e2 env_for_body in
          ((id, decl, env, Let (v, e1', e2')), env2, k2)

      | VarAssign (v, e1) ->
          let e1', env1, k1 = aux e1 env in
          let env' =
            match k1 with
            | Some k -> VarMap.add v k env1
            | None -> env1
          in
          ((id, decl, env, VarAssign (v, e1')), env', None)

      | Unop (op, e1) ->
          let e1', env1, k1 = aux e1 env in
          let expr', kres =
            match k1 with
            | Some (C c) ->
                (match unop_const op c with
                | Some c' -> (Const c', Some (C c'))
                | None -> (Unop (op, e1'), None))
            | _ -> (Unop (op, e1'), None)
          in
          ((id, decl, env, expr'), env1, kres)

      | Binop (op, e1, e2) ->
          let e1', env1, k1 = aux e1 env in
          let e2', env2, k2 = aux e2 env1 in
          let expr', kres =
            match (k1, k2) with
            | Some (C c1), Some (C c2) ->
                (match binop_const op c1 c2 with
                | Some c' -> (Const c', Some (C c'))
                | None -> (Binop (op, e1', e2'), None))
            | _ -> (Binop (op, e1', e2'), None)
          in
          ((id, decl, env, expr'), env2, kres)

      | FieldRead (e1, field) ->
          let e1', env1, _k1 = aux e1 env in
          (* We could propagate field accesses on constants if we had a more precise representation of constants (e.g. structs with fields). For now we don't. *)
          ((id, decl, env, FieldRead (e1', field)), env1, None)

      | FieldUpdate (e1, field, e2) ->
          let e1', env1, _k1 = aux e1 env in
          let e2', env2, _k2 = aux e2 env1 in
          ((id, decl, env, FieldUpdate (e1', field, e2')), env2, None)

      | Call (f, args) ->
          let f', env1, _ = aux f env in
          let args_rev, env2 =
            List.fold_left
              (fun (acc, acc_env) a ->
                let a', acc_env', k = aux a acc_env in
                ((a', k) :: acc, acc_env'))
              ([], env1)
              args
          in
          let args_with_kinds = List.rev args_rev in
          let args' = List.map fst args_with_kinds in
          let env_out, kres =
            match f', args_with_kinds with
            (* PROTECT(arg) / protect / Rf_protect: propagate the arg's kind *)
            | (_, _, _, Id fv), [(_, k)]
              when Variable.get_name fv = Some "PROTECT" || is_rfun "protect" fv -> env2, k
            (* mkChar("s"): propagate the string-constant kind, so a
               SET_STRING_ELT below can read the assigned string. *)
            | (_, _, _, Id fv), [(_, (Some (C (CStr _)) as k))]
              when is_rfun "mkChar" fv -> env2, k
            (* mkNamed(VECSXP, names_var): resolve names from the kind *)
            | (_, _, _, Id fv), [((_, _, _, Id vty), _); (_, Some (C (CArray names)))]
              when is_rfun "mkNamed" fv
                   && Variable.get_name vty = Some "VECSXP" ->
                env2, Some (L (extract_names names))
            (* allocVector(STRSXP, n): seed an empty ordered STRSXP of length n
               ("" = R_BlankString default per slot). *)
            | (_, _, _, Id fv), [((_, _, _, Id vty), _); (_, Some (C (CInt n)))]
              when is_rfun "allocVector" fv
                   && Variable.get_name vty = Some "STRSXP" && n >= 0 ->
                env2, Some (S (List.init n (fun _ -> "")))
            (* SET_STRING_ELT(v, i, mkChar s): set index i of v's tracked STRSXP.
               (v = SET_STRING_ELT(v,...) per PAst rewrite, so VarAssign rebinds v.) *)
            | (_, _, _, Id fv),
              [ ((_, _, _, Id _), Some (S strs)); (_, Some (C (CInt i))); (_, Some (C (CStr s))) ]
              when Variable.get_name fv = Some "SET_STRING_ELT"
                   && i >= 0 && i < List.length strs ->
                env2, Some (S (List.mapi (fun j o -> if j = i then s else o) strs))
            (* SET_STRING_ELT on a tracked STRSXP but with a dynamic index/string:
               invalidate the tracking so no partial S lingers (drop to the
               type-level fallback). *)
            | (_, _, _, Id fv), ((_, _, _, Id vv), Some (S _)) :: _
              when Variable.get_name fv = Some "SET_STRING_ELT" ->
                VarMap.remove vv env2, None
            | _ -> env2, None
          in
          ((id, decl, env, Call (f', args')), env_out, kres)

      | If (cond, then_, else_) ->
          let cond', env1, _ = aux cond env in
          let then_', env2, _ = aux then_ env1 in
          let else_', env3 =
            match else_ with
            | None -> (None, env2)
            | Some e3 ->
                let e3', env3, _ = aux e3 env2 in
                (Some e3', env3)
          in
          ((id, decl, env, If (cond', then_', else_')), env3, None)

      | Ite (cond, then_, else_) ->
          let cond', env1, _ = aux cond env in
          let then_', env2, _ = aux then_ env1 in
          let else_', env3, _ = aux else_ env2 in
          ((id, decl, env, Ite (cond', then_', else_')), env3, None)

      | While (cond, body) ->
          let cond', env1, _ = aux cond env in
          let body', env2, _ = aux body env1 in
          ((id, decl, env, While (cond', body')), env2, None)

      | Seq (e1, e2) ->
          let e1', env1, _ = aux e1 env in
          let e2', env2, k2 = aux e2 env1 in
          ((id, decl, env, Seq (e1', e2')), env2, k2)

      | Return eo ->
          let eo', env' =
            match eo with
            | None -> (None, env)
            | Some e1 ->
                let e1', env', _ = aux e1 env in
                (Some e1', env')
          in
          ((id, decl, env, Return eo'), env', None)

      | TyCheck (e1, ty) ->
          let e1', env1, k1 = aux e1 env in
          ((id, decl, env, TyCheck (e1', ty)), env1, k1)

      | Cast (ty, e1) ->
          let e1', env1, _ = aux e1 env in
          ((id, decl, env, Cast (ty, e1')), env1, None)

      | CoerceNarrow (ty, e1) ->
          let e1', env1, _ = aux e1 env in
          ((id, decl, env, CoerceNarrow (ty, e1')), env1, None)

      | Function (name, ret_type, params, body) ->
          (* Do not propagate outer constants into function bodies. *)
          let body', _, _ = aux body VarMap.empty in
          ((id, decl, env, Function (name, ret_type, params, body')), env, None)

      | Switch (e1, cases) ->
          let e1', env1, _ = aux e1 env in
          let cases', env' =
            List.fold_left
              (fun (acc_cases, acc_env) (case_e, body_e, has_break) ->
                let case_e', envc, _ = aux case_e acc_env in
                let body_e', envb, _ = aux body_e envc in
                (acc_cases @ [ (case_e', body_e', has_break) ], envb))
              ([], env1)
              cases
          in
          ((id, decl, env, Switch (e1', cases')), env', None)
    in
    let e', _, _ = aux e VarMap.empty in
    e'

  let to_mlsem e =
    e |> recognize_const_comparison |> propagate_const |> aux_e |> Mlsem.Lang.Transform.transform