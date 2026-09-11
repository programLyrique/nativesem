open Rstt
open Mlsem.Types 
open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API *)

(* The set of values that can flow as a real C SEXP (anything reachable via
   [r_obj*]). Of the [Prim] family only [Chr] (CHARSXP) is itself a passable
   SEXP — the others ([Int], [Dbl], [Lgl], [Clx], [Raw]) only exist as the
   element type of an enclosing [Vec]. Everything else except sym and null 
   can carry attributes,
   so [Attr.any] (any attr-tagged value, regardless of class) covers env,
    list, lang, closure, externalptr, and vec uniformly. *)
let any_sexp = Rstt.(Ty.disj [Prim.(Chr.any |> mk); Attr.any; Null.any; Sym.any; ])

let any_c = Ty.disj [Cint.any; Cenums.char; Cenums.double; Cptr.any]

(* ===== Character vectors of statically-known strings =====

   rstt's vector constructor only lets a *scalar* ([Vec.Scalar], i.e. [v1(...)])
   carry a refined element type: a non-scalar [Vec.Vector] must be given a
   "whole" primitive component (a full [CHR] / [INT] / ... , see
   [Rstt.Prim.is_whole]), because a vector of unknown length cannot soundly
   claim every one of its elements is one specific string. Vector lengths are no
   longer part of the type algebra, so the length-N vector of class/name
   singletons the previous version built ([v[N]("a" | "b")]) has no counterpart:
   we keep the precise type only in the length-1 case and widen to [v(CHR)]
   otherwise. *)
let chr_vec = Rstt.(Vec.mk (Vec.Vector (Prim.mk Prim.Chr.any)))

(* The character vector holding exactly the strings [names] when that is
   expressible, i.e. when [names] is a single string; [v(CHR)] otherwise.
   [Chr.str'] and not [Chr.str]: primitive domains include NA by default in the
   current rstt, and a recovered class/element name is never NA. *)
let chr_vec_of_names names =
  let open Rstt in
  match names with
  | [ name ] -> Vec.mk (Vec.Scalar (Prim.mk (Prim.Chr.str' name)))
  | _ -> chr_vec

let exprsxp = Rstt.Builder.(
  build TIdMap.empty (TList {bindings=[] ; tl=TOption (TCup (TLang, TSym))})
)

(* Special type constructors for lists*)
let allocVector_vecsxp_ty n =
  let open Rstt.Builder in 
  let builder = TList ({ bindings = List.init n (fun i -> ("_" ^ (string_of_int i), TNull)) ; tl = TOption TEmpty }) in
  build TIdMap.empty builder

let mkNamed_vecsxp_ty names = 
  let open Rstt.Builder in 
  let builder =
    TList
      { bindings = List.map (fun name -> (name, TAny)) names;
        tl = TOption TEmpty }
  in 
  build TIdMap.empty builder


let set_vector_elt_ty name =
  let open Rstt.Builder in
  let _, r' = rvar empty_env "r" in
  let _, a = tvar empty_env "a" in
  (* [{ `r }, 'a] -> {name: 'a, `r} ; actually, maybe 'a & any_sexp for the 1st argument... *)
  let builder =
    TArrow
      ( TTuple [TList {bindings = []; tl = TRowVar r'}; TVar a],
        TList ({bindings = [(name, TVar a)]; tl = TRowVar r'}) )
  in
  build TIdMap.empty builder

(* Domain of the [getAttrib(_, R_ClassSymbol)] projection: any R SEXP. *)
let getAttrib_class_pdom _result_ty = any_sexp

(* Type of [getAttrib(v, R_ClassSymbol)] given v's inferred type. The class
   attribute is a chr vector listing v's classes. When v's [classes]
   component (from its [attr(content, classes)] encoding) is a concrete,
   positive list we refine to that list of names (only expressible when there is
   a single class, see [chr_vec_of_names]); otherwise we fall back to [v(CHR)].
   The result is [Attr.mk_content]-wrapped to match the encoding R values use in
   [.ty] (Builder.build does this implicitly for Vec; we have to be explicit
   here). *)
let getAttrib_class_ty v_ty =
  let open Rstt in
  let fallback = Attr.mk_content chr_vec in
  try
    let classes_ty = Attr.proj_classes v_ty in
    match Classes.destruct classes_ty with
    | { pos; neg = []; unk = []; tail = NoOther } when pos <> [] ->
      let names = List.map (fun (Classes.L (s, _)) -> s) pos in
      Attr.mk_content (chr_vec_of_names names)
    | _ -> fallback
  with _ -> fallback

(* [inherits(x, "name")] is lowered (in [ast.ml]) to a type-case testing [x]
   directly against [inherits_test_ty name]: the set of attr-tagged values whose
   class set includes [name] (other class labels left [Unknown], i.e. they may
   or may not be present -- [inherits] only asserts membership, not the exact
   class set; [AllOthers] would be wrong as it asserts every other class is
   present, a near-empty type). mlsem's occurrence typing then refines [x] in
   both branches: the then-branch to [x & <name, ...>], the else-branch to
   [x \ <name, ...>] (which keeps e.g. the [p(CHR)] CHARSXP half of [any_sexp],
   soundly: a CHARSXP carries no class so the test is false for it).

   Testing [x] directly (rather than projecting its class component) keeps the
   type-case total over all SEXPs -- a projection would be partial (undefined on
   the classless [p(CHR)]), making [inherits] on a possibly-CHARSXP value
   untypeable. *)
let inherits_test_ty name =
  let open Rstt in
  let classes =
    Classes.mk { pos = [Classes.L (name, [])]; neg = []; unk = []; tail = Classes.Unknown }
  in
  Attr.mk { content = Attr.proj_content Attr.any; classes ; attrs = Lst.any }

(* setAttrib(vec, R_ClassSymbol, val): the result is vec's value re-tagged with
   the class attribute carried by [val]. This is the dual of
   [getAttrib_class_ty]: there we read class names out of [v]'s [classes]
   component, here we write them in from [val]'s type.

   [setAttrib_class_classes] derives the class component from [val]'s type. When
   [val] is a chr vector whose element type is a positive, finite set of string
   singletons (e.g. [mkString "foo"], typed [(c_string('a)) --> v1(^CHR('a))]
   in base.ty, or a variable holding such a value — the type flows through let
   bindings) we set exactly those classes ([tail = NoOther]). Otherwise the
   concrete names are unknown and we fall back to [Classes.any], i.e. arbitrary
   classes. Only a *scalar* vector can carry string singletons in the current
   type algebra, so in practice this recovers the single-class case; the
   multi-class [allocVector(STRSXP,n)] + [SET_STRING_ELT] build-up is recovered
   by the const-propagation path instead ([setAttrib_class_cons_known]).

   The empty string is dropped from the recovered names: it is the STRSXP
   allocation default ("") standing in for any slot not provably overwritten, not
   a class the program asked for, and "" is not a meaningful S3/S4 class name.
   (Soundness note: this means a degenerate genuine "" class would be missed, but
   no real code dispatches on it.) If nothing but "" remains, the names are
   treated as unknown ([Classes.any]). *)
let setAttrib_class_classes val_ty =
  let open Rstt in
  let names_opt =
    try
      let content = try Attr.proj_content val_ty with _ -> val_ty in
      match Vec.destruct content with
      | [ (atom, []) ] ->
        let elem = match atom with
          | Vec.Vector e | Vec.Scalar e -> e
        in
        (* [Prim.Chr.destruct] returns [(has_na, atomic_line list)] (rstt's
           NA-by-default refactor). A concrete, positive, finite set of string
           singletons is a single line with [pos=true] and no type variables;
           the [content] strings are its class names. *)
        (match Prim.Chr.destruct (Prim.destruct elem) with
         | _, [ { pos = true; prim = content; pvs = []; nvs = [] } ] ->
           (match List.filter (fun s -> s <> "") content with
            | [] -> None
            | names -> Some names)
         | _ -> None)
      | _ -> None
    with _ -> None
  in
  match names_opt with
  | Some names ->
    Classes.mk
      { pos = List.map (fun n -> Classes.L (n, [])) names;
        neg = []; unk = []; tail = Classes.NoOther }
  | None -> Classes.any

(* Domain of the [setAttrib(_, R_ClassSymbol, _)] constructor: the value being
   tagged and the class value are both arbitrary R SEXPs. The checker calls this
   with [Ty.any], so the argument is ignored. The two entries match the two
   constructor arguments ([vec], [val]). *)
let setAttrib_class_cdom _result_ty = [ [ any_sexp; any_sexp ] ]

(* Result type of [setAttrib(vec, R_ClassSymbol, val)] from the inferred types
   of [vec] and [val]: keep vec's content and its other attributes, install
   val's classes. setAttrib on R_ClassSymbol only replaces the class attribute,
   so vec's [attrs] (names, dim, ...) carry through unchanged. *)
let setAttrib_class_cons tys =
  let open Rstt in
  match tys with
  | [ vec_ty; val_ty ] ->
    let content = try Attr.proj_content vec_ty with _ -> vec_ty in
    let attrs = try Attr.proj_attrs vec_ty with _ -> Lst.any in
    let classes = setAttrib_class_classes val_ty in
    Attr.mk { content; classes; attrs }
  | _ -> assert false

(* ===== Arbitrary (non-class) attributes, stored in the [attrs] Lst field =====

   An R value's attributes are modeled by [Attr]'s [attrs] field, a list type
   ([Lst]) whose [bindings] map an attribute name (a string label) to its value
   type, with an open [tl] tail standing for any other (unknown) attribute. When
   the attribute name is statically known (an [R_*Symbol] constant or an
   [install "lit"]), getAttrib/setAttrib read/write the matching label; when it
   is dynamic, getAttrib reads the union of all attributes and setAttrib widens
   the tail. The class attribute is handled separately above (the [classes]
   field); [names] is intentionally not routed here (see ast.ml). *)

(* Build a required list field carrying type [t]. *)
let mk_field (t : Rstt.Ty.t) : Rstt.Ty.F.t =
  let open Rstt in
  Ty.F.mk_descr (Ty.O.required t)

(* An [Lst] value is a tagged record; recover that underlying record type. *)
let lst_record attrs_ty =
  let open Rstt in
  attrs_ty |> Ty.get_descr |> Descr.get_tags |> Tags.get Lst.tag
  |> Op.TagComp.as_atom |> snd

(* [label]'s field in a record, as (value-when-present, can-be-absent). This is
   the projection mechanism [Attr] uses for content/classes/attrs, so it reads
   optional ("present-or-absent") tails correctly -- unlike a raw [Ty.F.get_descr]
   on the add_option-wrapped tail field. A bound required label gives
   (its value, false); an open tail gives (any, true); a closed record lacking
   the label gives (empty, true). *)
let record_proj_field rec_ty label =
  let open Rstt in
  let atom =
    rec_ty |> Ty.get_descr |> Descr.get_records |> Op.Records.proj label |> Ty.O.get
  in
  (Ty.O.Atom.get atom, Ty.O.Atom.is_optional atom)

(* A sentinel label no R attribute uses; projecting it yields the attrs tail. *)
let attr_tail_label = "\000__attr_tail__"

(* Bind [name -> val_ty] in an [attrs] Lst, replacing any prior binding and
   keeping the other bindings and the tail. Falls back to a fresh open list
   carrying just this label when the Lst can't be destructured. *)
let lst_set_label attrs_ty name val_ty =
  let open Rstt in
  let fresh () = Lst.mk { bindings = [ (name, mk_field val_ty) ]; tl = Ty.F.any } in
  try
    match Lst.destruct attrs_ty with
    | [ (atom :: _, []) ] ->
      let bindings = (name, mk_field val_ty) :: List.remove_assoc name atom.Lst.bindings in
      Lst.mk { bindings; tl = atom.Lst.tl }
    | _ -> fresh ()
  with _ -> fresh ()

(* Widen the tail of an [attrs] Lst to also allow [val_ty]: a setAttrib with an
   unknown attribute name means some (unknown) attribute now maps to [val_ty]. *)
let lst_widen_tail attrs_ty val_ty =
  let open Rstt in
  try
    match Lst.destruct attrs_ty with
    | [ (atom :: _, []) ] ->
      let tl = Ty.F.cup atom.Lst.tl (mk_field val_ty) in
      Lst.mk { bindings = atom.Lst.bindings; tl }
    | _ -> Lst.any
  with _ -> Lst.any

(* The value-when-present of a label, capped to a SEXP (getAttrib returns an R
   SEXP, so an open tail's [any] tightens to [any_sexp]). *)
let attr_value_of rec_ty label =
  let open Rstt in
  let v, optional = record_proj_field rec_ty label in
  (Ty.cap v any_sexp, optional)

(* Project the value of attribute [name] from an [attrs] Lst. A bound (required)
   attribute reads back as exactly its value; an absent-or-tail one is unioned
   with NULL, since R's getAttrib returns NULL when the attribute is unset.
   [any_sexp] does NOT include NULL (R's NULL is not attr-tagged), so NULL is
   added explicitly whenever the attribute may be absent. *)
let lst_proj_label attrs_ty name =
  let open Rstt in
  try
    let v, optional = attr_value_of (lst_record attrs_ty) (Labels.named name) in
    if optional then Ty.cup v Null.any else v
  with _ -> Ty.cup any_sexp Null.any

(* Project the value of an unknown (dynamic) attribute name: the union of every
   binding value and the tail value, plus NULL (a dynamic attribute may be
   unset). When the attrs are provably closed (empty tail) this narrows to the
   union of the known attributes | NULL; an open tail widens it to any_sexp. *)
let lst_proj_any attrs_ty =
  let open Rstt in
  try
    let rec_ty = lst_record attrs_ty in
    let labels =
      Lst.destruct attrs_ty
      |> List.concat_map (fun (ps, _ns) ->
        List.concat_map (fun atom -> List.map fst atom.Lst.bindings) ps)
    in
    let vals =
      List.map (fun l -> fst (attr_value_of rec_ty (Labels.named l))) labels
    in
    let tail_val = fst (attr_value_of rec_ty (Labels.named attr_tail_label)) in
    Ty.disj (Null.any :: tail_val :: vals)
  with _ -> Ty.cup any_sexp Null.any

(* Domain of getAttrib(_, sym) for the attrs projections: any R SEXP. *)
let getAttrib_attr_pdom _result_ty = any_sexp

(* The value type R guarantees for the recognized special attributes (their
   slot in a value's attributes always holds this type, or NULL when unset).
   Used to bound getAttrib's result so e.g. getAttrib(x, R_DimSymbol) is an
   integer vector | NULL rather than any_sexp | NULL. [names] is deliberately
   absent (handled via the list-field-names model); [class] has its own field. *)
let attr_int_vec = Rstt.(Attr.mk_content (Vec.mk (Vec.Vector (Prim.mk Prim.Int.any))))
let attr_chr_vec = Rstt.(Attr.mk_content chr_vec)
let attr_list = Rstt.(Attr.mk_content Lst.any)
let special_attr_type = function
  | "dim" -> Some attr_int_vec                        (* INTSXP *)
  | "levels" -> Some attr_chr_vec                     (* factor levels: STRSXP *)
  | "dimnames" -> Some attr_list                      (* VECSXP *)
  | "row.names" -> Some (Ty.cup attr_int_vec attr_chr_vec) (* INTSXP or STRSXP *)
  | _ -> None

(* getAttrib(v, sym) for a statically-known attribute [name]: the value bound to
   [name] in v's attrs, or NULL if it may be unset (capping/NULL handled in
   [lst_proj_label]). For a recognized special attribute, the result is further
   bounded by the type R guarantees for it (so an unset/unknown dim reads as an
   integer vector | NULL, not any_sexp | NULL). *)
let getAttrib_named_ty name v_ty =
  let open Rstt in
  let attrs = try Attr.proj_attrs v_ty with _ -> Lst.any in
  let raw = lst_proj_label attrs name in
  match special_attr_type name with
  | Some known -> Ty.cap raw (Ty.cup known Null.any)
  | None -> raw

(* A positional placeholder label ([_0], [_1], ...) from allocVector_vecsxp_ty,
   as opposed to a real R name. *)
let is_placeholder_label s =
  String.length s >= 2 && s.[0] = '_'
  && String.for_all (function '0' .. '9' -> true | _ -> false)
       (String.sub s 1 (String.length s - 1))

(* getAttrib(v, R_NamesSymbol): names live in the content list's field labels
   (set by mkNamed/SET_VECTOR_ELT), so read them back as a names vector rather
   than from the attrs field. A list with real labels yields exactly those names
   as string singletons when that is expressible -- i.e. for a one-element list,
   see [chr_vec_of_names] -- and [v(CHR)] otherwise; a positional
   placeholder-only list (_0, _1, ... from allocVector(VECSXP,n)) is unnamed ->
   NULL; a non-list / unknown value falls back to chr vector | NULL (a names
   attribute is always a character vector or NULL). "" is a valid R name (an
   unnamed element) and is kept. *)
let getAttrib_names_ty v_ty =
  let open Rstt in
  let fallback = Ty.cup attr_chr_vec Null.any in
  try
    let content = Attr.proj_content v_ty in
    let labels =
      match Lst.destruct content with
      | (atom :: _, _) :: _ -> List.map fst atom.Lst.bindings
      | _ -> []
    in
    if labels = [] then fallback
    else if List.for_all is_placeholder_label labels then Null.any
    else Attr.mk_content (chr_vec_of_names labels)
  with _ -> fallback

(* getAttrib(v, sym) for a dynamic attribute name: the union of all of v's
   attribute values, or NULL. (Capping/NULL handled in [lst_proj_any].) *)
let getAttrib_dynamic_ty v_ty =
  let open Rstt in
  let attrs = try Attr.proj_attrs v_ty with _ -> Lst.any in
  lst_proj_any attrs

(* setAttrib(vec, sym, val) for a statically-known non-class attribute [name]:
   keep vec's content and classes, and set the [name] label of its attrs to
   val's type (other attributes carry through). *)
let setAttrib_named_cons name tys =
  let open Rstt in
  match tys with
  | [ vec_ty; val_ty ] ->
    let content = try Attr.proj_content vec_ty with _ -> vec_ty in
    let classes = try Attr.proj_classes vec_ty with _ -> Classes.any in
    let attrs0 = try Attr.proj_attrs vec_ty with _ -> Lst.any in
    let attrs = lst_set_label attrs0 name val_ty in
    Attr.mk { content; classes; attrs }
  | _ -> assert false

(* setAttrib(vec, sym, val) for a dynamic attribute name: keep content/classes,
   widen the attrs tail with val's type. *)
let setAttrib_dynamic_cons tys =
  let open Rstt in
  match tys with
  | [ vec_ty; val_ty ] ->
    let content = try Attr.proj_content vec_ty with _ -> vec_ty in
    let classes = try Attr.proj_classes vec_ty with _ -> Classes.any in
    let attrs0 = try Attr.proj_attrs vec_ty with _ -> Lst.any in
    let attrs = lst_widen_tail attrs0 val_ty in
    Attr.mk { content; classes; attrs }
  | _ -> assert false

(* ===== const-prop'd STRSXP contents: setAttrib(class/names) with known,
   ordered strings (no type-read, no "" seed). Used when the value being set is
   an [allocVector(STRSXP,n)]+[SET_STRING_ELT] build-up the const-prop tracked. *)

(* Domain of a 1-argument setAttrib constructor (only the value being tagged);
   the class/names strings are captured statically, not passed as a type arg. *)
let setAttrib_one_cdom _result_ty = [ [ any_sexp ] ]

(* setAttrib(vec, R_ClassSymbol, v) with v's STRSXP contents known: the classes
   are exactly [names] minus "" (the alloc default / not a class) -- no type-read
   or "" filtering of an over-approximated element type. *)
let setAttrib_class_cons_known names tys =
  let open Rstt in
  match tys with
  | [ vec_ty ] ->
    let content = try Attr.proj_content vec_ty with _ -> vec_ty in
    let attrs = try Attr.proj_attrs vec_ty with _ -> Lst.any in
    let classes =
      match List.filter (fun s -> s <> "") names with
      | [] -> Classes.any
      | ns ->
        Classes.mk { pos = List.map (fun n -> Classes.L (n, [])) ns;
                     neg = []; unk = []; tail = Classes.NoOther }
    in
    Attr.mk { content; classes; attrs }
  | _ -> assert false

(* Relabel a list's positional slots with [names] (by index), keeping each slot's
   value type. Only when [content] is a single-atom list whose slot count matches
   [names]; otherwise [content] is returned unchanged. ("" names -> "" labels;
   duplicate "" collapse in the record, as with mkNamed.) *)
let relabel_list content names =
  let open Rstt in
  try
    match Lst.destruct content with
    | [ (atom :: _, []) ] when List.length atom.Lst.bindings = List.length names ->
      let bindings = List.map2 (fun (_, f) name -> (name, f)) atom.Lst.bindings names in
      Lst.mk { bindings; tl = atom.Lst.tl }
    | _ -> content
  with _ -> content

(* setAttrib(list, R_NamesSymbol, v) with v's STRSXP contents known: relabel the
   list's slots with the ordered names. Non-list content passes through unchanged
   (names are not modeled for non-lists). getAttrib(_, R_NamesSymbol) then reads
   the relabeled field labels back precisely. *)
let setAttrib_names_cons_known names tys =
  let open Rstt in
  match tys with
  | [ vec_ty ] ->
    let content = try Attr.proj_content vec_ty with _ -> vec_ty in
    let classes = try Attr.proj_classes vec_ty with _ -> Classes.any in
    let attrs = try Attr.proj_attrs vec_ty with _ -> Lst.any in
    Attr.mk { content = relabel_list content names; classes; attrs }
  | _ -> assert false

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let non_int_or_ptr = Ty.diff Ty.any (Ty.cup Cint.any_na Cptr.any) in
  (* In rstt, c_true is defined as c(1), not as c(1..) | c(..-1) i.e. not as an interval*)
  let nonzero = Ty.diff Cint.any_na Cint.ff in
  let nonnull = Ty.diff Cptr.any Cptr.null in
  let def = Arrow.mk non_int_or_ptr Cint.bool in
  (* C conditions follow C truthiness: 0 is false, any other integer is true. *)
  let tt_int = Arrow.mk nonzero Cint.tt in
  let ff_int = Arrow.mk Cint.ff Cint.ff in
  (* Pointers *)
  let tt_ptr = Arrow.mk nonnull Cint.tt in
  let ff_ptr = Arrow.mk Cptr.null Cint.ff in
  let ty = Ty.conj [def;tt_int;ff_int;tt_ptr;ff_ptr] in
  v, ty


module BuiltinOp = struct
let eq = MVariable.create Immut (Some "==__2")
let neq = MVariable.create Immut (Some "!=__2")

let inf_strict = MVariable.create Immut (Some "<__2")
let sup_strict = MVariable.create Immut (Some ">__2")
let all = [ eq ; neq ; inf_strict ; sup_strict ]
let find_builtin str =
  let f v =
    match Variable.get_name v with
    | None -> false
    | Some name -> String.equal name str
  in
  List.find_opt f all
end



module BuiltinVar = struct
  let mk_builtin name ty =
  let v = MVariable.create Immut (Some name) in
  v, ty

  let all = List.map 
    (* Types that are not for vectors should get attributes. *)
    (fun (name, ty) -> mk_builtin name ty)
    [ ("INTSXP", Prim.Int.any |> Prim.mk);
      ("REALSXP", Prim.Dbl.any |> Prim.mk);
      ("RAWSXP", Prim.Raw.any |> Prim.mk);
      ("CPLXSXP", Prim.Clx.any |> Prim.mk);
      ("STRSXP", Prim.Chr.any |> Prim.mk);
      ("LGLSXP", Prim.Lgl.any |> Prim.mk);
      ("NILSXP", Null.any |> Attr.mk_content);
      ("VECSXP", Rstt.Lst.any |> Attr.mk_content);
      ("EXPRSXP", exprsxp);
      ("CLOSXP", Arrow.mk Ty.any Ty.any |> Attr.mk_content);
      ("BUILTINSXP", Arrow.mk Ty.any Ty.any |> Attr.mk_content);
      ("SPECIALSXP", Arrow.mk Ty.any Ty.any |> Attr.mk_content);
      ("CHARSXP", Prim.Chr.any |> Prim.mk);
      ("SYMSXP", Sym.any |> Attr.mk_content);
      ("LISTSXP", Lang.any |> Attr.mk_content);
      ("LANGSXP", Lang.any |> Attr.mk_content);
      ("ENVSXP", Rstt.Env.any |> Attr.mk_content);
      ("EXTPTRSXP", ExternalPtr.any |> Attr.mk_content);
      ("ANYSXP", Attr.any);
      ("DOTSXP", Ty.any); (* TODO: replace by the actual type: a list with an any tail*)
      (* Booleans*)
      ("TRUE", Cint.tt);
      ("FALSE", Cint.ff);
      (* Special values *)
      ("R_NilValue", Null.any);
      (* Predefined preprocessor macros (see
         https://gcc.gnu.org/onlinedocs/cpp/Standard-Predefined-Macros.html).
         [__FILE__], [__DATE__], [__TIME__] expand to string literals,
         [__LINE__] to an integer constant. We use the non-singleton C
         types since the concrete value is never load-bearing for typing. *)
      ("__FILE__", Cptr.string);
      ("__LINE__", Cint.any);
      ("__DATE__", Cptr.string);
      ("__TIME__", Cptr.string);
    ]


let dynamic : (string, Variable.t * Ty.t) Hashtbl.t = Hashtbl.create 64

(* Register [name] with declared type [ty]. If [name] is already present,
   the variable is reused (so call sites alias) and the stored type is
   refined when [ty] is a subtype of the previously-stored type. This lets
   a later, more precise declaration upgrade a fallback binding (e.g. a
   [Ty.any] placeholder installed by [PAst.var]'s unknown-name path gets
   replaced when the real [extern SEXP foo;] declaration is seen). *)
let register_dynamic_binding ?(mut = false) ?annot name ty : Variable.t * Ty.t =
  (* [annot] (when given) is the gradual [GTy.t] to use for the [AnnotMut]
     kind, overriding the default pinned [GTy.mk ty]. Used by [infer_def]
     for SEXP globals: there we want [mk_gradual empty any_sexp] so a read
     can refine to whichever SEXP sub-family (sym / env / lang / ...) the
     use-site actually requires. Non-SEXP globals stay pinned. *)
  let annot_of ty = match annot with Some a -> a | None -> GTy.mk ty in
  match Hashtbl.find_opt dynamic name with
  | Some (v, existing) ->
      let ty =
        if Ty.leq ty existing && not (Ty.equiv ty existing) then ty
        else existing
      in
      let needs_mutable_rebind =
        if not mut then false
        else
          match MVariable.kind v with
          | MVariable.AnnotMut existing -> not (GTy.equiv existing (annot_of ty))
          | _ -> true
      in
      if needs_mutable_rebind then begin
        let v = MVariable.create (MVariable.AnnotMut (annot_of ty)) (Some name) in
        Hashtbl.replace dynamic name (v, ty);
        v, ty
      end else begin
        Hashtbl.replace dynamic name (v, ty);
        v, ty
      end
  | None ->
      let kind = if mut then MVariable.AnnotMut (annot_of ty) else Immut in
      let v = MVariable.create kind (Some name) in
      Hashtbl.add dynamic name (v, ty);
      v, ty

let register_dynamic ?mut name ty : Variable.t =
  fst (register_dynamic_binding ?mut name ty)

let find_builtin (v : Variable.t) =
  let name = Variable.get_name v in
  let from_all = List.find_opt (fun (v2, _) ->
    match Variable.get_name v2 with
    | None -> false
    | Some n -> Some n = name
  ) all |> Option.map snd
  in
  match from_all with
  | Some _ -> from_all
  | None ->
      (match name with
       | None -> None
       | Some n -> Hashtbl.find_opt dynamic n |> Option.map snd)

let find_builtin_var str =
  let from_all =
    List.find_opt (fun (v, _) ->
      match Variable.get_name v with
      | None -> false
      | Some name -> String.equal name str
    ) all |> Option.map fst
  in
  match from_all with
  | Some _ -> from_all
  | None -> Hashtbl.find_opt dynamic str |> Option.map fst

let has_builtin str =
  (List.exists (fun (v, _) ->
    match Variable.get_name v with
    | None -> false
    | Some name -> String.equal name str
  ) all) || Hashtbl.mem dynamic str
end


(* SEXPTYPE numeric values from R's [Rinternals.h], mapped to the
   [BuiltinVar] binding name we register in [BuiltinVar.all] above. Used
   by [PAst.process_call] to rewrite an integer SEXPTYPE argument back to
   its symbolic name (e.g. [Rf_allocVector(13, n)] is rewritten to
   [Rf_allocVector(INTSXP, n)]) so the call matches the [prim] type-tag
   domain of the [allocVector] signature instead of failing as an
   untypeable application. Values outside this table are left alone; the
   call will then fail downstream, which mirrors the runtime check
   [allocVector] does on the type argument. *)
let sexptype_name_of_int = function
  | 0 -> Some "NILSXP"
  | 1 -> Some "SYMSXP"
  | 2 -> Some "LISTSXP"
  | 3 -> Some "CLOSXP"
  | 4 -> Some "ENVSXP"
  | 6 -> Some "LANGSXP"
  | 7 -> Some "SPECIALSXP"
  | 8 -> Some "BUILTINSXP"
  | 9 -> Some "CHARSXP"
  | 10 -> Some "LGLSXP"
  | 13 -> Some "INTSXP"
  | 14 -> Some "REALSXP"
  | 15 -> Some "CPLXSXP"
  | 16 -> Some "STRSXP"
  | 17 -> Some "DOTSXP"
  | 18 -> Some "ANYSXP"
  | 19 -> Some "VECSXP"
  | 20 -> Some "EXPRSXP"
  | 22 -> Some "EXTPTRSXP"
  | 24 -> Some "RAWSXP"
  | _ -> None


(* For the new type algebra *)
let load_parsed_types () =
  let types_dir = Type_parser.find_types_dir () in
  let ty_files =
    Sys.readdir types_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".ty")
    |> List.sort String.compare
    |> List.map (Filename.concat types_dir)
  in
  List.fold_left (fun acc f ->
    let rf_aliases = Filename.basename f = "base.ty" in
    let types = Type_parser.load_file ~rf_aliases f in
    Rstt.Builder.StrMap.union (fun _k _v1 v2 -> Some v2) acc types
  ) Rstt.Builder.StrMap.empty ty_files

(* Wall-clock time of [load_parsed_types ()] at module-init. Captured here and
   exposed so [bin/main.ml] can emit a [Phase: load_ty …] line under
   [--log-times] without restructuring the [let] binding. *)
let ty_load_time = ref 0.0

let parsed_types, parsed_types_penv =
  let t0 = Unix.gettimeofday () in
  let r =
    Mlsem.Types.PEnv.sequential_handler Mlsem.Types.PEnv.empty
      (fun () -> load_parsed_types ()) ()
  in
  ty_load_time := Unix.gettimeofday () -. t0;
  r

let build_vars m = 
  let add_var name ty acc =
    let mVar =  BuiltinOp.find_builtin name |>
       Option.value ~default:(
        BuiltinVar.find_builtin_var name |>
        Option.value ~default:(MVariable.create Immut (Some name))
       ) in
    (mVar, ty) :: acc
  in
  Rstt.Builder.StrMap.fold add_var m []

(* [errno] is a mutable POSIX global (an [int] lvalue): library code does
   [errno = 0] / [errno = ERANGE]. It must be *mutable* so assignments don't
   crash reconstruction with "Cannot assign to an immutable variable", yet we
   still want precise reads ([c_int]). A [.ty] declaration can't express this
   (those bindings are [Immut]), so we build it here as an [AnnotMut] binding
   pinned at [c_int]. [Cint.any] has no type variables, so the [poly] scheme
   installed by [initial_env] is sound for a mutable variable. *)
let errno_var = MVariable.create (MVariable.AnnotMut (GTy.mk Cint.any)) (Some "errno")

let defs = (tobool, tobool_t) :: (errno_var, Cint.any) :: build_vars parsed_types

module StrMap = Map.Make(String)
let defs_map = List.fold_left 
  (fun acc (v, _) -> 
    match Variable.get_name v with 
    | Some name -> StrMap.add name v acc
    | None -> failwith "Definition variable must have a name."
  ) StrMap.empty defs

let initial_env =
  let add_def env (v,ty) =
    Env.add v (GTy.mk ty |> TyScheme.mk_poly) env
  in
  List.fold_left add_def Env.empty defs
