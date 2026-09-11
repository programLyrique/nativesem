(** This is to type normal C functions, and functions that use the .C convention.
See https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Interface-functions-_002eC-and-_002eFortran 

We directly type from PAst. We only need to look at the parameters and return type.
*)

open Mlsem.Types
open PAst


let is_variadic_params params = List.exists (function Vararg -> true | _ -> false) params


(* Is it a C function with no SEXP type as parameter or return type?
And with simple types. If there are structs, we still want to infer it with its body
because the struct could contain SEXPs...*)
let is_simple_c_function past =
  match past with
  | Fundef(ret_ty, _, params, _) ->
    let ret = match ret_ty with
      | Ast.SEXP -> false
      | Ast.Struct _ -> false
      | Ast.FunPtr _ -> false
      | _ -> true
    in
    let params = List.for_all (function
        | Vararg -> true
        | Param (ty, _) ->
            match ty with
            | Ast.SEXP -> false
            | Ast.Struct _ -> false
            | Ast.FunPtr _ -> false
            | _ -> true) params
  in
   ret && params
  | _ -> false

       

(* [typedef_map] carries the accumulated [TypeDecl]s so that typedef-aliased
   parameter/return types (e.g. [r_obj* = struct SEXPREC*]) are resolved to
   their canonical form before being translated to Sstt. Without it,
   [Typeref "r_obj"] degrades to [Ty.any] and a [SEXP] signature becomes
   [*any -> *any]. The result is a bare arrow (no [Attr] wrap): a C function
   is not an R closure, and [Ast.build_call]'s [pfun] projection accepts bare
   arrows directly. *)
let infer_cfun ?(typedef_map = Ast.DeclMap.empty) return_ty params =
  let resolve = PAst.resolve_ctype typedef_map in
  let ret_ty = Ast.typeof_ctype (resolve return_ty) in
  (* Variadic functions ([...]) are typed as [any -> ret]: any caller-side
     argument tuple is a subtype of [any], so all call shapes type-check.
     Imprecise on the fixed-arg side but it lets variadic libc/R API
     functions (printf, Rprintf, fprintf, snprintf, fcntl, …) be reachable
     instead of being silently dropped. *)
  let arg_ty =
    if is_variadic_params params then Ty.any
    else
      let arg_types =
        List.filter_map (function
          | Param (ty, _) -> Some (Ast.typeof_ctype (resolve ty))
          | Vararg -> None) params
      in
      Tuple.mk arg_types
  in
  Arrow.mk arg_ty ret_ty


let dotC_typeof typedef_map ct =
  let rec resolve ty =
    match ty with
    | Ast.Typeref name ->
        (match Ast.DeclMap.find_opt name typedef_map with
         | Some t -> resolve t
         | None -> ty)
    | Ast.Ptr inner -> Ast.Ptr (resolve inner)
    | _ -> ty
  in
  let ct = resolve ct in
  let open Ast in
  let open Rstt in
  match ct with
  (* TODO: add also Rcomplex* *)
  | Ptr Int -> Ty.cup (Vec.Vector (Prim.mk Prim.Int.any) |> Vec.mk |> Attr.mk_content)
    (Vec.Vector (Prim.mk Prim.Lgl.any) |> Vec.mk |> Attr.mk_content) (* We allow logical vectors as well since they can be coerced to int *)
  | Ptr Float -> Vec.Vector (Prim.mk Prim.Dbl.any) |> Vec.mk |> Attr.mk_content
  | Ptr Ptr Char -> Vec.Vector (Prim.mk Prim.Chr.any) |> Vec.mk |> Attr.mk_content
  | Ptr Char -> Vec.Vector (Prim.mk Prim.Raw.any) |> Vec.mk |> Attr.mk_content (* Actually unsigned char* *)
  | _ -> failwith (Printf.sprintf "Unsupported type for .C interface: %s. Only init*, double*, char** and unsigned char* are supported." (Ast.show_ctype ct))


let infer_dotC ?(typedef_map = Ast.DeclMap.empty) ret_ty params =
  if ret_ty != Ast.Void then
    failwith "Return type of .C functions must be void.";
  if is_variadic_params params then
    failwith "Variadic parameters are not supported in the .C interface.";
  let arg_types =
    List.filter_map (function
      | Param (ty, _) -> Some (dotC_typeof typedef_map ty)
      | Vararg -> None) params
  in
  let arg_tuple = Tuple.mk arg_types in
  (* Bare arrow, as in [infer_cfun] — see comment there. *)
  Arrow.mk arg_tuple Rstt.Cenums.void


let infer_dotC_from_past ?(typedef_map = Ast.DeclMap.empty) = function
  | _, Fundef (ret_ty, _, params, _) -> infer_dotC ~typedef_map ret_ty params
  | _ -> failwith "Expected a function definition."


let mk_test_fundef ?(ret_ty=Ast.Void) ?(params=[]) name =
  let pos = Mlsem.Common.Position.dummy in
  let body = (pos, Seq []) in
  (pos, Fundef (ret_ty, name, params, body))


let infer_dotC_failure past =
  try
    ignore (infer_dotC_from_past past);
    None
  with Failure msg ->
    Some msg


let mk_simple_test_fundef ?(ret_ty=Ast.Void) ?(params=[]) name =
  let _, past = mk_test_fundef ~ret_ty ~params name in
  past


let%test "infer_dotC infers supported .C parameters from a PAst function" =
  let past =
    mk_test_fundef
      ~params:
        [ Param (Ast.Ptr Ast.Int, "ints");
          Param (Ast.Ptr Ast.Float, "dbls");
          Param (Ast.Ptr (Ast.Ptr Ast.Char), "strings");
          Param (Ast.Ptr Ast.Char, "bytes") ]
      "native_entry"
  in
  let open Rstt in
  let expected =
    Arrow.mk
      (Tuple.mk
         [ Ty.cup
             (Vec.Vector (Prim.mk Prim.Int.any) |> Vec.mk |> Attr.mk_content)
             (Vec.Vector (Prim.mk Prim.Lgl.any) |> Vec.mk |> Attr.mk_content);
           Vec.Vector (Prim.mk Prim.Dbl.any) |> Vec.mk |> Attr.mk_content;
           Vec.Vector (Prim.mk Prim.Chr.any) |> Vec.mk |> Attr.mk_content;
           Vec.Vector (Prim.mk Prim.Raw.any) |> Vec.mk |> Attr.mk_content ])
      Cenums.void
  in
  Ty.equiv (infer_dotC_from_past past) expected


let%test "infer_dotC rejects non-void return type from a PAst function" =
  let past =
    mk_test_fundef ~ret_ty:Ast.Int ~params:[Param (Ast.Ptr Ast.Int, "ints")] "native_entry"
  in
  infer_dotC_failure past = Some "Return type of .C functions must be void."


let%test "infer_dotC rejects unsupported .C parameter types from a PAst function" =
  let past =
    mk_test_fundef ~params:[Param (Ast.Int, "value")] "native_entry"
  in
  let expected =
    Printf.sprintf
      "Unsupported type for .C interface: %s. Only init*, double*, char** and unsigned char* are supported."
      (Ast.show_ctype Ast.Int)
  in
  infer_dotC_failure past = Some expected


let%test "is_simple_c_function accepts scalar pointer-only C entry points" =
  is_simple_c_function
    (mk_simple_test_fundef
       ~ret_ty:Ast.Void
       ~params:[Param (Ast.Ptr Ast.Int, "x"); Param (Ast.Float, "y")]
       "native_entry")


let%test "is_simple_c_function accepts variadic functions when fixed args are simple" =
  is_simple_c_function
    (mk_simple_test_fundef
       ~ret_ty:Ast.Int
       ~params:[Param (Ast.Ptr Ast.Char, "fmt"); Vararg]
       "my_printf")


let%test "is_simple_c_function rejects SEXP structs and non-functions" =
  not
    (is_simple_c_function
     (mk_simple_test_fundef ~ret_ty:Ast.SEXP ~params:[Param (Ast.Ptr Ast.Int, "x")] "sexp_entry"))
  && not
       (is_simple_c_function
       (mk_simple_test_fundef
             ~ret_ty:Ast.Void
             ~params:[Param (Ast.Struct ("payload", [Ast.Int, "field"]), "x")]
             "struct_entry"))
  && not (is_simple_c_function (GlobalVar (Extern, "g", Ast.Int)))


let%test "infer_cfun resolves typedef aliases before building the function type" =
  let typedef_map =
    Ast.DeclMap.empty
    |> Ast.DeclMap.add "sexp_t" Ast.SEXP
    |> Ast.DeclMap.add "int_ptr_t" (Ast.Ptr Ast.Int)
  in
  Ty.equiv
    (infer_cfun ~typedef_map (Ast.Typeref "sexp_t") [Param (Ast.Typeref "int_ptr_t", "arg")])
    (infer_cfun Ast.SEXP [Param (Ast.Ptr Ast.Int, "arg")])


let%test "infer_cfun types variadic functions as any -> ret" =
  let actual =
    infer_cfun Ast.Int [Param (Ast.Ptr Ast.Char, "fmt"); Vararg]
  in
  let expected =
    Arrow.mk Ty.any (Ast.typeof_ctype Ast.Int)
  in
  Ty.equiv actual expected


let%test "infer_dotC rejects variadic functions" =
  let past =
    mk_test_fundef ~params:[Param (Ast.Ptr Ast.Int, "ints"); Vararg] "native_entry"
  in
  infer_dotC_failure past
    = Some "Variadic parameters are not supported in the .C interface."


let%test "infer_dotC resolves typedef aliases for supported pointer arguments" =
  let typedef_map =
    Ast.DeclMap.empty
    |> Ast.DeclMap.add "int_ptr_t" (Ast.Ptr Ast.Int)
    |> Ast.DeclMap.add "char_ptr_ptr_t" (Ast.Ptr (Ast.Ptr Ast.Char))
  in
  Ty.equiv
    (infer_dotC ~typedef_map Ast.Void
       [Param (Ast.Typeref "int_ptr_t", "ints"); Param (Ast.Typeref "char_ptr_ptr_t", "names")])
    (infer_dotC Ast.Void [Param (Ast.Ptr Ast.Int, "ints"); Param (Ast.Ptr (Ast.Ptr Ast.Char), "names")])


let%test "infer_dotC_from_past rejects non-function top-level items" =
  match infer_dotC_failure (Mlsem.Common.Position.dummy, GlobalVar (Extern, "g", Ast.Int)) with
  | Some "Expected a function definition." -> true
  | _ -> false
