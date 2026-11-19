(** Types of the C++ functions a package exports with cpp11 or Rcpp, from the
    glue these libraries generate, without reading the C++ code itself.

    cpp11 writes [src/cpp11.cpp], Rcpp [src/RcppExports.cpp]. For each exported
    function, they declare it and define the routine R calls, always in this
    form (cpp11):
    {v
    cpp11::list simplifyPieces(cpp11::list pieces, int p, bool fillLeft);
    extern "C" SEXP _tidyr_simplifyPieces(SEXP pieces, SEXP p, SEXP fillLeft) {
    v}
    or (Rcpp, with a [// name] comment before the declaration):
    {v
    int add(int a, int b);
    RcppExport SEXP _pkg_add(SEXP aSEXP, SEXP bSEXP) {
    v}
    With [// [[Rcpp::interfaces(cpp)]]], Rcpp puts a [static SEXP _pkg_add_try]
    between the declaration and [_pkg_add], which calls it: the declaration
    then gives the type of [_pkg_add].
    The routine converts each argument to the declared C++ type
    ([cpp11::as_cpp], [Rcpp::as]), which signals an R error when it cannot,
    and the result back to an R value ([cpp11::as_sexp], [Rcpp::wrap]). So the
    declared types give the type of the routine: its domain is what the
    conversions accept, its result what they produce. A type with no known
    conversion is [any] (e.g. [SEXP], or a class of the package). *)

type flavor = Cpp11 | Rcpp

type export = {
  c_name : string ;                (* the routine R calls *)
  cpp_name : string ;              (* the C++ function *)
  result : string ;                (* declared C++ types, as written *)
  params : (string * string) list ;  (* (C++ type, name) *)
  flavor : flavor ;
}

let glue_files = [ "cpp11.cpp", Cpp11 ; "RcppExports.cpp", Rcpp ]

(* [s] split at the commas outside of brackets ([<>], [()]) and of string
   or character literals (a default argument such as [sep = ","]). *)
let split_top s =
  let parts = ref [] and depth = ref 0 and start = ref 0 and quote = ref None in
  String.iteri (fun i c -> match !quote, c with
    | Some q, _ -> if c = q && (i = 0 || s.[i-1] <> '\\') then quote := None
    | None, ('"' | '\'') -> quote := Some c
    | None, ('<' | '(') -> incr depth
    | None, ('>' | ')') -> decr depth
    | None, ',' when !depth = 0 -> parts := String.sub s !start (i - !start) :: !parts ; start := i + 1
    | _ -> ()) s ;
  List.rev (String.sub s !start (String.length s - !start) :: !parts)
  |> List.map String.trim |> List.filter (( <> ) "")

(* A parameter [type name], possibly with a default ([= ...]). *)
let param p =
  let p = match String.index_opt p '=' with Some i -> String.trim (String.sub p 0 i) | None -> p in
  let re = Str.regexp {|^\(.*[^A-Za-z0-9_]\)\([A-Za-z_][A-Za-z0-9_]*\)$|} in
  if Str.string_match re p 0 then Some (String.trim (Str.matched_group 1 p), Str.matched_group 2 p)
  else None

let wrapper_re = Str.regexp {|^\(extern "C"\|RcppExport\|static\)[ \t]+SEXP[ \t]+\([A-Za-z_][A-Za-z0-9_.]*\)[ \t]*(|}
let decl_re = Str.regexp {|^\(.*[^A-Za-z0-9_]\)\([A-Za-z_][A-Za-z0-9_]*\)[ \t]*(\(.*\))[ \t]*;[ \t]*$|}

(* The exports declared in [text], the contents of a glue file. *)
let parse flavor text =
  let lines = String.split_on_char '\n' text |> Array.of_list in
  let rec decl_before i =
    if i < 0 then None
    else
      let l = String.trim lines.(i) in
      if l = "" || String.starts_with ~prefix:"//" l then decl_before (i - 1)
      else if Str.string_match decl_re l 0 then
        let result = String.trim (Str.matched_group 1 l) and cpp_name = Str.matched_group 2 l
        and params = Str.matched_group 3 l in
        let params = List.map param (split_top params) in
        if List.for_all Option.is_some params then Some (result, cpp_name, List.filter_map Fun.id params)
        else None
      else None
  in
  Array.to_list lines |> List.mapi (fun i l -> (i, l))
  |> List.filter_map (fun (i, l) ->
    if Str.string_match wrapper_re l 0 then
      let static = Str.matched_group 1 l = "static" and c_name = Str.matched_group 2 l in
      if static && not (String.ends_with ~suffix:"_try" c_name) then None else
      let c_name = if static then String.sub c_name 0 (String.length c_name - 4) else c_name in
      Option.map (fun (result, cpp_name, params) -> { c_name ; cpp_name ; result ; params ; flavor })
        (decl_before (i - 1))
    else None)

(* ===== From C++ types to R types ===== *)

(* [ty] without qualifiers, references and namespaces: [const cpp11::strings&]
   is [strings], [std::vector<std::string>] is [vector<string>]. *)
let normalize ty =
  let ty = Str.global_replace (Str.regexp {|\bconst\b\|&\|\bcpp11::\|\bRcpp::\|\bstd::\|\bwritable::\|\btraits::|}) "" ty in
  Str.global_replace (Str.regexp "[ \t]+") " " ty |> String.trim
  |> Str.global_replace (Str.regexp " ?\\([<>,*]\\) ?") "\\1"

let template ty =
  let re = Str.regexp {|^\([A-Za-z_]+\)<\(.*\)>$|} in
  if Str.string_match re ty 0 then Some (Str.matched_group 1 ty, Str.matched_group 2 ty) else None

let numeric_scalars = [ "int" ; "double" ; "float" ; "long" ; "unsigned" ; "unsigned int" ; "size_t" ;
                        "R_xlen_t" ; "R_len_t" ; "long long" ; "short" ]
let string_scalars = [ "string" ; "char*" ; "r_string" ; "String" ]

(* The R values the conversion of an argument to [ty] accepts. cpp11 checks
   the type of a vector exactly; its scalars accept a vector of length 1 that
   converts without loss. Rcpp converts like [as.vector] ([r_cast]): any
   atomic vector but a character one to a numeric or logical vector, any
   atomic vector to a character one. *)
let rec domain flavor ty =
  let ty = normalize ty in
  match flavor, ty, template ty with
  | _, _, Some ("Nullable", t) -> Printf.sprintf "(%s) | null" (domain flavor t)
  | Cpp11, _, Some ("vector", ("int" | "long" | "R_xlen_t")) -> "v(INT) | v(DBL)"
  | Cpp11, _, Some ("vector", ("double" | "float")) -> "v(DBL) | v(INT)"
  | Cpp11, _, Some ("vector", t) when List.mem t string_scalars -> "v(CHR)"
  | Cpp11, _, Some ("vector", "bool") -> "v(LGL)"
  | Cpp11, _, Some ("list_of", _) -> "list"
  | Cpp11, ("integers" | "integers_matrix"), _ -> "v(INT)"
  | Cpp11, ("doubles" | "doubles_matrix"), _ -> "v(DBL)"
  | Cpp11, ("strings" | "strings_matrix"), _ -> "v(CHR)"
  | Cpp11, ("logicals" | "logicals_matrix"), _ -> "v(LGL)"
  | Cpp11, "raws", _ -> "v(RAW)"
  | Cpp11, "complexes", _ -> "v(CLX)"
  | Cpp11, ("list" | "data_frame"), _ -> "list"
  | Cpp11, ("int" | "double" | "long" | "R_xlen_t"), _ -> "v1(INT) | v1(DBL) | v1(LGL)"
  | Cpp11, ("bool" | "r_bool"), _ -> "v1(LGL)"
  | Cpp11, t, _ when List.mem t string_scalars -> "v1(CHR)"
  | Rcpp, _, Some ("vector", t) when List.mem t string_scalars -> "v(chr) | sym"
  | Rcpp, _, Some ("vector", _) -> "v(clx) | v(RAW)"
  | Rcpp, ("NumericVector" | "IntegerVector" | "LogicalVector" | "ComplexVector" | "RawVector"
          | "NumericMatrix" | "IntegerMatrix" | "LogicalMatrix" | "ComplexMatrix" | "RawMatrix"), _ ->
    "v(clx) | v(RAW)"
  | Rcpp, ("CharacterVector" | "StringVector" | "CharacterMatrix" | "StringMatrix"), _ -> "v(chr) | sym"
  | Rcpp, ("bool" | "char"), _ -> "v1(clx) | v1(RAW)"
  | Rcpp, t, _ when List.mem t numeric_scalars -> "v1(clx) | v1(RAW)"
  | Rcpp, t, _ when List.mem t string_scalars -> "v1(CHR) | sym"
  | _, "Environment", _ | _, "environment", _ -> "env"
  | _ -> "any"

(* The R value the conversion of a result of type [ty] produces. *)
let rec result flavor ty =
  let ty = normalize ty in
  match ty, template ty with
  | "void", _ -> "null"
  | _, Some ("Nullable", t) -> Printf.sprintf "(%s) | null" (result flavor t)
  | _, Some ("vector", ("int" | "long" | "short")) -> "v(INT)"
  | _, Some ("vector", ("double" | "float")) -> "v(DBL)"
  | _, Some ("vector", "bool") -> "v(LGL)"
  | _, Some ("vector", t) when List.mem t string_scalars -> "v(CHR)"
  | _, Some ("list_of", _) -> "list"
  | ("integers" | "integers_matrix" | "IntegerVector" | "IntegerMatrix"), _ -> "v(INT)"
  | ("doubles" | "doubles_matrix" | "NumericVector" | "NumericMatrix"), _ -> "v(DBL)"
  | ("strings" | "strings_matrix" | "CharacterVector" | "StringVector"
    | "CharacterMatrix" | "StringMatrix"), _ -> "v(CHR)"
  | ("logicals" | "logicals_matrix" | "LogicalVector" | "LogicalMatrix"), _ -> "v(LGL)"
  | ("raws" | "RawVector"), _ -> "v(RAW)"
  | ("complexes" | "ComplexVector"), _ -> "v(CLX)"
  | ("list" | "data_frame" | "List" | "GenericVector" | "DataFrame"), _ -> "list"
  | ("int" | "long" | "short" | "unsigned" | "unsigned int"), _ -> "v1(INT)"
  | ("double" | "float"), _ -> "v1(DBL)"
  | ("bool" | "r_bool"), _ -> "v1(LGL)"
  | t, _ when List.mem t string_scalars -> "v1(CHR)"
  | ("Environment" | "environment"), _ -> "env"
  | _ -> "any"

(* The signature of [e] in the syntax of the [.ty] files. *)
let signature e =
  Printf.sprintf "[%s] --> %s"
    (String.concat ", " (List.map (fun (t, _) -> domain e.flavor t) e.params))
    (result e.flavor e.result)

let build str =
  let open Rstt in
  let _, ty = Builder.resolve Builder.empty_env (Rstt_repl.IO.parse_type str) in
  Builder.build Builder.TIdMap.empty ty

(* The exports of the package at [root] (its [src/] glue files), each with
   its type, or the reason it has none. *)
let exports root =
  let src = Filename.concat root "src" in
  glue_files |> List.concat_map (fun (file, flavor) ->
    let f = Filename.concat src file in
    if not (Sys.file_exists f) then []
    else parse flavor (In_channel.with_open_text f In_channel.input_all))

let types root =
  exports root |> List.map (fun e ->
    let s = signature e in
    (e, match build s with ty -> Ok ty | exception ex -> Error (s ^ ": " ^ Printexc.to_string ex)))

(* ===== Tests ===== *)

let%test "cpp11 glue" =
  let text = {|// melt.cpp
cpp11::list melt_dataframe(cpp11::data_frame data, const cpp11::integers& id_ind, cpp11::strings variable_name, cpp11::sexp attrTemplate, bool factorsAsStrings);
extern "C" SEXP _tidyr_melt_dataframe(SEXP data, SEXP id_ind, SEXP variable_name, SEXP attrTemplate, SEXP factorsAsStrings) {
  BEGIN_CPP11
// simplifyPieces.cpp
cpp11::list simplifyPieces(cpp11::list pieces, int p, bool fillLeft);
extern "C" SEXP _tidyr_simplifyPieces(SEXP pieces, SEXP p, SEXP fillLeft) {
|} in
  List.map (fun e -> e.c_name, signature e) (parse Cpp11 text)
  = [ "_tidyr_melt_dataframe", "[list, v(INT), v(CHR), any, v1(LGL)] --> list" ;
      "_tidyr_simplifyPieces", "[list, v1(INT) | v1(DBL) | v1(LGL), v1(LGL)] --> list" ]

let%test "Rcpp glue" =
  let text = {|// add
int add(int a, int b);
RcppExport SEXP _pkg_add(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
// names
std::vector<std::string> names(Rcpp::Nullable<Rcpp::CharacterVector> x, const std::string& sep = ",");
RcppExport SEXP _pkg_names(SEXP xSEXP, SEXP sepSEXP) {
// nothing
void nothing();
RcppExport SEXP _pkg_nothing() {
// seed
void seed(Rcpp::Nullable<Rcpp::IntegerVector> seed);
static SEXP _pkg_seed_try(SEXP seedSEXP) {
BEGIN_RCPP
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _pkg_seed(SEXP seedSEXP) {
|} in
  List.map (fun e -> e.c_name, signature e) (parse Rcpp text)
  = [ "_pkg_add", "[v1(clx) | v1(RAW), v1(clx) | v1(RAW)] --> v1(INT)" ;
      "_pkg_names", "[(v(chr) | sym) | null, v1(CHR) | sym] --> v(CHR)" ;
      "_pkg_nothing", "[] --> null" ;
      "_pkg_seed", "[(v(clx) | v(RAW)) | null] --> null" ]

let%test "the signatures build" =
  List.for_all (fun s -> match build s with _ -> true | exception _ -> false)
    [ "[list, v(INT), v(CHR), any, v1(LGL)] --> list" ;
      "[v1(clx) | v1(RAW), v1(clx) | v1(RAW)] --> v1(INT)" ;
      "[(v(chr) | sym) | null, v1(CHR) | sym] --> v(CHR)" ;
      "[] --> null" ; "[env] --> v(DBL)" ]
