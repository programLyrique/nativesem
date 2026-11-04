(** Analyze a R package to get the list of entry points and their calling conventions.
We look through the R source code to find .C, .Call, .Fortran, and .External

.Call(PREFIX_funa_name, arg1, arg2, arg3)

Just with regexes, which is quite fragile.
We detect the native symbol prefix from the package root NAMESPACE file.
When `.fixes = "PREFIX_"` is present, the R variables exposed for registered
native routines are prefixed, while the underlying native symbol names remain
unprefixed.

useDynLib(libName, .registration = TRUE, .fixes = "PREFIX_")

The R-visible name of a registered routine is mapped to the C function that
implements it through the registration tables (see {!registered_routines}).

TODO: use tree_sitter with the R grammar to extract the calls to these functions in a more robust way.
*)

(* Type to represent calling conventions *)
type calling_convention = C | Call | Fortran | External

let calling_convention_to_string = function
  | C -> "C"
  | Call -> "Call"
  | Fortran -> "Fortran"
  | External -> "External"

(** {1 Registration tables}

    A package registers its native routines with [R_registerRoutines], from
    arrays of [R_CMethodDef], [R_CallMethodDef], [R_FortranMethodDef] and
    [R_ExternalMethodDef]:
{v
static R_NativePrimitiveArgType aind_t[] = {INTSXP, INTSXP, INTSXP, INTSXP, LGLSXP};
static const R_FortranMethodDef FortEntries[] = {
  {"aind", (DL_FUNC) &F77_SUB(aind), 5, aind_t},
  {NULL, NULL, 0}
};
v}
    An entry gives the R-visible name, the function, its number of arguments
    and, for [.C] and [.Fortran], optionally the R types of its arguments,
    which R then checks at each call. The tables are read from the source
    text, after expanding the function-like macros that commonly build their
    entries ([#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}]). *)

(** An [R_NativePrimitiveArgType], i.e. a [SEXPTYPE]. *)
type native_arg_type =
  | Real | Integer | Logical | String | Raw | Complex
  | Any        (** [ANYSXP]: no check *)
  | Other of string

type registered = {
  r_name : string ;      (** without the [.fixes] prefix *)
  c_name : string ;      (** the symbol: [x_] for [F77_SUB(x)] *)
  convention : calling_convention ;
  arity : int option ;   (** [None] for a variable number ([-1]) *)
  arg_types : native_arg_type list option ;
}

let native_arg_type_of_string = function
  | "REALSXP" -> Real | "INTSXP" -> Integer | "LGLSXP" -> Logical
  | "STRSXP" -> String | "RAWSXP" -> Raw | "CPLXSXP" -> Complex
  | "ANYSXP" -> Any | s -> Other s

let string_of_native_arg_type = function
  | Real -> "REALSXP" | Integer -> "INTSXP" | Logical -> "LGLSXP"
  | String -> "STRSXP" | Raw -> "RAWSXP" | Complex -> "CPLXSXP"
  | Any -> "ANYSXP" | Other s -> s

(* The source text without its comments, and with its continued lines
   joined. String and character literals are kept as they are. *)
let strip_comments src =
  let n = String.length src in
  let b = Buffer.create n in
  let rec code i =
    if i >= n then ()
    else match src.[i] with
      | '/' when i + 1 < n && src.[i + 1] = '*' -> block (i + 2)
      | '/' when i + 1 < n && src.[i + 1] = '/' -> line (i + 2)
      | '\\' when i + 1 < n && src.[i + 1] = '\n' -> Buffer.add_char b ' ' ; code (i + 2)
      | ('"' | '\'') as q -> Buffer.add_char b q ; literal q (i + 1)
      | c -> Buffer.add_char b c ; code (i + 1)
  and block i =
    if i + 1 >= n then ()
    else if src.[i] = '*' && src.[i + 1] = '/' then (Buffer.add_char b ' ' ; code (i + 2))
    else block (i + 1)
  and line i =
    if i >= n then () else if src.[i] = '\n' then code i else line (i + 1)
  and literal q i =
    if i >= n then ()
    else begin
      Buffer.add_char b src.[i] ;
      if src.[i] = '\\' && i + 1 < n then (Buffer.add_char b src.[i + 1] ; literal q (i + 2))
      else if src.[i] = q then code (i + 1)
      else literal q (i + 1)
    end
  in
  code 0 ; Buffer.contents b

(* [s] split at the commas outside of any parentheses, braces or brackets,
   each part trimmed. *)
let split_top_level s =
  let parts = ref [] and depth = ref 0 and start = ref 0 in
  String.iteri (fun i c ->
    match c with
    | '(' | '{' | '[' -> incr depth
    | ')' | '}' | ']' -> decr depth
    | ',' when !depth = 0 ->
      parts := String.sub s !start (i - !start) :: !parts ; start := i + 1
    | _ -> ()) s ;
  parts := String.sub s !start (String.length s - !start) :: !parts ;
  List.rev_map String.trim !parts |> List.filter ((<>) "")

(* The text between the brace at [i] and its matching closing brace. *)
let braced s i =
  let rec go j depth =
    if j >= String.length s then None
    else match s.[j] with
      | '{' -> go (j + 1) (depth + 1)
      | '}' when depth = 1 -> Some (String.sub s (i + 1) (j - i - 1))
      | '}' -> go (j + 1) (depth - 1)
      | _ -> go (j + 1) depth
  in
  go i 0

let is_ident_char c =
  match c with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false

(* All the matches of [re] in [s], as their groups [1..ngroups] and the
   position of their end. *)
let all_matches re ngroups s =
  let rec go pos acc =
    match Str.search_forward re s pos with
    | exception Not_found -> List.rev acc
    | _ ->
      let groups = List.init ngroups (fun g -> Str.matched_group (g + 1) s) in
      let e = Str.match_end () in
      go (max e (pos + 1)) ((groups, e) :: acc)
  in
  go 0 []

(* Function-like macros, as name -> (parameters, body). *)
let function_macros src =
  let re = Str.regexp
      "^[ \t]*#[ \t]*define[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)(\\([^)]*\\))\\([^\n]*\\)" in
  all_matches re 3 src
  |> List.map (function
      | [name ; params ; body], _ ->
        (name, (String.split_on_char ',' params |> List.map String.trim, String.trim body))
      | _ -> assert false)

(* [body] with its parameters replaced by [args], stringified by [#] and
   pasted by [##]. *)
let expand_macro (params, body) args =
  let arg p = List.assoc_opt p (List.combine params args) in
  let n = String.length body in
  let b = Buffer.create n in
  let rec go i =
    if i >= n then ()
    else if body.[i] = '#' && i + 1 < n && body.[i + 1] = '#' then begin
      (* Pasting: drop the blanks around [##]. *)
      let len = Buffer.length b in
      let rec trim k = if k > 0 && (Buffer.nth b (k - 1) = ' ' || Buffer.nth b (k - 1) = '\t') then trim (k - 1) else k in
      Buffer.truncate b (trim len) ;
      let rec skip j = if j < n && (body.[j] = ' ' || body.[j] = '\t') then skip (j + 1) else j in
      go (skip (i + 2))
    end
    else if body.[i] = '#' then begin
      let rec skip j = if j < n && (body.[j] = ' ' || body.[j] = '\t') then skip (j + 1) else j in
      let j = skip (i + 1) in
      let rec ident k = if k < n && is_ident_char body.[k] then ident (k + 1) else k in
      let k = ident j in
      let id = String.sub body j (k - j) in
      Buffer.add_string b ("\"" ^ Option.value ~default:id (arg id) ^ "\"") ;
      go k
    end
    else if is_ident_char body.[i] then begin
      let rec ident k = if k < n && is_ident_char body.[k] then ident (k + 1) else k in
      let k = ident i in
      let id = String.sub body i (k - i) in
      Buffer.add_string b (Option.value ~default:id (arg id)) ;
      go k
    end
    else (Buffer.add_char b body.[i] ; go (i + 1))
  in
  go 0 ; Buffer.contents b

let table_conventions =
  [ "R_CMethodDef", C ; "R_CallMethodDef", Call ;
    "R_FortranMethodDef", Fortran ; "R_ExternalMethodDef", External ]

(* The symbol a [DL_FUNC] expression designates. *)
let symbol_of_fun e =
  let re = Str.regexp
      "^\\((DL_FUNC)\\)?[ \t]*&?[ \t]*\\(\\(F77_SUB\\|F77_NAME\\|F77_CALL\\)[ \t]*([ \t]*\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)" in
  if Str.string_match re (String.trim e) 0 then
    let name = Str.matched_group 4 e |> String.trim in
    let fortran = try ignore (Str.matched_group 2 e) ; true with Not_found -> false in
    Some (if fortran then name ^ "_" else name)
  else None

(** The routines the tables of the C source [src] register. *)
let parse_registrations src =
  let src = strip_comments src in
  let macros = function_macros src in
  let types_re = Str.regexp
      "R_NativePrimitiveArgType[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*\\[[^]]*\\][ \t\n]*=[ \t\n]*{\\([^}]*\\)}" in
  let types =
    all_matches types_re 2 src
    |> List.map (function
        | [name ; elts], _ -> (name, split_top_level elts |> List.map native_arg_type_of_string)
        | _ -> assert false) in
  let table_re = Str.regexp
      "\\(R_CMethodDef\\|R_CallMethodDef\\|R_FortranMethodDef\\|R_ExternalMethodDef\\)[ \t\n]+[A-Za-z_][A-Za-z0-9_]*[ \t\n]*\\[[^]]*\\][ \t\n]*=[ \t\n]*{" in
  (* An entry, its macros expanded, as its fields. *)
  let rec fields depth e =
    let e = String.trim e in
    if String.length e > 0 && e.[0] = '{' then
      Option.map split_top_level (braced e 0)
    else if depth > 8 then None
    else
      let re = Str.regexp "\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*(" in
      if Str.string_match re e 0 then
        match List.assoc_opt (Str.matched_group 1 e) macros with
        | Some (params, _ as m) ->
          let open_paren = Str.match_end () - 1 in
          let close = String.rindex e ')' in
          let args = split_top_level (String.sub e (open_paren + 1) (close - open_paren - 1)) in
          if List.length args <> List.length params then None
          else fields (depth + 1) (expand_macro m args)
        | None -> None
      else None
  in
  let entry convention e =
    match fields 0 e with
    | Some (name :: f :: rest) when String.length name >= 2 && name.[0] = '"' ->
      let r_name = String.sub name 1 (String.length name - 2) in
      let arg_types = match rest with
        | [_ ; t] -> List.assoc_opt (String.trim t) types
        | _ -> None in
      let arity = match rest with
        | n :: _ -> (match int_of_string_opt (String.trim n) with
            | Some n when n >= 0 -> Some n
            | Some _ -> None
            | None -> Option.map List.length arg_types)
        | [] -> None in
      Option.map (fun c_name -> { r_name ; c_name ; convention ; arity ; arg_types })
        (symbol_of_fun f)
    | _ -> None
  in
  all_matches table_re 1 src
  |> List.concat_map (function
      | [kind], e ->
        let convention = List.assoc kind table_conventions in
        (match braced src (e - 1) with
         | Some body -> split_top_level body |> List.filter_map (entry convention)
         | None -> [])
      | _ -> assert false)

let%test "parse_registrations reads Fortran tables with types arrays" =
  parse_registrations {|
static R_NativePrimitiveArgType aind_t[] = {INTSXP, /* n */ INTSXP, LGLSXP};
static const R_FortranMethodDef FortEntries[] = {
  {"aind", (DL_FUNC) &F77_SUB(aind), 3, aind_t},
  {"cp",     (DL_FUNC) &F77_SUB(cp),     13},
  {NULL, NULL, 0}
};|}
  = [ { r_name = "aind" ; c_name = "aind_" ; convention = Fortran ; arity = Some 3 ;
        arg_types = Some [Integer ; Integer ; Logical] } ;
      { r_name = "cp" ; c_name = "cp_" ; convention = Fortran ; arity = Some 13 ;
        arg_types = None } ]

let%test "parse_registrations expands the macros building the entries" =
  parse_registrations {|
#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}
#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}
static R_NativePrimitiveArgType solve_t[] = { REALSXP, INTSXP };
static const R_CMethodDef cMethods[] = { CDEF(solve), {NULL, NULL, 0} };
static const R_CallMethodDef callMethods[] = {
  CALLDEF(make_pair, 2),
  {"vctrs_split", (DL_FUNC) &vec_split, -1},
  {NULL, NULL, 0}
};|}
  = [ { r_name = "solve" ; c_name = "solve" ; convention = C ; arity = Some 2 ;
        arg_types = Some [Real ; Integer] } ;
      { r_name = "make_pair" ; c_name = "make_pair" ; convention = Call ; arity = Some 2 ;
        arg_types = None } ;
      { r_name = "vctrs_split" ; c_name = "vec_split" ; convention = Call ; arity = None ;
        arg_types = None } ]

let windows_only_dirs = ["win32"; "win64"; "windows"]

let get_c_files path =
  let src_dir = Filename.concat path "src" in
  if not (Sys.file_exists src_dir && Sys.is_directory src_dir) then []
  else
    (* Packages commonly vendor helper libraries in subdirectories of [src/]
       (e.g. vctrs has [src/rlang/], utf8 has [src/utf8lite/src/]). Their
       [.h] files contain [#define]s used by the top-level sources, so we
       need to collect the full transitive closure. *)
    let rec walk dir acc =
      try
        let entries = Sys.readdir dir in
        Array.fold_left (fun acc entry ->
          let entry_path = Filename.concat dir entry in
          if Sys.is_directory entry_path then
            (* Windows-only code (askpass's [src/win32/], sys's
               [src/win32/exec.c]) is only built on Windows, by Makevars.win;
               analysing it on Linux reports Windows APIs as unbound. *)
            (if List.mem (String.lowercase_ascii entry) windows_only_dirs then acc
             else walk entry_path acc)
          else if Filename.check_suffix entry ".c" || Filename.check_suffix entry ".h" then
            entry_path :: acc
          else acc
        ) acc entries
      with _ -> acc
    in
    let all_files = walk src_dir [] in
    (* Parse headers before source files so that [#define]s are registered
       in [Parser.define_constants] before any [.c] file that references
       them (e.g. [#define R_PRI_SSIZE "d"] in [rlang-types.h] must be
       known before [arg.c] uses [`"%" R_PRI_SSIZE "\n"`]). *)
    let headers, sources =
      List.partition (fun f -> Filename.check_suffix f ".h") all_files
    in
    headers @ sources

(** The routines registered by the C sources of the package at [path]. *)
let registered_routines path =
  get_c_files path
  |> List.filter (fun f -> Filename.check_suffix f ".c")
  |> List.concat_map (fun f ->
      try parse_registrations (In_channel.with_open_bin f In_channel.input_all)
      with Sys_error _ -> [])

let find_native_calls path =
  (* Extract prefix from the package root NAMESPACE file. *)
  let find_prefix () =
    let namespace_file = Filename.concat path "NAMESPACE" in
    if not (Sys.file_exists namespace_file) then ""
    else
      try
        let lines = In_channel.with_open_text namespace_file In_channel.input_lines in
        let content = String.concat "\n" lines in
        let pattern = Str.regexp "useDynLib.*\\.fixes[ \t]*=[ \t]*\"\\([^\"]+\\)\"" in
        try
          ignore (Str.search_forward pattern content 0);
          Str.matched_group 1 content
        with Not_found -> ""
      with _ -> ""
  in

  (* Convert prefixed R variables back to the underlying native symbol name:
     strip the [.fixes] prefix, then follow the registration tables, whose
     entries can map an R-visible name to a differently named C function,
     e.g. [{"vctrs_split", (DL_FUNC) &vec_split, 2}]. *)
  let registered = registered_routines path in
  let strip_prefix prefix convention func_name =
    let func_name =
      if prefix = "" then func_name
      else if String.starts_with ~prefix func_name then
        String.sub func_name (String.length prefix) (String.length func_name - String.length prefix)
      else
        func_name
    in
    match List.find_opt (fun r -> r.r_name = func_name && r.convention = convention) registered with
    | Some r -> r.c_name
    | None -> func_name
  in

  (* Find native calls in R source files *)
  let find_calls prefix =
    let r_dir = Filename.concat path "R" in
    if not (Sys.file_exists r_dir && Sys.is_directory r_dir) then []
    else
      try
        let files = Sys.readdir r_dir in
        let calls = ref [] in
        Array.iter (fun filename ->
          let filepath = Filename.concat r_dir filename in
          if not (Sys.is_directory filepath) then
            (try
              let lines = In_channel.with_open_text filepath In_channel.input_lines in
              let content = String.concat "\n" lines in
              (* Create regex patterns for .C, .Call, .Fortran, .External. *)
              let extract_calls pattern_name convention =
                let pattern = Str.regexp
                  ("\\." ^ pattern_name ^ "[ \t]*([ \t]*\"?\\([a-zA-Z0-9_]+\\)")
                in
                let rec find_all pos acc =
                  try
                    ignore (Str.search_forward pattern content pos);
                    let func_name = Str.matched_group 1 content in
                    let next_pos = Str.match_end () in
                    let native_name = strip_prefix prefix convention func_name in
                    find_all next_pos ((native_name, convention) :: acc)
                  with Not_found -> List.rev acc
                in
                find_all 0 []
              in
              let c_calls = extract_calls "C" C in
              let call_calls = extract_calls "Call" Call in
              let fortran_calls = extract_calls "Fortran" Fortran in
              let external_calls = extract_calls "External" External in
              calls := !calls @ c_calls @ call_calls @ fortran_calls @ external_calls
            with _ -> ())
        ) files;
        !calls
      with _ -> []
  in

  let prefix = find_prefix () in
  find_calls prefix

let%test "get_c_files skips Windows-only subdirectories" =
  let root = Filename.temp_dir "rctyping-pkg" "" in
  let touch path = close_out (open_out path) in
  let src = Filename.concat root "src" in
  Sys.mkdir src 0o755;
  List.iter (fun d -> Sys.mkdir (Filename.concat src d) 0o755) ["win32"; "Windows"; "vendor"];
  List.iter touch [ Filename.concat src "a.c";
                    Filename.concat (Filename.concat src "win32") "w.c";
                    Filename.concat (Filename.concat src "Windows") "x.c";
                    Filename.concat (Filename.concat src "vendor") "v.c" ];
  get_c_files root |> List.map Filename.basename |> List.sort compare = ["a.c"; "v.c"]
