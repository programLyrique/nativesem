(** Analyze a R package to get the list of entry points and their calling conventions.
We look through the R source code to find .C, .Call, .Fortran, and .External

.Call(PREFIX_funa_name, arg1, arg2, arg3)

Just with regexes, which is quite fragile.
We detect the native symbol prefix from the package root NAMESPACE file.
When `.fixes = "PREFIX_"` is present, the R variables exposed for registered
native routines are prefixed, while the underlying native symbol names remain
unprefixed.

useDynLib(libName, .registration = TRUE, .fixes = "PREFIX_")

TODO: use tree_sitter with the R grammar to extract the calls to these functions in a more robust way.
*)

(* Type to represent calling conventions *)
type calling_convention = C | Call | Fortran | External

let calling_convention_to_string = function
  | C -> "C"
  | Call -> "Call"
  | Fortran -> "Fortran"
  | External -> "External"

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

  (* Convert prefixed R variables back to the underlying native symbol name.
     TODO: stripping the [.fixes] prefix only handles one of two layers of
     R-to-C name remapping. The other layer is the [R_CallMethodDef] table
     in [src/init.c], whose entries can map an arbitrary R-visible string
     key to a different C function pointer, e.g.
       {"vctrs_split", (DL_FUNC) &vec_split, 2}.
     Calls of the form [.Call(vctrs_split, ...)] resolve to [vec_split]
     at runtime, but here we still report the entry point as [vctrs_split],
     which never matches a [Fundef] in the source — surfaced on the
     dashboard as "no record in functions.csv". To fix, parse the
     [R_CallMethodDef] (and [R_CMethodDef]/[R_FortranMethodDef]/
     [R_ExternalMethodDef]) initializers, build an R-name -> C-name map,
     and use the C name as the entry-point identifier. *)
  let strip_prefix prefix func_name =
    if prefix = "" then func_name
    else if String.starts_with ~prefix func_name then
      String.sub func_name (String.length prefix) (String.length func_name - String.length prefix)
    else
      func_name
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
              (* Create regex patterns for .C, .Call, .Fortran, .External.
                 TODO: the captured identifier is the R-side name as it
                 appears in [.Call("foo", ...)] / [.Call(foo, ...)]. For
                 packages that register entries via [R_CallMethodDef] with
                 a string key different from the C function name (e.g.
                 vctrs maps [vctrs_split] -> [&vec_split]), this name
                 won't match any [Fundef] in [src/]. Resolving via the
                 [R_CallMethodDef] table (see [strip_prefix]'s TODO) and
                 emitting the C-side name here would close that gap. *)
              let extract_calls pattern_name convention =
                let pattern = Str.regexp
                  ("\\." ^ pattern_name ^ "[ \t]*([ \t]*\"?\\([a-zA-Z0-9_]+\\)")
                in
                let rec find_all pos acc =
                  try
                    ignore (Str.search_forward pattern content pos);
                    let func_name = Str.matched_group 1 content in
                    let next_pos = Str.match_end () in
                    let native_name = strip_prefix prefix func_name in
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
