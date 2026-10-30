open R_c_typing
open Cmdliner
open Mlsem.Types
module System = Mlsem.System


let positive_float =
  let parse value =
    match float_of_string_opt value with
    | Some seconds when seconds > 0. -> Ok seconds
    | Some _ -> Error (`Msg "timeout must be strictly positive")
    | None -> Error (`Msg (Printf.sprintf "invalid float: %s" value))
  in
  Arg.conv (parse, Format.pp_print_float)

let main ~mlsem_recording opts include_dirs path =
  System.Config.infer_overload := true;
  if mlsem_recording then
    Mlsem.Types.Recording.start_recording ();
  if opts.Runner.log_times then
    Format.printf "Phase: load_ty %.3f s@." !Defs.ty_load_time;
  let env_dirs = match Sys.getenv_opt "C_INCLUDE_PATH" with
    | None | Some "" -> []
    | Some s -> String.split_on_char ':' s |> List.filter (fun x -> x <> "")
  in
  (* Ask the system compiler where it looks for headers. On Debian this
     picks up /usr/include/x86_64-linux-gnu (multiarch), where most libs'
     headers now actually live. Falls back to the hardcoded list if no
     compiler is available. *)
  let gcc_dirs = R_c_typing.Utils.detect_gcc_include_dirs () in
  R_c_typing.Parser.set_include_dirs
    (include_dirs @ env_dirs @ gcc_dirs @ R_c_typing.Parser.default_include_dirs);
  let idenv = Runner.Idenv.empty in
  let env = Defs.initial_env in
  if not (Sys.file_exists path) then
    failwith (Printf.sprintf "Path not found: %s" path);
  let is_package = Sys.is_directory path in
  Printf.printf "Typing %s: %s\n"
    (if is_package then "package" else "file")
    path;
  if is_package then
    Runner.run_on_package opts path idenv env |> ignore
  else
    Runner.run_on_file opts path idenv env |> ignore;
  if mlsem_recording then begin
    let t_rec = Unix.gettimeofday () in
    Mlsem.Types.Recording.save_to_file "mlsem_recording.json"
      (Mlsem.Types.Recording.tally_calls ());
    if opts.Runner.log_times then
      Format.printf "Phase: recording_save %.3f s@."
        (Unix.gettimeofday () -. t_rec)
  end;
  ()

let cst_opt =
  let doc = "Print CST (concrete syntax tree)" in
  Arg.(value & flag & info ["cst"] ~doc)

let past_opt =
  let doc = "Print parsed AST" in
  Arg.(value & flag & info ["past"] ~doc)

let ast_opt =
  let doc = "Print AST (typed abstract syntax tree)" in
  Arg.(value & flag & info ["ast"] ~doc)

let mlsem_opt = 
  let doc = "Print MLsem AST" in
  Arg.(value & flag & info ["mlsem"] ~doc)

let no_typing_opt =
  let doc = "Disable type inference" in
  Arg.(value & flag & info ["no-typing"] ~doc)

let debug_opt = 
  let doc = "Enable debug mode" in
  Arg.(value & flag & info ["debug"] ~doc)

let filter_opt =
  let doc = "Filter output to only *show* variables matching the given substring" in
  Arg.(value & opt (some string) None & info ["f";"filter"] ~docv:"SUBSTRING" ~doc)

let include_dir_opt =
  let doc = "Add a system include search directory (repeatable). Also honors C_INCLUDE_PATH env var." in
  Arg.(value & opt_all string [] & info ["I"; "include-dir"] ~docv:"DIR" ~doc)

let timeout_opt =
  let doc = "Set a per-function timeout in seconds for full function body inference/checking (default: no timeout)." in
  Arg.(value & opt (some positive_float) None & info ["timeout"] ~docv:"SECONDS" ~doc)

let fallback_c_sig_opt =
  let doc =
    "When full-body inference fails (untypeable, internal error, or timeout), \
     bind the function at its declared C signature so callers can still be \
     typed instead of cascading 'unbound variable' errors. The original error \
     is still printed, followed by a 'fallback: <type>' line."
  in
  Arg.(value & flag & info ["fallback-c-signature"] ~doc)

let fallback_mode_opt =
  let doc =
    "Which functions $(b,--fallback-c-signature) applies to: $(b,all) of them \
     (the default), $(b,except-all-sexp) (not those whose parameters and \
     result are all SEXP: their signature only says that they take and return \
     R values), or $(b,none). Implies $(b,--fallback-c-signature), but for \
     $(b,none)."
  in
  let modes = [ "all", Some Runner.All ; "except-all-sexp", Some Runner.Except_all_sexp ;
                "none", None ] in
  Arg.(value & opt (some (enum modes)) None & info ["fallback-mode"] ~docv:"MODE" ~doc)

let call_graph_opt =
  let doc =
    "Write the call graph (after entry-point reachability filtering) in \
     Graphviz .dot format to FILE. Non-trivial SCCs are colored, nodes are \
     clustered by source file, and entry points are marked with \
     [shape=doublecircle]. Render with [dot -Tsvg FILE -o FILE.svg]."
  in
  Arg.(value & opt (some string) None & info ["call-graph"] ~docv:"FILE" ~doc)

let mlsem_recording_opt =
  let doc =
    "Bracket the run with \
     [Mlsem.Types.Recording.start_recording] / [save_to_file], dumping a \
     per-process call tally to [mlsem_recording.json] in the current \
     directory. The dump can be several tens of MB and add a few seconds \
     per package, so it is off by default — including for the CRAN \
     pipeline. Turn on to inspect mlsem-side call statistics."
  in
  Arg.(value & flag & info ["mlsem-recording"] ~doc)

let lambda_rec_opt =
  let doc =
    "Also type each recursive function as a recursive group (MLsem's \
     LambdaRec), giving that inference at most $(docv) seconds, and intersect \
     the result with the type obtained against its declared C signature. \
     Requires --fallback-c-signature. Off by default." in
  Arg.(value & opt (some float) None & info ["lambda-rec"] ~docv:"SECONDS" ~doc)

let log_times_opt =
  let doc =
    "Emit timing diagnostics. Two kinds of lines: (1) a '  timing: <seconds> \
     s' line below every function's result giving the wall-clock time of \
     its body inference (indented to match the existing 'fallback:' \
     convention so per-function CSV parsing is unaffected); (2) coarse \
     'Phase: <name> <seconds> s' lines for the major pipeline stages: \
     load_ty, parsing, call_graph, non_fundef_pass, fundef_pass, \
     recording_save. The 'Phase:' prefix is filtered as noise by the \
     r-typing CSV parser. Off by default."
  in
  Arg.(value & flag & info ["log-times"] ~doc)

let path_arg =
  let doc = "C source file to parse or package directory to analyze" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)


let cmd =
  let open Term.Syntax in
  Cmd.v
    (Cmd.info "r-c-typing")
    (let+ cst = cst_opt
     and+ past = past_opt
     and+ ast = ast_opt
     and+ mlsem = mlsem_opt
     and+ no_typing = no_typing_opt
     and+ debug = debug_opt
     and+ filter = filter_opt
     and+ include_dirs = include_dir_opt
     and+ timeout = timeout_opt
     and+ fallback_c_signature = fallback_c_sig_opt
     and+ fallback_mode = fallback_mode_opt
     and+ call_graph = call_graph_opt
     and+ log_times = log_times_opt
     and+ lambda_rec = lambda_rec_opt
     and+ mlsem_recording = mlsem_recording_opt
    and+ path = path_arg in
    let fallback_c_signature =
      match fallback_mode with
      | Some None -> false
      | Some (Some m) -> Runner.fallback_mode := m ; true
      | None -> fallback_c_signature in
    PEnv.sequential_handler Defs.parsed_types_penv (fun path -> main ~mlsem_recording {cst; past; ast; mlsem ; typing = not no_typing ; debug ; filter; timeout; fallback_c_signature; call_graph; log_times; lambda_rec} include_dirs path) path |> fst)
     

let () = exit (Cmd.eval cmd)