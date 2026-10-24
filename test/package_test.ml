open R_c_typing
open Mlsem.Types
module System = Mlsem.System

let usage () =
  prerr_endline "Usage: package_test.exe <package-path> [filter]";
  exit 2

let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then usage ();
  let package_path = Sys.argv.(1) in
  let filter = if Array.length Sys.argv = 3 then Some Sys.argv.(2) else None in
  
  (* Options: enable parsing of the PAst and typing, but do not print CST/AST/MLsem. *)
  let opts = Runner.{ cst = false; past = false; ast = false; mlsem = false;
                      typing = true; debug = false; filter; timeout = None;
                      fallback_c_signature = false; call_graph = None;
                      log_times = false } in
  Parser.set_warn_unsupported false;
  let idenv = Runner.Idenv.empty in
  let env = Defs.initial_env in
  ignore (PEnv.sequential_handler Defs.parsed_types_penv 
    (fun path -> Runner.run_on_package opts path idenv env)
    package_path);
  (* Ensure all output is flushed before exiting *) 
  flush stdout;
  Format.pp_print_flush Format.std_formatter ()
