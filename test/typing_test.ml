open R_c_typing
open Mlsem.Types
module System = Mlsem.System

let usage () =
  prerr_endline "Usage: typing_test.exe [--fallback-c-signature] <c-file>";
  exit 2

let () =
  System.Config.infer_overload := true ;
  let fallback_c_signature, full_c_file =
    match Array.to_list Sys.argv with
    | [ _ ; file ] -> false, file
    | [ _ ; "--fallback-c-signature" ; file ] -> true, file
    | _ -> usage ()
  in
  (* Options: enable parsing of the PAst and typing, but do not print CST/AST/MLsem. *)
  let opts = Runner.{ cst = false; past = false; ast = false; mlsem = false;
                      typing = true; debug = false; filter = None; timeout = None;
                      fallback_c_signature; call_graph = None;
                      log_times = false; lambda_rec = None } in
  let idenv = Runner.Idenv.empty in
  let env = Defs.initial_env in
  (* Runner.run_on_file already prints the interesting output to stdout via
     Format/Printf, which dune will capture in the rules using with-stdout-to. *)
  ignore (PEnv.sequential_handler Defs.parsed_types_penv 
    (fun filename -> Runner.run_on_file opts filename idenv env)
    full_c_file);
  (* Ensure all output is flushed before exiting *) 
  flush stdout;
  Format.pp_print_flush Format.std_formatter ()
