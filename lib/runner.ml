open Mlsem.Common
open Mlsem.Types
module System = Mlsem.System
module MVariable = Mlsem.Lang.MVariable

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

exception Inference_timeout of float

(* ===== Structured outcomes =====

   [infer_ast] and [infer_def] print every result and then return their input
   environments unchanged when inference fails, so an embedder (TypR) can see
   *whether* a native function typed -- via [find_existing_binding] -- but not
   *why* it did not. With [fallback_c_signature] on it cannot even see that: a
   function bound at its declared C signature is indistinguishable from one
   that really typed.

   [on_outcome] reports each terminal result beside the print that already
   happens. A [ref] rather than an optional argument because [infer_def]
   recurses and is partially applied in three folds, and because this layer is
   already configured through global mutable state ([Config.infer_overload],
   [Config.void_ty], the printer config); an embedder installs and restores it
   around its run.

   Four things to know when consuming it:

   - Outcomes fire only for units the [visible_name] predicate accepts. Items
     reached by recursing into [PAst.Include] are never visible, so a package
     that includes <R.h> reports its own definitions rather than the thousands
     of declarations the header drags in. This is the one place the hook is
     narrower than the printing: the failure arms of [infer_ast] print
     unconditionally, but report only when visible.
   - Conversely it is wider than the printing for [`Define] and for file-scope
     variables, whose [print_visible] call is behind [opts.debug]. Those
     outcomes fire either way -- the debug flag is a display choice, and a
     measurement should not depend on it. Expect outcomes with no counterpart
     in stdout.
   - A function that fails and is then rescued by [fallback_c_signature]
     reports *twice*, in this order: the failure ([Timeout] / [Untypeable] /
     [Internal]), then [Fallback]. That pair is the point -- it is what makes a
     substituted signature distinguishable from a real inference.
   - [`SimpleC] mirrors [print_visible]'s kind of the same name, and like it is
     only produced under [~simple_c_fun:true]. Every in-tree caller of that is
     the [PAst.Include] recursion, which is invisible, so the constructor is
     unreachable unless an embedder opts in explicitly. *)
type outcome =
  | Typed of { header : [ `Default | `SimpleC | `DotC | `Define ] ;
               var : Variable.t ; tys : TyScheme.t }
  | Fallback of TyScheme.t
  | Timeout of float
  | Untypeable of { title : string ; descr : string option }
  | Internal of string    (* Not_found / Invalid_argument / tallying Unsat *)

let on_outcome : (name:string -> elapsed:float -> outcome -> unit) ref =
  ref (fun ~name:_ ~elapsed:_ _ -> ())

(* [TyScheme.norm_and_simpl] was removed from mlsem when the normalization hook
   it used became a separate configuration point (see the bottom of this file).
   The rstt-specific simplification is now applied explicitly. *)
let norm_and_simpl tys =
  let vars, gty = TyScheme.get tys in
  TyScheme.mk vars (GTy.map Rstt.TyOp.simplify gty)
  |> TyScheme.simplify_factorize

type cmd_options = {
  cst : bool;
  past : bool;
  ast : bool;
  mlsem : bool;
  typing : bool;
  debug : bool;
  filter: string option;
  timeout : float option;
  (* When full-body inference fails (untypeable, Not_found, or timeout), bind
     the function at its declared C signature ([ret_ty params -> ret_ty]) so
     callers can still be typed instead of cascading "unbound variable". The
     original error is still printed; the fallback is additionally reported on
     a [fallback:] line. Off by default. *)
  fallback_c_signature : bool;
  (* When [Some path], dump the call graph (after [keep_reachable]) in
     Graphviz .dot format to [path]. Cycles are colored, files clustered,
     entry points marked. *)
  call_graph : string option;
  (* When true, emit timing diagnostics:
     - one "  timing: <seconds> s" line on every terminal path of [infer_ast]
       (success, timeout, untypeable, internal Not_found) so slow functions
       can be spotted in the per-package output (indented to match the
       existing [  fallback:] convention so [parse_output.R] keeps treating
       it as continuation);
     - "Phase: <name> <seconds> s" lines summarising the coarse pipeline
       stages (load_ty, parsing, call_graph, non_fundef_pass, fundef_pass,
       recording_save). The [Phase:] prefix is registered as noise by
       [parse_output.R] so it never reaches the per-function CSV.
     Off by default. *)
  log_times : bool;
}

let make_substring_pred = function
  | None -> (fun _ -> true)
  | Some sub ->
      let re = Str.regexp_string sub in
      fun s ->
        try
          ignore (Str.search_forward re s 0);
          true
        with Not_found -> false

let call_graph_node_names call_graph =
  let names = ref [] in
  Call_graph.Callgraph.iter_nodes call_graph (fun name ->
    names := name :: !names);
  List.rev !names

(* idenv: str -> Variable.t 
   env: typing environment: Variable.t -> TyScheme.ty 
*)

(**  Give the [dyn] (gradual) type to any free variables not already in the
     environment. These are C identifiers that neither had a [.ty] binding nor
     a declaration in the translation unit — e.g. calls into another package's
     C code. Binding them gradually lets inference tolerate the unknown type
     instead of propagating constraints from a free variable. *)
let extend_env mlast env =
  let fv = System.Ast.fv mlast in
  let dom = Env.domain env |> VarSet.of_list in
  let missing = VarSet.diff fv dom in
  missing |> VarSet.elements |> List.fold_left
    (fun env v -> Env.add v (TyScheme.mk_mono GTy.dyn) env) env

let is_declaration = function
  | _, PAst.Fundef (_, _, _, (_, PAst.Seq [])) -> true
  | _ -> false

let has_ty_binding name = Defs.StrMap.mem name Defs.defs_map

let find_existing_binding name idenv env =
  match StrMap.find_opt name idenv with
  | Some v ->
      (try Some (v, Env.find v env)
       with Not_found -> None)
  | None ->
      match Defs.StrMap.find_opt name Defs.defs_map with
      | Some v ->
          (try Some (v, Env.find v env)
           with Not_found -> None)
      | None -> None

let has_existing_binding name idenv =
  StrMap.mem name idenv || has_ty_binding name

let register_enum_constants ty : (Variable.t * Ty.t) list =
  let rec aux ty =
    match ty with
    | Ast.Enum (_, enumerators) ->
        List.filter_map
          (fun (name, value) ->
            match value with
            | Some value ->
                let ty = Rstt.Cint.singl value in
                let v = Defs.BuiltinVar.register_dynamic name ty in
                Some (v, ty)
            | None -> None)
          enumerators
    | Ast.Ptr inner -> aux inner
    | Ast.Array (inner, _) -> aux inner
    | _ -> []
  in
  aux ty

(** [extra ()] is called between the type line (and optional [raw:] debug
    line) and the trailing blank separator, so callers can inject indented
    continuation lines (e.g. ["  timing: …"]) inside the same per-function
    block. Defaults to no-op so existing call sites are unchanged. *)
let print_visible ?(debug=false) ?(extra=fun () -> ()) kind visible v tys =
  if visible then begin
    (match kind with
    | `Default ->
        Format.printf "%a: @[<h>%a@]@."
    | `SimpleC ->
        Format.printf "c(%a): @[<h>%a@]@."
    | `DotC ->
        Format.printf ".C(%a): @[<h>%a@]@."
    | `Define ->
        Format.printf "define %a: @[<h>%a@]@.")
      Variable.pp v TyScheme.pp_short tys ;
    if debug then begin
      let _, gty = TyScheme.get tys in
      Format.printf "  raw: @[<h>%a@]@." Ty.pp_raw (GTy.ub gty)
    end;
    extra ();
    Format.printf "@."
  end

let with_inference_timeout timeout thunk =
  match timeout with
  | None -> thunk ()
  | Some seconds ->
      let previous_handler =
        Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise (Inference_timeout seconds)))
      in
      let previous_timer = Unix.getitimer Unix.ITIMER_REAL in
      let restore () =
        ignore (Unix.setitimer Unix.ITIMER_REAL previous_timer);
        Sys.set_signal Sys.sigalrm previous_handler
      in
      Fun.protect
        ~finally:restore
        (fun () ->
          ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value = seconds });
          thunk ())

let infer_ast ?fallback visible opts (idenv, env, decl) (ast : Ast.e) =
  let name,v =
    match ast with
    | _,_,_,Ast.Function (name, _, _, _) ->
        (* When a function-like preprocessor macro shares a name with the
           file-scope variable it wraps (rlang's
           [#define r_stop_internal(...) (r_stop_internal)(...)] is the
           canonical example), [name] is already bound to a mutable global.
           Registering the Fundef as [Immut] would shadow that binding and
           cause assignments to the underlying variable to crash with
           "Cannot assign to an immutable variable". Use [Mut] in that case
           so the macro's inferred call type is still recorded *and*
           assignments to the variable elsewhere remain legal. *)
        let kind =
          match StrMap.find_opt name idenv with
          | Some v_existing when MVariable.is_mutable v_existing -> MVariable.Mut
          | _ -> MVariable.Immut
        in
        name, MVariable.create kind (Some name)
    | _ -> failwith "Expected a function definition at the top level."
  in
  let started = Unix.gettimeofday () in
  let log_timing () =
    if opts.log_times && visible then
      Format.printf "  timing: %.6f s@." (Unix.gettimeofday () -. started)
  in
  (* Same clock as [log_timing], so a reported [elapsed] and a printed
     [timing:] line for the same function agree. *)
  let report o =
    if visible then
      !on_outcome ~name ~elapsed:(Unix.gettimeofday () -. started) o
  in
  if opts.debug then
    Format.printf "Type inference for function %s@." name;
  (* [fallback] is a thunk that produces the declared C signature when body
     inference fails. Kept lazy so the cost (and possible [Failure] from
     [typeof_ctype]) is only paid on the exception path. The original error
     is still printed above; on success this adds a [fallback:] line naming
     the substituted type and binds [name] in env — stopping the "unbound
     variable" cascade through callers. *)
  let apply_fallback () =
    match fallback with
    | None -> (idenv, env, decl)
    | Some compute ->
        (try
          let ty = compute () in
          let tys = ty |> GTy.mk |> TyScheme.mk_mono in
          (* Indent the fallback line so downstream parsers (parse_output.R)
             don't mistake [fallback: <type>] for a new function header. *)
          if visible then
            Format.printf "  fallback: @[<h>%a@]@.@." TyScheme.pp_short tys;
          report (Fallback tys) ;
          (StrMap.add name v idenv, Env.add v tys env, decl)
        with Failure _ -> (idenv, env, decl))
  in
  let mlsem_ast = ref None in
  let mlsem_ast_for_dump () =
    match !mlsem_ast with
    | Some a -> a
    | None -> failwith "mlsem AST not built"
  in
  try
    let m = Ast.to_mlsem ast in
    mlsem_ast := Some m;
    if opts.mlsem && visible then
      Format.printf "%a@." Mlsem.System.Ast.pp m;
    if opts.typing then
        with_inference_timeout opts.timeout (fun () ->
          let _env = extend_env m env in
          let renvs = System.Refinement.refinements env m in
          let reconstructed = System.Reconstruction.infer env renvs m in
          let typ = System.Checker.typeof_def env reconstructed m in
          let tys = norm_and_simpl typ in
          let (vars, typ) = TyScheme.get tys in
          let typ = GTy.ub typ in
          (*Format.printf "%a: upper bound= %a@.@." Variable.pp v  Ty.pp typ ;*)
          (* We only keep the upper bound as type for v and add it to the environment *)
          let tys = TyScheme.mk vars (GTy.mk typ) in
          print_visible ~debug:opts.debug ~extra:log_timing `Default visible v tys;
          report (Typed { header = `Default ; var = v ; tys }) ;
          (StrMap.add name v idenv, Env.add v tys env, decl))
    else
      idenv, env, decl
  with
  | Inference_timeout seconds ->
      Format.printf "%s:@.timeout: inference/checking exceeded %.6g seconds@." name seconds;
      if not opts.mlsem && opts.debug then
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp (mlsem_ast_for_dump ());
      log_timing ();
      report (Timeout seconds) ;
      apply_fallback ()
  | System.Checker.Untypeable err ->
      Format.printf "%s:@.untypeable: %s@." name err.title;
      err.descr |> Option.iter (Format.printf "%s@." ) ;
      if not opts.mlsem && opts.debug  then (* Still print the mlsem ast*)
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp (mlsem_ast_for_dump ());
      log_timing ();
      report (Untypeable { title = err.title ; descr = err.descr }) ;
      apply_fallback ()
  | Not_found ->
      (* Refinement/reconstruction occasionally raises Not_found when an
         identifier referenced in the body isn't bound in the env (e.g. R C
         API macros, file-scope globals). Treat as untypeable so the rest of
         the package still gets processed.
         Fixed upstream; keep this catcher as a safety net for older
         mlsem/sstt pins. *)
      Format.printf "%s:@.untypeable: internal error: Not_found@." name;
      if opts.debug then
        Format.eprintf "%s@." (Printexc.get_backtrace ()) ;
      if not opts.mlsem && opts.debug then
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp (mlsem_ast_for_dump ());
      log_timing ();
      report (Internal "Not_found") ;
      apply_fallback ()
  | Invalid_argument msg ->
      (* mlsem reconstruction asserts [Cannot assign to an immutable variable]
         and similar internal invariants. Treat as untypeable so the function
         is reported by name and the rest of the package still gets processed. *)
      Format.printf "%s:@.untypeable: invalid mlsem AST: %s@." name msg;
      log_timing ();
      report (Internal (Printf.sprintf "invalid mlsem AST: %s" msg)) ;
      apply_fallback ()
  | exn when
      let slot = Printexc.exn_slot_name exn in
      let has_sub s sub =
        let ls = String.length s and lsub = String.length sub in
        let rec loop i =
          if i + lsub > ls then false
          else if String.sub s i lsub = sub then true
          else loop (i + 1)
        in
        loop 0
      in
      has_sub slot "Tallying" && has_sub slot "Unsat"
    ->
      (* Upstream sstt's [Sstt__Tallying.Make(VS).Unsat] occasionally escapes
         [Reconstruction.refine] instead of being caught locally inside
         tallying.ml. Treat as untypeable so the rest of the package still
         gets processed.
         Fixed upstream; keep this catcher as a safety net for older
         mlsem/sstt pins. *)
      Format.printf "%s:@.untypeable: mlsem tallying Unsat (%s)@." name
        (Printexc.to_string exn);
      log_timing ();
      report (Internal (Printf.sprintf "mlsem tallying Unsat (%s)"
        (Printexc.to_string exn))) ;
      apply_fallback ()

(** past: the parsed AST *)
let rec infer_def ?internal_scope ?(force_internal_global=false) ?(simple_c_fun=false) ?(convention=None) ?(skip_if_defined=false) visible_name opts (idenv, env, decl)  past =
  let name = PAst.top_level_unit_name past in
  let visible = visible_name name in
  let started = Unix.gettimeofday () in
  (* The arms below rebind [name]; [PAst.top_level_unit_name] returns exactly
     that binding for every arm that reports, so take it per call site. The
     arms reaching [infer_ast] report from there instead, with their own
     clock. *)
  let report name o =
    if visible then
      !on_outcome ~name ~elapsed:(Unix.gettimeofday () -. started) o
  in

  (* When processing items from external headers, don't override an existing
     definition (e.g. from a .ty file or a previous header). *)
  if skip_if_defined && name <> "" && has_existing_binding name idenv then
    (idenv, env, decl)
  else
  match past with
  | _, PAst.Include items ->
      (* Items come from an external system header. Process them with
         simple C-function inference and priority to already-defined symbols. *)
      List.fold_left
        (fun acc item ->
          infer_def ?internal_scope ~force_internal_global ~simple_c_fun:true ~skip_if_defined:true (fun _ -> false) opts acc item)
        (idenv, env, decl)
        items
  | _, PAst.Define (name, value) ->
      let ty = Ast.typeof_const (PAst.aux_const value) |> GTy.mk |> TyScheme.mk_mono in
      let v =
        if skip_if_defined && not (Defs.StrMap.mem name Defs.defs_map) then
          Defs.BuiltinVar.register_dynamic name (Ast.typeof_const (PAst.aux_const value))
        else
          MVariable.create Immut (Some name)
      in
      if opts.debug && visible then
        print_visible ~debug:opts.debug `Define visible v ty;
      report name (Typed { header = `Define ; var = v ; tys = ty }) ;
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.TypeDecl (name, ty) ->
      let env =
        register_enum_constants ty
        |> List.fold_left (fun env (v, ty) -> Env.add v (TyScheme.mk_mono (GTy.mk ty)) env) env
      in
      (idenv, env, Ast.DeclMap.add name ty decl)
  | _, PAst.GlobalVar (linkage, name, ctype) ->
      (* File-scope variable. A [.ty] binding always wins (manually-curated
         types must not be overridden). For every other pre-existing binding
         we overwrite: in well-formed C, repeated declarations have the same
         ctype so the env binding is unchanged, and when the prior binding
         came from the unknown-name fallback in [PAst.var] (type [any]) the
         more precise ctype-derived type takes over.

         [Env.replace] (not [Env.add]) is required because [register_dynamic]
         returns the same [Variable.t] for repeated declarations of a global
         across translation units (shared [extern] in a header included from
         several .c files). [Env.add] asserts the variable is unbound and
         would crash the package on the second sighting. *)
      let is_internal =
        force_internal_global
        ||
        match linkage with
        | PAst.Static -> true
        | PAst.Extern | PAst.Definition -> false
      in
      if not is_internal && has_ty_binding name then
        (idenv, env, decl)
      else begin
        let storage_name =
          if is_internal then
            begin
              let scope = Option.value ~default:"<file>" internal_scope in
              scope ^ "::" ^ name
            end
          else
            name
        in
        let resolved = PAst.resolve_ctype decl ctype in
        let ty =
          try Ast.typeof_ctype resolved
          with Failure _ -> Ty.any
        in
        (* For SEXP globals, install a gradual annotation [empty .. any_sexp]
           instead of pinning the type at [any_sexp]. The C declaration
           [extern SEXP foo] only constrains the upper bound (any SEXP);
           which SEXP sub-family ([sym] / [env] / [lang] / [v(CHR)] / ...)
           a given read corresponds to is decided by the use-site, and
           gradual reads let the type checker refine accordingly. Without
           this, every read of e.g. [syms_x] returns the full [any_sexp]
           and dispatches that need [sym] or [env] fail. Non-SEXP globals
           (int, c_string, ...) keep their pinned annotation. *)
        let annot =
          if resolved = Ast.SEXP
          then Some (GTy.mk_gradual Ty.empty ty)
          else None
        in
        let v, ty =
          Defs.BuiltinVar.register_dynamic_binding ~mut:true ?annot storage_name ty
        in
        let tys = ty |> GTy.mk |> TyScheme.mk_mono in
        let env = MVariable.add_to_env v tys (Env.rm v env) in
        if opts.debug && visible then
          print_visible ~debug:opts.debug `Default visible v tys;
        report name (Typed { header = `Default ; var = v ; tys }) ;
        (StrMap.add name v idenv, env, decl)
      end
  | _,PAst.Fundef (ret_ty, name, params, _) when convention=Some(Package.C) ->
    (try
      let ty = C_interface.infer_dotC ~typedef_map:decl ret_ty params |> GTy.mk |> TyScheme.mk_mono in
      if has_ty_binding name then begin
        (match find_existing_binding name idenv env with
         | Some (v, tys) ->
             print_visible ~debug:opts.debug `DotC visible v tys ;
             report name (Typed { header = `DotC ; var = v ; tys })
         | None -> ());
        (idenv, env, decl)
      end else if is_declaration past && StrMap.mem name idenv then
        (idenv, env, decl)
      else
        let v = MVariable.create Immut (Some name) in
        print_visible ~debug:opts.debug `DotC visible v ty;
        report name (Typed { header = `DotC ; var = v ; tys = ty }) ;
        (StrMap.add name v idenv, Env.add v ty env, decl)
    with Failure msg ->
      if visible then
        Format.printf "%s:@.untypeable: %s@." name msg;
      report name (Untypeable { title = msg ; descr = None }) ;
      (idenv, env, decl))
  | _, (PAst.Fundef (ret_ty, name, params, _) as e) when simple_c_fun && C_interface.is_simple_c_function e ->
    let ty = C_interface.infer_cfun ~typedef_map:decl ret_ty params |> GTy.mk |>  TyScheme.mk_mono in
    if has_ty_binding name then begin
      (match find_existing_binding name idenv env with
       | Some (v, tys) ->
           print_visible ~debug:opts.debug `SimpleC visible v tys ;
           report name (Typed { header = `SimpleC ; var = v ; tys })
       | None -> ());
      (idenv, env, decl)
    end else if is_declaration past && StrMap.mem name idenv then
      (idenv, env, decl)
    else
      let v = MVariable.create Immut (Some name) in
      print_visible ~debug:opts.debug `SimpleC visible v ty;
      report name (Typed { header = `SimpleC ; var = v ; tys = ty }) ;
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.Fundef (ret_ty, name, params, _) when is_declaration past ->
    if has_ty_binding name then begin
      (match find_existing_binding name idenv env with
       | Some (v, tys) ->
           print_visible ~debug:opts.debug `Default visible v tys ;
           report name (Typed { header = `Default ; var = v ; tys })
       | None -> ());
      (idenv, env, decl)
    end else if StrMap.mem name idenv then
      (idenv, env, decl)
    else
      let ty = C_interface.infer_cfun ~typedef_map:decl ret_ty params |> GTy.mk |> TyScheme.mk_mono in
      let v = MVariable.create Immut (Some name) in
      print_visible ~debug:opts.debug `Default visible v ty;
      report name (Typed { header = `Default ; var = v ; tys = ty }) ;
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.Fundef (_, name, _, _) when has_ty_binding name ->
    (match find_existing_binding name idenv env with
     | Some (v, tys) ->
         print_visible ~debug:opts.debug `Default visible v tys ;
         report name (Typed { header = `Default ; var = v ; tys })
     | None -> ());
    (idenv, env, decl)
  | _ ->

      let e = PAst.transform {PAst.id = [idenv]; decl} past in
      if opts.ast && visible then
        Printf.printf "%s\n" (Ast.show_e e);
      (* Provide a declared-C-signature fallback for Fundefs when [opts] asks
         for it. Only meaningful for Fundefs — everything else reaching this
         branch (if any) just gets a best-effort inference with no fallback. *)
      let fallback =
        if opts.fallback_c_signature then
          match past with
          | _, PAst.Fundef (ret_ty, _, params, _) ->
              Some (fun () -> C_interface.infer_cfun ~typedef_map:decl ret_ty params)
          | _ -> None
        else None
      in
      match e with
      | _, decl', _, Ast.Noop -> (idenv, env, decl')
      | _, decl', _, _ -> infer_ast ?fallback visible opts (idenv, env, decl') e

let run_on_file opts filename idenv env =
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "File not found: %s" filename);
  let cst = Parser.parse_file filename in
  if opts.cst then Parser.print_res cst;
  let past = Parser.to_ast cst in
  let visible_name = make_substring_pred opts.filter in
  if opts.past then begin
    let visible_past =
      match opts.filter with
      | None -> past
      | Some _ ->
          List.filter
            (function
              | _, PAst.Fundef (_, name, _, _) -> visible_name name
              | _, PAst.TypeDecl _ -> false
              | _, PAst.GlobalVar _ -> false
              | _, PAst.Define _ -> false
              | _, PAst.Include _ -> false)
            past
    in
    Printf.printf "%s\n" (PAst.show_definitions visible_past)
  end;
  let idenv, env, _ =
    List.fold_left
      (infer_def ~internal_scope:filename visible_name opts)
      (idenv, env, Ast.DeclMap.empty)
      past
  in
  (idenv, env)

(** Parse each file to a [PAst] program, paired with its file name. Split out
    of [run_on_files] so a caller that has to parse the sources anyway (TypR)
    can hand the result to [run_on_pasts] instead of parsing them twice.

    Diagnostic phase timers report each of the three coarse stages (parse /
    non-Fundef setup / Fundef pass) on a [Phase: …] line so the gap between
    wall-clock and the per-function timings can be attributed.
    [r-typing/scripts/parse_output.R] filters these as noise so they don't leak
    into the per-function CSV. *)
let parse_files opts filenames =
  let t_parse_start = Unix.gettimeofday () in
  let pasts = List.map (fun filename ->
      if not (Sys.file_exists filename) then
        failwith (Printf.sprintf "File not found: %s" filename);
      if opts.debug then
        Printf.printf "Parsing file: %s\n" filename;
      let cst = Parser.parse_file filename in
      if opts.cst then Parser.print_res cst;
      let past = Parser.to_ast cst in
      (filename, past)
    ) filenames in
  if opts.log_times then
    Format.printf "Phase: parsing %.3f s@." (Unix.gettimeofday () -. t_parse_start);
  pasts

(** Type the already-parsed translation units [pasts] (a [(filename, past)]
    list, as returned by [parse_files]). *)
let run_on_pasts opts pasts ?entry_points idenv env =
  let t_callgraph_start = Unix.gettimeofday () in
  let full_call_graph = Call_graph.of_past_list (List.map snd pasts) in
  let entry_names = match entry_points with
    | None -> []
    | Some entries ->
      entries |> List.filter_map (fun (func_name, convention) ->
        match convention with
        | Package.Call | Package.C | Package.External -> Some func_name
        | _ -> None)
  in
  let filter_roots =
    match opts.filter with
    | None -> None
    | Some _ ->
        let visible_name = make_substring_pred opts.filter in
        Some (List.filter visible_name (call_graph_node_names full_call_graph))
  in
  let root_names =
    match filter_roots with
    | Some roots -> roots
    | None -> entry_names
  in
  let call_graph =
    match entry_points, filter_roots with
    | _, Some roots -> Call_graph.keep_reachable full_call_graph roots
    | None, None -> full_call_graph
    | Some _, None -> Call_graph.keep_reachable full_call_graph entry_names
  in
  (* Optional: dump the call graph for inspection (after [keep_reachable]).
     A name -> source-file map is built from [pasts] for per-file clustering. *)
  (match opts.call_graph with
   | None -> ()
   | Some path ->
       let module H = Hashtbl in
       let file_of = H.create 256 in
       let rec scan filename = function
         | _, PAst.Fundef (_, n, _, _) -> H.replace file_of n (Filename.basename filename)
         | _, PAst.Include items -> List.iter (scan filename) items
         | _ -> ()
       in
       List.iter (fun (filename, items) ->
         List.iter (scan filename) items) pasts;
       let file_of_node n = H.find_opt file_of n in
       let dot = Call_graph.to_dot ~file_of_node ~entry_points:root_names call_graph in
       let oc = open_out path in
       output_string oc dot;
       close_out oc;
       Printf.printf "Call graph written to %s\n" path);
  (* Only keep the transitive closure of the entry points *)
  let filtered_pasts = List.concat_map (fun (filename, past) ->
      List.filter_map (fun item ->
        match item with
        | (_,PAst.Fundef (_, name, _, _)) when Call_graph.Callgraph.has_node call_graph name ->
            Some (filename, item)
        | _ -> None
      ) past
    ) pasts in
  let visible_name =
    match filter_roots with
    | None -> make_substring_pred opts.filter
    | Some _ -> (fun name -> Call_graph.Callgraph.has_node call_graph name)
  in
  let definition_files =
    List.fold_left
      (fun acc (filename, past) ->
        List.fold_left
          (fun acc item ->
            match item with
            | _, PAst.GlobalVar (PAst.Definition, name, _) ->
                let files =
                  StrMap.find_opt name acc
                  |> Option.value ~default:StrSet.empty
                in
                StrMap.add name (StrSet.add filename files) acc
            | _ -> acc)
          acc
          past)
      StrMap.empty
      pasts
  in
  let conflicted_definitions =
    StrMap.fold
      (fun name files acc ->
        if StrSet.cardinal files > 1 then StrSet.add name acc else acc)
      definition_files
      StrSet.empty
  in
  (* Pre-collect [TypeDecl]s across every file (recursing into [Include] items)
     before processing [GlobalVar]s. Otherwise globals declared in a file with
     no visible struct body — e.g. [globals.c] writing [struct r_globals_syms
     r_syms;] without including the header that defines the struct — register
     [r_syms] as an empty record. A later registration in another file with
     the full struct cannot upgrade the variable: its stored ty' is frozen at
     creation, the closed empty record and the closed populated record are not
     in a subtype relation, and [register_dynamic_binding] keeps the existing
     type. Building [decl] up front guarantees every [resolve_ctype] call
     sees the full declaration map. *)
  if opts.log_times then
    Format.printf "Phase: call_graph %.3f s@." (Unix.gettimeofday () -. t_callgraph_start);
  let t_non_fundef_start = Unix.gettimeofday () in
  let rec collect_type_decls decl item =
    match item with
    | _, PAst.TypeDecl (name, ty) ->
        let ty = PAst.resolve_ctype decl ty in
        Ast.DeclMap.add name ty decl
    | _, PAst.Include items ->
        List.fold_left collect_type_decls decl items
    | _ -> decl
  in
  let decl =
    List.fold_left
      (fun decl (_filename, past) ->
        List.fold_left collect_type_decls decl past)
      Ast.DeclMap.empty
      pasts
  in
  (* First apply non-function top-level units (structs, defines) in source order. *)
  let idenv, env, decl, internal_idenvs =
    List.fold_left
      (fun (idenv, env, decl, internal_idenvs) (filename, past) ->
        List.fold_left
          (fun (idenv, env, decl, internal_idenvs) item ->
            match item with
            | _, PAst.Fundef _ -> (idenv, env, decl, internal_idenvs)
            | _, PAst.GlobalVar (PAst.Static, _, _) ->
                let file_idenv =
                  StrMap.find_opt filename internal_idenvs
                  |> Option.value ~default:StrMap.empty
                in
                let file_idenv, env, decl =
                  infer_def ~internal_scope:filename ~force_internal_global:true visible_name opts
                    (file_idenv, env, decl) item
                in
                (idenv, env, decl, StrMap.add filename file_idenv internal_idenvs)
            | _, PAst.GlobalVar (PAst.Definition, _, _) when
                StrSet.mem (PAst.top_level_unit_name item) conflicted_definitions ->
                let file_idenv =
                  StrMap.find_opt filename internal_idenvs
                  |> Option.value ~default:StrMap.empty
                in
                let file_idenv, env, decl =
                  infer_def ~internal_scope:filename ~force_internal_global:true visible_name opts
                    (file_idenv, env, decl) item
                in
                (idenv, env, decl, StrMap.add filename file_idenv internal_idenvs)
            | _ ->
                let idenv, env, decl =
                  infer_def ~internal_scope:filename visible_name opts
                    (idenv, env, decl) item
                in
                (idenv, env, decl, internal_idenvs))
          (idenv, env, decl, internal_idenvs)
          past)
      (idenv, env, decl, StrMap.empty)
      pasts
  in
  if opts.log_times then
    Format.printf "Phase: non_fundef_pass %.3f s@."
      (Unix.gettimeofday () -. t_non_fundef_start);

  (* Sort function definitions by call graph. *)
  let sorted_pasts = Call_graph.topo_sort filtered_pasts call_graph in

  if opts.past then begin
    let visible_past = match opts.filter with
      | None -> List.map snd sorted_pasts
      | Some _ ->
          List.filter_map (fun (_, item) ->
            match item with
            | _, PAst.Fundef (_, name, _, _) when visible_name name -> Some item
            | _ -> None
          ) sorted_pasts
    in
    Printf.printf "%s\n" (PAst.show_definitions visible_past)
  end;
  let entry_points = StrMap.of_list (Option.value ~default:[] entry_points) in
  let t_fundef_start = Unix.gettimeofday () in
  let idenv, env, _ =
    List.fold_left
      (fun (idenv, env, decl) (filename, item) ->
        let convention = StrMap.find_opt (PAst.top_level_unit_name item) entry_points in
        let file_idenv =
          StrMap.find_opt filename internal_idenvs
          |> Option.value ~default:StrMap.empty
        in
        let idenv_for_file =
          StrMap.union (fun _ local _global -> Some local) file_idenv idenv
        in
        let idenv', env, decl =
          infer_def ~internal_scope:filename ~convention visible_name opts
            (idenv_for_file, env, decl) item
        in
        let idenv =
          match StrMap.find_opt (PAst.top_level_unit_name item) idenv' with
          | Some v -> StrMap.add (PAst.top_level_unit_name item) v idenv
          | None -> idenv
        in
        (idenv, env, decl))
      (idenv, env, decl)
      sorted_pasts
  in
  if opts.log_times then
    Format.printf "Phase: fundef_pass %.3f s@."
      (Unix.gettimeofday () -. t_fundef_start);
  (idenv, env)

let run_on_files opts filenames ?entry_points idenv env =
  run_on_pasts opts (parse_files opts filenames) ?entry_points idenv env


let run_on_package opts path idenv env =
  let entry_points = Package.find_native_calls path in
  if opts.debug then begin
    Printf.printf "Native calls found in package %s:\n" path;
    List.iter (fun (func_name, convention) ->
      Printf.printf "  %s: %s\n" func_name (Package.calling_convention_to_string convention)
    ) entry_points;
    Printf.printf "\n"
  end;

  (*Get all C files in src/ *)
  let c_files = Package.get_c_files path in
  if opts.debug then begin
    Printf.printf "C source files found in package %s:\n" path;
    List.iter (fun filepath ->
      Printf.printf "  %s\n" filepath
    ) c_files;
    Printf.printf "\n"
  end;

  (* Infer types for  entrypoints *)
  let entry_points_for conv = entry_points |> List.filter_map (fun (func_name, convention) ->
    if convention = conv then Some func_name else None
  ) in
  let call_entry_points = entry_points_for Package.Call in
  let c_entry_points    = entry_points_for Package.C in
  (* Count entry points by calling convention *)
  let count_convention conv =
    List.length (List.filter (fun (_, c) -> c = conv) entry_points)
  in
  let n_call = count_convention Package.Call in
  let n_c = count_convention Package.C in
  let n_fortran = count_convention Package.Fortran in
  let n_external = count_convention Package.External in
  Format.printf
    "Entry points detected: Call=%d, C=%d, Fortran=%d, External=%d@."
    n_call n_c n_fortran n_external;
  Format.printf "Entry points for .Call convention:@.";
  List.iter (fun entry -> Format.printf "  %s@." entry) call_entry_points;
  Format.printf "@.";
  Format.printf "Entry points for .C convention:@.";
  List.iter (fun entry -> Format.printf "  %s@." entry) c_entry_points;
  Format.printf "@.";
  run_on_files opts c_files ~entry_points idenv env
  
(* Solutions produced by tallying may assign a type variable a primitive
   component that is not "whole" (e.g. [INT \ 42L]). Such a component cannot be
   the element type of a *non-scalar* vector -- the record encoding of [v(...)]
   is only sound for whole modes -- so rstt's [TyOp.normalize_subst] widens the
   binding itself back to whole components. That also erases the content of a
   bare CHARSXP, e.g. [mkChar("world") : p("world")] becomes [p(CHR)], where
   nothing is at stake: a CHARSXP is not a vector. Rsem never meets the case
   (there are no bare primitives at the R level); NativeSem meets it everywhere.

   So the rule is applied per variable:
   - a variable of the environment ([ctx.tvars]) may already sit under a
     non-scalar vector elsewhere in the environment, out of sight: rstt's
     conservative rule is kept for it;
   - any other variable (an instantiation copy, the fresh result of an
     application) only occurs in the type being instantiated and is consumed by
     this very substitution, so what matters is what it is bound to: only the
     non-scalar vectors *inside* that type need widening. *)

(* Widen the element type of every non-scalar vector in [ty] to whole
   components: positive atoms are enlarged, negative ones reduced (hence dropped
   when that empties them). Scalars and bare primitives are left alone. The
   result is a supertype of [ty] that satisfies rstt's vector invariant. *)
let enlarge_vector_contents ty =
  let open Sstt in
  let fix_vec_comp c =
    if not (Tag.equal (TagComp.tag c) Rstt.Vec.tag) then c
    else
      let ty = Tags.mk_comp c |> Descr.mk_tags |> Ty.mk_descr in
      let pos = function
        | Rstt.Vec.Vector c -> Rstt.Vec.Vector (Rstt.Prim.enlarge c)
        | Rstt.Vec.Scalar _ as a -> a in
      let neg = function
        | Rstt.Vec.Vector c -> Rstt.Vec.Vector (Rstt.Prim.reduce c)
        | Rstt.Vec.Scalar _ as a -> a in
      Rstt.Vec.destruct ty
      |> List.map (fun (p, ns) -> Rstt.Vec.mk_line (pos p, List.map neg ns))
      |> Ty.disj |> Ty.get_descr |> Descr.get_tags |> Tags.get Rstt.Vec.tag
  in
  let fix_descr d =
    let b, comps = Descr.destruct d in
    let comps = comps |> List.map (function
      | Descr.Tags t ->
        let b, cs = Tags.destruct t in
        Descr.Tags (Tags.construct (b, List.map fix_vec_comp cs))
      | c -> c) in
    Descr.construct (b, comps)
  in
  Transform.transform (VDescr.map fix_descr) ty

let subst_normalization (ctx : Mlsem_system.Heuristics.tally_context) substs =
  substs |> List.filter_map (fun s ->
    (* The environment after this step holds not only [ctx.tvars] but every
       variable the solution introduces into their bindings -- the same
       [ntvars] mlsem computes in [Reconstruction.tally_simpl]. Such a variable
       is not consumed here: it lives on in the environment, and if [s] also
       binds it, that binding must satisfy the conservative rule too. *)
    let tvars =
      MVarSet.union ctx.tvars (Subst.restrict ctx.tvars s |> Subst.intro) in
    let env_part = Subst.restrict tvars s in
    match Rstt.TyOp.normalize_subst env_part with
    | None -> None
    | Some env_part ->
      (* [normalize_subst] rewrites a binding and *composes*, so every other
         binding that mentions the rewritten variable follows. Splitting [s]
         in two loses that for the fresh bindings: solutions are routinely not
         idempotent, so a fresh binding may well mention an environment
         variable whose binding was just rewritten. Re-apply the normalized
         environment part to them before recombining. *)
      let fresh_part =
        Subst.remove_many tvars s
        |> Subst.map1 (fun ty -> Subst.apply env_part ty |> enlarge_vector_contents) in
      Some (Subst.combine env_part fresh_part))

let () =
  (* rstt registers its own printers for [Prim], [Vec], [Lst], ... at module
     initialisation; [Rstt.Pp.printer_params ()] returns the accumulated set. *)
  Mlsem_types.PrinterCfg.set_descr_printer Rstt.Pp.print_descr_ctx ;
  Mlsem_types.PrinterCfg.set_printer Rstt.Pp.print ;
  Mlsem_types.PrinterCfg.add_printer_param (Rstt.Pp.printer_params ()) ;
  (* Disambiguate the two [any]s in printed output: [Attr.any] (the attr-tagged
     subset of SEXPs — everything except CHARSXP) was rendering as the bare
     word [any], which clashes with [Ty.any] (the universal type written
     literally as [any] in [.ty] input). Print [Attr.any] as [attr_any] so
     diagnostics like [r_attrib_get_cb: (any | chr, ...)] become readable. *)
  Mlsem_types.PrinterCfg.add_printer_param
    (Rstt.Pp.printer_params' [(Rstt.Attr.any, "attr_any"); (Defs.any_sexp, "any_sexp")]) ;
  Mlsem_system.Config.normalization_fun := Fun.id ;
  Mlsem_system.Config.subst_normalization_fun := subst_normalization

let%test "filter predicate with Some substring" =
  let pred = make_substring_pred (Some "from") in
  pred "from_str" && pred "substring_from" && not (pred "to_str")

let%test "filter predicate with None accepts all" =
  let pred = make_substring_pred None in
  pred "anything" && pred "everything"


let%test ".ty bindings have highest precedence" =
  let tys = TyScheme.mk_mono GTy.dyn in
  let past = (Position.dummy, PAst.Fundef (Ast.Int, "strerror", [], (Position.dummy, PAst.Seq []))) in
  let idenv, env, _ =
    if has_ty_binding "strerror" then
      (StrMap.empty, Defs.initial_env, Ast.DeclMap.empty)
    else if is_declaration past && StrMap.mem "strerror" StrMap.empty then
      (StrMap.empty, Defs.initial_env, Ast.DeclMap.empty)
    else
      let v = MVariable.create Immut (Some "strerror") in
      (StrMap.add "strerror" v StrMap.empty,
       Env.add v tys Defs.initial_env,
       Ast.DeclMap.empty)
  in
      match find_existing_binding "strerror" idenv env, find_existing_binding "strerror" StrMap.empty Defs.initial_env with
      | Some (actual_v, actual_tys), Some (expected_v, expected_tys) ->
        not (StrMap.mem "strerror" idenv)
        && Variable.equal actual_v expected_v
        && actual_tys == expected_tys
      | _ -> false

let%test "full function overrides declaration" =
  let decl_v = MVariable.create Immut (Some "foo") in
  let full_v = MVariable.create Immut (Some "foo") in
  let decl_ty = TyScheme.mk_mono GTy.dyn in
  let full_ty = TyScheme.mk_mono (GTy.mk Rstt.Cenums.void) in
  let idenv = StrMap.add "foo" decl_v StrMap.empty in
  let env = Env.add decl_v decl_ty Env.empty in
  let idenv, env, _ =
    (StrMap.add "foo" full_v idenv,
     Env.add full_v full_ty env,
     Ast.DeclMap.empty)
  in
  Variable.equal (StrMap.find "foo" idenv) full_v
  && Env.find full_v env == full_ty

let%test "declaration does not override existing full function" =
  let full_v = MVariable.create Immut (Some "foo") in
  let full_ty = TyScheme.mk_mono GTy.dyn in
  let idenv = StrMap.add "foo" full_v StrMap.empty in
  let env = Env.add full_v full_ty Env.empty in
  let past = (Position.dummy, PAst.Fundef (Ast.Int, "foo", [], (Position.dummy, PAst.Seq []))) in
  let idenv, env, _ =
    if has_ty_binding "foo" then
      (idenv, env, Ast.DeclMap.empty)
    else if is_declaration past && StrMap.mem "foo" idenv then
      (idenv, env, Ast.DeclMap.empty)
    else
      let v = MVariable.create Immut (Some "foo") in
      (StrMap.add "foo" v idenv,
       Env.add v (TyScheme.mk_mono (GTy.mk Rstt.Cenums.void)) env,
       Ast.DeclMap.empty)
  in
  Variable.equal (StrMap.find "foo" idenv) full_v
  && Env.find full_v env == full_ty
