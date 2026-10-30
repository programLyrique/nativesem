open Mlsem.Common
open Mlsem.Types
module System = Mlsem.System
module MVariable = Mlsem.Lang.MVariable

module StrMap = Map.Make(String)

(* The identifier environment, [idenv]: the C names bound so far, each in its
   namespace ([PAst.Name.Ident] or, for a function-like macro,
   [PAst.Name.Macro]). *)
module Idenv = PAst.NameMap
let id name = PAst.Name.Ident name
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
  (* When [Some seconds], a recursive function is also typed as an MLsem
     [LambdaRec], within that time, and the result intersected with the one
     obtained against its C signature (see [infer_def]). Off by default. *)
  lambda_rec : float option;
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
  match Idenv.find_opt (id name) idenv with
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
  Idenv.mem (id name) idenv || has_ty_binding name

(* Binds a constant of a header ([#define], enumerator), whose variable
   ([Defs.BuiltinVar.register_dynamic]) is shared by name: a header is read
   once per file that includes it. The last definition wins, as for a
   [#define] after an [#undef]; an enumerator coming back with another value
   (an error in C within one file) is reported. *)
let bind_constant ~enum name v ty env =
  (match Env.find_opt v env with
   | Some old when enum && not (GTy.equiv (snd (TyScheme.get old)) ty) ->
       Format.eprintf "Warning: enumerator %s redefined with another value@." name
   | _ -> ()) ;
  Env.replace v (TyScheme.mk_mono ty) env

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
                Defs.BuiltinVar.mark_exact name ;
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

(* Bind [v] under [key] at the declared C signature [compute ()], printing a
   [fallback:] line and reporting a [Fallback] outcome. Leaves the
   environments unchanged if the signature cannot be built. Shared by
   [infer_ast] (inference failed) and [bind_c_signature] (an embedder's own
   time limit fired). *)
let bind_fallback ~report visible (idenv, env, decl) key v compute =
  try
    let ty = compute () in
    let tys = ty |> GTy.mk |> TyScheme.mk_mono in
    (* Indent the fallback line so downstream parsers (parse_output.R)
       don't mistake [fallback: <type>] for a new function header. *)
    if visible then
      Format.printf "  fallback: @[<h>%a@]@.@." TyScheme.pp_short tys;
    report (Fallback tys) ;
    (Idenv.add key v idenv, Env.add v tys env, decl)
  with Failure _ -> (idenv, env, decl)

(* Complete an inferred function type [typ] (a scheme over [vars]) that only
   ever fails, with the declared C signature [declared] on the inputs it lost.
   Applied to functions declared to return a value only (see [infer_def]).

   NativeSem drops from a function's domain the inputs whose path it could
   not type (a branch with an unbound name or an untypeable call), while an
   input that leads to an error stays, with result [empty]. A type all of
   whose arms return [empty] and whose domain does not cover the declared one
   therefore lost its real paths: vctrs' [s3_ptype] keeps only its
   [r_stop_internal] branches, [r_call_n] only a NULL argument. Such a type
   serves callers worse than the signature, so the missing part is given the
   signature's result. Error helpers are untouched, their domain being whole;
   so is a type with a real result, whose narrow domain may be a genuine
   requirement (an [INTEGER(x)] call) that the R side checks. Returns [None]
   when nothing is completed. *)
let complete_degenerate vars typ declared =
  (* An inferred function is attr-tagged ([cattr]: any classes and
     attributes); a C signature is a bare arrow. *)
  let wrapped = Ty.leq typ Rstt.Attr.any && not (Ty.is_empty typ) in
  let content t = if wrapped then Rstt.Attr.proj_content t else t in
  let wrap t = if wrapped then Ast.AttrConstr.cons Rstt.Classes.any [ t ] else t in
  let arms = try Arrow.dnf (content typ) with _ -> [] in
  let degenerate =
    arms <> [] && List.for_all (List.for_all (fun (_, codom) -> Ty.is_empty codom)) arms in
  if not degenerate then None
  else
    let ground =
      let s = MVarSet.proj1 vars |> Sstt.VarSet.elements
              |> List.map (fun v -> (v, Ty.any)) |> Subst.of_list1 in
      Subst.apply s (content typ)
    in
    let missing = Ty.diff (Arrow.domain declared) (Arrow.domain ground) in
    if Ty.is_empty missing then None
    else Some (Ty.cap typ (wrap (Arrow.mk missing (Arrow.apply declared missing))), missing)

(* [alternative]: another valid type of the function (a recursive function
   typed as a [LambdaRec], see [infer_def]), intersected with the inferred one,
   or used instead of the C signature when inference fails. *)
(* The implicit pointer conversions of [name] whose value had no common
   pointee with the declared type, e.g. a struct laid out in [RAW()]: only the
   replacing branch of their lowering ([Ast.narrowings]) is reached. They are
   typed at the declared type, as an explicit cast would be; a warning on
   stderr keeps the rare real confusions visible. *)
let warn_pointer_fallbacks name m reconstructed narrowings =
  if narrowings <> [] then
    match System.Analyzer.analyze m reconstructed with
    | exception Failure _ -> ()
    | _, visited ->
      let unreachable =
        System.Analyzer.get_unreachable visited m
        |> List.map (fun (msg : System.Analyzer.msg) -> msg.eid) in
      let reached eid = not (List.exists (Eid.equal eid) unreachable) in
      narrowings |> List.rev |> List.iter (fun (n : Ast.narrowing) ->
        if not (reached n.narrowed) && reached n.replaced then
          (* Positions keep tree-sitter's 0-based rows, and no file name. *)
          let line =
            if n.at = Position.dummy then ""
            else Printf.sprintf ", line %d" (Position.line (Position.start_of_position n.at) + 1) in
          let ty = Ast.c_spelling n.target in
          Format.eprintf
            "warning: %s%s: pointer conversion to %s with no common pointee with the value; using %s@."
            name line ty ty)

let infer_ast ?fallback ?completion ?alternative ~key visible opts (idenv, env, decl) (ast : Ast.e) =
  let name,v =
    match ast with
    | _,_,_,Ast.Function (name, _, _, _, _) ->
        name, MVariable.create MVariable.Immut (Some name)
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
    match alternative, fallback with
    | Some tys, _ ->
      print_visible ~debug:opts.debug `Default visible v tys;
      report (Typed { header = `Default ; var = v ; tys }) ;
      (Idenv.add key v idenv, Env.add v tys env, decl)
    | None, None -> (idenv, env, decl)
    | None, Some compute -> bind_fallback ~report visible (idenv, env, decl) key v compute
  in
  let mlsem_ast = ref None in
  let mlsem_ast_for_dump () =
    match !mlsem_ast with
    | Some a -> a
    | None -> failwith "mlsem AST not built"
  in
  try
    Ast.narrowings := [] ;
    let m = Ast.to_mlsem ast in
    let narrowings = !Ast.narrowings in
    mlsem_ast := Some m;
    if opts.mlsem && visible then
      Format.printf "%a@." Mlsem.System.Ast.pp m;
    if opts.typing then
        with_inference_timeout opts.timeout (fun () ->
          let _env = extend_env m env in
          let renvs = System.Refinement.refinements env m in
          let reconstructed = System.Reconstruction.infer env renvs m in
          let typ = System.Checker.typeof_def env reconstructed m in
          warn_pointer_fallbacks name m reconstructed narrowings ;
          let tys = norm_and_simpl typ in
          let (vars, typ) = TyScheme.get tys in
          let typ = GTy.ub typ in
          let typ, completed_on =
            match completion with
            | None -> typ, None
            | Some compute ->
              (match complete_degenerate vars typ (compute ()) with
               | Some (completed, missing) -> completed, Some missing
               | None -> typ, None
               | exception Failure _ -> typ, None)
          in
          (*Format.printf "%a: upper bound= %a@.@." Variable.pp v  Ty.pp typ ;*)
          (* We only keep the upper bound as type for v and add it to the environment *)
          let tys = TyScheme.mk vars (GTy.mk typ) in
          let tys =
            match alternative with
            | None -> tys
            | Some alt ->
              let (vars', typ') = TyScheme.get alt in
              TyScheme.mk (MVarSet.union vars vars') (GTy.mk (Ty.cap typ (GTy.ub typ')))
              |> norm_and_simpl
          in
          (* An indented line under the type, as [timing:] and [fallback:]. *)
          let extra () =
            Option.iter (Format.printf "  completed by the C signature on: @[<h>%a@]@." Ty.pp)
              completed_on ;
            log_timing ()
          in
          print_visible ~debug:opts.debug ~extra `Default visible v tys;
          report (Typed { header = `Default ; var = v ; tys }) ;
          (Idenv.add key v idenv, Env.add v tys env, decl))
    else
      idenv, env, decl
  with
  | Inference_timeout seconds ->
      if visible then
        Format.printf "%s:@.timeout: inference/checking exceeded %.6g seconds@." name seconds;
      if not opts.mlsem && opts.debug then
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp (mlsem_ast_for_dump ());
      log_timing ();
      report (Timeout seconds) ;
      apply_fallback ()
  | System.Checker.Untypeable err ->
      if visible then begin
        Format.printf "%s:@.untypeable: %s@." name err.title;
        err.descr |> Option.iter (Format.printf "%s@." )
      end ;
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
      if visible then Format.printf "%s:@.untypeable: internal error: Not_found@." name;
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
      if visible then Format.printf "%s:@.untypeable: invalid mlsem AST: %s@." name msg;
      if opts.debug then
        Format.eprintf "%s@." (Printexc.get_backtrace ()) ;
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
      if visible then
        Format.printf "%s:@.untypeable: mlsem tallying Unsat (%s)@." name
          (Printexc.to_string exn);
      log_timing ();
      report (Internal (Printf.sprintf "mlsem tallying Unsat (%s)"
        (Printexc.to_string exn))) ;
      apply_fallback ()

(** past: the parsed AST *)
(* The name a top-level definition is bound under. *)
let binding_name = function
  | _, PAst.MacroDef (name, _, _) -> PAst.Name.Macro name
  | past -> id (PAst.top_level_unit_name past)

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
        if skip_if_defined && not (Defs.StrMap.mem name Defs.defs_map) then begin
          let value = PAst.aux_const value in
          if Ast.exact_const value then Defs.BuiltinVar.mark_exact name ;
          Defs.BuiltinVar.register_dynamic name (Ast.typeof_const value)
        end else
          MVariable.create Immut (Some name)
      in
      if opts.debug && visible then
        print_visible ~debug:opts.debug `Define visible v ty;
      report name (Typed { header = `Define ; var = v ; tys = ty }) ;
      let env =
        if skip_if_defined then bind_constant ~enum:false name v (snd (TyScheme.get ty)) env
        else Env.add v ty env
      in
      (Idenv.add (id name) v idenv, env, decl)
  | _, PAst.TypeDecl (name, ty) ->
      let env =
        register_enum_constants ty
        |> List.fold_left (fun env (v, ty) ->
            bind_constant ~enum:true (Variable.get_name v |> Option.value ~default:"?") v (GTy.mk ty) env) env
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
        (Idenv.add (id name) v idenv, env, decl)
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
      end else if is_declaration past && Idenv.mem (id name) idenv then
        (idenv, env, decl)
      else
        let v = MVariable.create Immut (Some name) in
        print_visible ~debug:opts.debug `DotC visible v ty;
        report name (Typed { header = `DotC ; var = v ; tys = ty }) ;
        (Idenv.add (id name) v idenv, Env.add v ty env, decl)
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
    end else if is_declaration past && Idenv.mem (id name) idenv then
      (idenv, env, decl)
    else
      let v = MVariable.create Immut (Some name) in
      print_visible ~debug:opts.debug `SimpleC visible v ty;
      report name (Typed { header = `SimpleC ; var = v ; tys = ty }) ;
      (Idenv.add (id name) v idenv, Env.add v ty env, decl)
  | _, PAst.Fundef (ret_ty, name, params, _) when is_declaration past ->
    if has_ty_binding name then begin
      (match find_existing_binding name idenv env with
       | Some (v, tys) ->
           print_visible ~debug:opts.debug `Default visible v tys ;
           report name (Typed { header = `Default ; var = v ; tys })
       | None -> ());
      (idenv, env, decl)
    end else if Idenv.mem (id name) idenv then
      (idenv, env, decl)
    else
      let ty = C_interface.infer_cfun ~typedef_map:decl ret_ty params |> GTy.mk |> TyScheme.mk_mono in
      let v = MVariable.create Immut (Some name) in
      print_visible ~debug:opts.debug `Default visible v ty;
      report name (Typed { header = `Default ; var = v ; tys = ty }) ;
      (Idenv.add (id name) v idenv, Env.add v ty env, decl)
  | _, PAst.MacroDef (name, params, (_, PAst.Seq [])) ->
    (* A macro whose body could not be parsed: bound at [any -> any] (its
       parameters are [Ast.Any]), as a declaration would be. *)
    let ty = C_interface.infer_cfun ~typedef_map:decl Ast.Any params |> GTy.mk |> TyScheme.mk_mono in
    let v = MVariable.create Immut (Some name) in
    print_visible ~debug:opts.debug `Default visible v ty;
    report name (Typed { header = `Default ; var = v ; tys = ty }) ;
    (Idenv.add (PAst.Name.Macro name) v idenv, Env.add v ty env, decl)
  | _, PAst.Fundef (_, name, _, _) when has_ty_binding name ->
    (match find_existing_binding name idenv env with
     | Some (v, tys) ->
         print_visible ~debug:opts.debug `Default visible v tys ;
         report name (Typed { header = `Default ; var = v ; tys })
     | None -> ());
    (idenv, env, decl)
  | _ ->
      (* A recursive function: its body refers to its own name, a self-loop
         in the call graph. *)
      let recursive =
        match past with
        | _, PAst.Fundef (_, name, _, body) ->
          PAst.extract_calls_from_expr ~fun_names:(PAst.StrSet.singleton name) body <> []
        | _ -> false
      in
      (* Its recursive calls must see a type, in two ways whose results are
         both valid types of the function, hence so is their intersection:

         - as an MLsem [LambdaRec] ([opts.lambda_rec]), the recursive calls
           referring to a [self] variable inferred with the body: precise, but
           [self] is monomorphic and has the function's own inferred domain,
           so a branch whose recursive argument is coarser is lost (vctrs'
           [last(CDR(x))], [CDR] giving any SEXP). Done first, quietly and
           within its own time budget, as the [alternative];
         - bound first, quietly, at its declared C signature: only the
           recursive calls' results are then as coarse as the signature, the
           rest of the body is still inferred, and the inferred type replaces
           the binding below. *)
      let alternative =
        match past with
        | _, PAst.Fundef (_, name, _, _) when recursive && opts.lambda_rec <> None ->
          let self = MVariable.create MVariable.Immut (Some name) in
          (match PAst.transform ~self {PAst.id = [idenv]; decl; ctypes = Ast.VarMap.empty; cells = StrMap.empty; labels = PAst.StrSet.empty} past with
           | _, _, _, Ast.Noop -> None
           | (_, decl', _, _) as e ->
             let key = binding_name past in
             let idenv', env', _ =
               infer_ast ~key false
                 { opts with fallback_c_signature = false ; timeout = opts.lambda_rec }
                 (idenv, env, decl') e in
             (match Idenv.find_opt key idenv', Idenv.find_opt key idenv with
              | Some v, before when Some v <> before -> Some (Env.find v env')
              | _ -> None))
        | _ -> None
      in
      let idenv, env, decl =
        match past with
        | _, PAst.Fundef (ret_ty, name, params, _)
          when recursive && opts.fallback_c_signature
               && not (Idenv.mem (id name) idenv) ->
            bind_fallback ~report:(fun _ -> ()) false (idenv, env, decl) (id name)
              (MVariable.create MVariable.Immut (Some name))
              (fun () -> C_interface.infer_cfun ~typedef_map:decl ret_ty params)
        | _ -> (idenv, env, decl)
      in
      let e = PAst.transform {PAst.id = [idenv]; decl; ctypes = Ast.VarMap.empty; cells = StrMap.empty; labels = PAst.StrSet.empty} past in
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
          | _, PAst.MacroDef (_, params, _) ->
              Some (fun () -> C_interface.infer_cfun ~typedef_map:decl Ast.Any params)
          | _ -> None
        else None
      in
      (* The signature also completes a type that only ever fails
         ([complete_degenerate]), but only for a function declared to return
         a value: such a function whose every path fails has lost its real
         paths, while a [void] error helper ([stop_*]) or a macro (whose
         declared type is just [any -> any]) failing everywhere is genuine. *)
      let completion =
        match past with
        | _, PAst.Fundef (ret_ty, _, _, _)
          when not (List.mem (PAst.resolve_ctype decl ret_ty) [ Ast.Void ; Ast.NoReturn ]) -> fallback
        | _ -> None
      in
      match e with
      | _, decl', _, Ast.Noop -> (idenv, env, decl')
      | _, decl', _, _ ->
        infer_ast ?fallback ?completion ?alternative ~key:(binding_name past) visible opts
          (idenv, env, decl') e

(** Bind the function definition [past] at its declared C signature, as
    [infer_ast] does when inference fails. For an embedder that enforces its
    own time limit (TypR) and must still bind a function that ran out of
    time: left unbound, every caller would report it as an unbound variable,
    and branches calling it would be pruned. *)
let bind_c_signature visible (idenv, env, decl) past =
  match past with
  | _, (PAst.Fundef (_, name, params, _) | PAst.MacroDef (name, params, _)) ->
      let ret_ty = match past with _, PAst.Fundef (ret_ty, _, _, _) -> ret_ty | _ -> Ast.Any in
      let v = MVariable.create MVariable.Immut (Some name) in
      let report o = if visible then !on_outcome ~name ~elapsed:0. o in
      bind_fallback ~report visible (idenv, env, decl) (binding_name past) v
        (fun () -> C_interface.infer_cfun ~typedef_map:decl ret_ty params)
  | _ -> (idenv, env, decl)

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
              | _, (PAst.Fundef (_, name, _, _) | PAst.MacroDef (name, _, _)) -> visible_name name
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
        | (_, (PAst.Fundef (_, name, _, _) | PAst.MacroDef (name, _, _)))
          when Call_graph.Callgraph.has_node call_graph name ->
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
            | _, (PAst.Fundef _ | PAst.MacroDef _) -> (idenv, env, decl, internal_idenvs)
            (* A [static] in a header is in every file that includes it (local
               includes are not followed): bound package-wide, below. *)
            | _, PAst.GlobalVar (PAst.Static, _, _)
              when not (Filename.check_suffix filename ".h") ->
                let file_idenv =
                  StrMap.find_opt filename internal_idenvs
                  |> Option.value ~default:Idenv.empty
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
                  |> Option.value ~default:Idenv.empty
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
          |> Option.value ~default:Idenv.empty
        in
        let idenv_for_file =
          Idenv.union (fun _ local _global -> Some local) file_idenv idenv
        in
        let idenv', env, decl =
          infer_def ~internal_scope:filename ~convention visible_name opts
            (idenv_for_file, env, decl) item
        in
        let idenv =
          let key = binding_name item in
          match Idenv.find_opt key idenv' with
          | Some v -> Idenv.add key v idenv
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
    | Some env_part' ->
      (* [normalize_subst] rewrites a binding and *composes* it into the rest
         of the solution, so every other binding mentioning the rewritten
         variable follows. Splitting [s] in two loses that for the fresh
         bindings: solutions are routinely not idempotent, so a fresh binding
         may well mention an environment variable whose binding was just
         rewritten. Propagate exactly the rewritten bindings -- and only those:
         substitutions are applied once, so resolving an untouched variable
         here would be a second application. *)
      let rewritten =
        env_part' |> Subst.filter1 (fun v ty ->
          not (Sstt.Ty.equiv ty (Subst.find1 env_part v))) in
      let fresh_part =
        Subst.remove_many tvars s
        |> Subst.map1 (fun ty -> Subst.apply rewritten ty |> enlarge_vector_contents) in
      Some (Subst.combine env_part' fresh_part))

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
      (Idenv.empty, Defs.initial_env, Ast.DeclMap.empty)
    else if is_declaration past && Idenv.mem (id "strerror") Idenv.empty then
      (Idenv.empty, Defs.initial_env, Ast.DeclMap.empty)
    else
      let v = MVariable.create Immut (Some "strerror") in
      (Idenv.add (id "strerror") v Idenv.empty,
       Env.add v tys Defs.initial_env,
       Ast.DeclMap.empty)
  in
      match find_existing_binding "strerror" idenv env, find_existing_binding "strerror" Idenv.empty Defs.initial_env with
      | Some (actual_v, actual_tys), Some (expected_v, expected_tys) ->
        not (Idenv.mem (id "strerror") idenv)
        && Variable.equal actual_v expected_v
        && actual_tys == expected_tys
      | _ -> false

let%test "full function overrides declaration" =
  let decl_v = MVariable.create Immut (Some "foo") in
  let full_v = MVariable.create Immut (Some "foo") in
  let decl_ty = TyScheme.mk_mono GTy.dyn in
  let full_ty = TyScheme.mk_mono (GTy.mk Rstt.Cenums.void) in
  let idenv = Idenv.add (id "foo") decl_v Idenv.empty in
  let env = Env.add decl_v decl_ty Env.empty in
  let idenv, env, _ =
    (Idenv.add (id "foo") full_v idenv,
     Env.add full_v full_ty env,
     Ast.DeclMap.empty)
  in
  Variable.equal (Idenv.find (id "foo") idenv) full_v
  && Env.find full_v env == full_ty

let%test "declaration does not override existing full function" =
  let full_v = MVariable.create Immut (Some "foo") in
  let full_ty = TyScheme.mk_mono GTy.dyn in
  let idenv = Idenv.add (id "foo") full_v Idenv.empty in
  let env = Env.add full_v full_ty Env.empty in
  let past = (Position.dummy, PAst.Fundef (Ast.Int, "foo", [], (Position.dummy, PAst.Seq []))) in
  let idenv, env, _ =
    if has_ty_binding "foo" then
      (idenv, env, Ast.DeclMap.empty)
    else if is_declaration past && Idenv.mem (id "foo") idenv then
      (idenv, env, Ast.DeclMap.empty)
    else
      let v = MVariable.create Immut (Some "foo") in
      (Idenv.add (id "foo") v idenv,
       Env.add v (TyScheme.mk_mono (GTy.mk Rstt.Cenums.void)) env,
       Ast.DeclMap.empty)
  in
  Variable.equal (Idenv.find (id "foo") idenv) full_v
  && Env.find full_v env == full_ty

(* [complete_degenerate] on [f(int k)]: only the error path for [k == 0] was
   typed, so the other inputs come back with the declared result. *)
let%test "complete_degenerate restores the inputs a failing type lost" =
  let open Rstt in
  let wrap t = Ast.AttrConstr.cons Classes.any [ t ] in
  let declared = Arrow.mk (Tuple.mk [ Cint.any_na ]) Defs.any_sexp in
  let inferred = wrap (Arrow.mk (Tuple.mk [ Cint.ff ]) Ty.empty) in
  match complete_degenerate MVarSet.empty inferred declared with
  | Some (completed, missing) ->
    Ty.equiv missing (Tuple.mk [ Ty.diff Cint.any_na Cint.ff ])
    && Ty.leq completed inferred
  | None -> false

let%test "complete_degenerate leaves error helpers and real results alone" =
  let open Rstt in
  let wrap t = Ast.AttrConstr.cons Classes.any [ t ] in
  let declared = Arrow.mk (Tuple.mk [ Cint.any_na ]) Defs.any_sexp in
  (* Every input errors: the domain is whole, nothing was lost. *)
  let helper = wrap (Arrow.mk (Tuple.mk [ Cint.any_na ]) Ty.empty) in
  (* A narrow domain with a real result is a requirement, kept as is. *)
  let narrow = wrap (Arrow.mk (Tuple.mk [ Cint.ff ]) Cint.tt) in
  complete_degenerate MVarSet.empty helper declared = None
  && complete_degenerate MVarSet.empty narrow declared = None
