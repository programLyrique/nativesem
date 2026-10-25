open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
module U = Parser_utils

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

let warn_unsupported = ref true

let set_warn_unsupported value =
  warn_unsupported := value

let default_predefined_defines =
  match Sys.os_type with
  | "Win32" ->
      StrSet.of_list ["_WIN32"; "WIN32"; "__WIN32"; "__WINDOWS__"]
  | _ ->
      StrSet.of_list
        ["__linux__"; "__linux"; "linux"; "__gnu_linux__"; "__unix__"; "__unix"; "unix"]

let predefined_defines = ref default_predefined_defines

let set_predefined_defines defines =
  predefined_defines := StrSet.of_list defines

let current_predefined_defines () =
  !predefined_defines

(* Names of every [#define] / [#define f(...)] seen during the top-level walk,
   accumulated so the in-body preprocessor expander can evaluate
   [#if defined(NAME)] / [#ifdef NAME] for file-scope macros. *)
let seen_define_names : StrSet.t ref = ref StrSet.empty

let reset_seen_define_names () = seen_define_names := StrSet.empty

(* The function-like macros defined so far, minus those [#undef]'d. A name is
   added once the macro's own body has been parsed: inside its expansion C
   does not expand the macro again, so [NAME(...)] there is an ordinary call.
   The parser turns [NAME(...)] into a [MacroCall] for these names. *)
let function_macros : StrSet.t ref = ref StrSet.empty

let is_function_macro name = StrSet.mem name !function_macros

(* The file-scope variables declared so far. A macro body may assign them: they
   are visible where the macro is defined, unlike the caller's locals (see
   [behaves_like_function]). *)
let globals : StrSet.t ref = ref StrSet.empty

let remember_global name = globals := StrSet.add name !globals

(* The function-like macros whose body does not behave like a function body
   ([behaves_like_function]): their meaning depends on the expansion site, so
   a call is replaced by the body, the arguments substituted for the
   parameters ([expand_macro], called by the parser). *)
let expansions : (string, A.param list * A.e) Hashtbl.t = Hashtbl.create 16

let remember_seen_define_name name =
  seen_define_names := StrSet.add name !seen_define_names

let known_defines () = StrSet.union !predefined_defines !seen_define_names

(** TEMPORARY WORKAROUND: preloaded string values for a few macros we can't
    currently resolve through normal preprocessing. These are either (a) in
    system headers we don't parse (e.g. [<Rinternals.h>], [<inttypes.h>]'s
    deeper expansions), or (b) guarded by [#if R_VERSION < R_Version(...)]
    conditions that our [eval_preproc_expression] can't evaluate because it
    lacks numeric-valued defines and function-like macro support.

    The long-term plan is to extend [eval_preproc_expression] with numeric
    defines and function-like macro evaluation (at minimum [R_Version])
    so the conditions in package headers resolve correctly. Until then,
    grow this list as new packages surface missing macros. *)
let default_predefined_define_constants : A.const StrMap.t =
  List.fold_left (fun m (k, v) -> StrMap.add k v m) StrMap.empty [
    "R_PRIdXLEN_T", A.CStr "d";
    "PRIu64", A.CStr "lu";
    "PRId64", A.CStr "ld";
    "PRIx64", A.CStr "lx";
    "PRIX64", A.CStr "lX";
    "PRIo64", A.CStr "lo";
  ]

let default_include_dirs = ["/usr/include"; "/usr/local/include"]

let processed_headers : (string, unit) Hashtbl.t = Hashtbl.create 64

let define_constants : A.const StrMap.t ref = ref default_predefined_define_constants

let reset_define_constants () =
  define_constants := default_predefined_define_constants

let remove_define_constant name =
  define_constants := StrMap.remove name !define_constants

let remember_define_constant name value =
  define_constants := StrMap.add name value !define_constants

let resolve_defined_string name =
  match StrMap.find_opt name !define_constants with
  | Some (A.CStr s) -> Some s
  | _ -> None

let reset_processed_headers () =
  Hashtbl.clear processed_headers

let include_dirs : string list ref = ref default_include_dirs

let set_include_dirs dirs =
  include_dirs := dirs;
  reset_processed_headers ()

let resolve_header relative_path =
  List.find_map (fun dir ->
    let full = Filename.concat dir relative_path in
    if Sys.file_exists full then Some full else None
  ) !include_dirs

(* These callbacks are the small part of Parser that Preprocessor still needs
   to re-enter recursively. Shared stateless helpers live in Parser_utils;
   only the functions that would otherwise create a Parser <-> Preprocessor
   module cycle stay injectable here. *)
type parser_callbacks = {
  parse_string : string -> (CST.translation_unit, CST.extra) Parsing_result.t;
  top_level_statement : top_level_statement -> A.e;
  parse_file : string -> (CST.translation_unit, CST.extra) Parsing_result.t;
  parse_translation_unit : translation_unit -> A.top_level_unit list;
}

let preproc_define_name ((_, name_tok, _, _) : preproc_def) =
  U.token_to_string name_tok

let preproc_ifdef_is_ifndef = function
  | `Pat_25b90ba _ -> false
  | `Pat_9d92f6a _ -> true

let preproc_elifdef_is_ifndef = function
  | `Pat_0307ca2 _ -> false
  | `Pat_a6d4183 _ -> true

let rec eval_preproc_expression defines (expr : preproc_expression) : int =
  match expr with
  | `Id (_loc, name) ->
      if StrSet.mem name defines then 1 else 0
  | `Num_lit (_loc, s) ->
      let core = U.strip_int_suffix s in
      (try int_of_string core with Failure _ -> 0)
  | `Char_lit char_lit ->
      Option.value (U.eval_char_literal char_lit) ~default:0
  | `Prep_defi defined ->
      let name = match defined with
        | `Defi_LPAR_id_RPAR (_, _, (_loc, name), _) -> name
        | `Defi_id (_, (_loc, name)) -> name
      in
      if StrSet.mem name defines then 1 else 0
  | `Prep_un_exp (op, sub_expr) ->
      let v = eval_preproc_expression defines sub_expr in
      (match op with
       | `BANG _ -> if v = 0 then 1 else 0
       | `TILDE _ -> lnot v
       | `DASH _ -> -v
       | `PLUS _ -> v)
  | `Prep_bin_exp bin_expr ->
      eval_preproc_binary defines bin_expr
  | `Prep_paren_exp (_, sub_expr, _) ->
      eval_preproc_expression defines sub_expr
  | `Prep_call_exp ((_loc, _name), _args) ->
      0

and eval_preproc_binary defines (bin_expr : preproc_binary_expression) : int =
  let eval = eval_preproc_expression defines in
  match bin_expr with
  | `Prep_exp_PLUS_prep_exp (l, _, r) -> eval l + eval r
  | `Prep_exp_DASH_prep_exp (l, _, r) -> eval l - eval r
  | `Prep_exp_STAR_prep_exp (l, _, r) -> eval l * eval r
  | `Prep_exp_SLASH_prep_exp (l, _, r) ->
      let rv = eval r in if rv = 0 then 0 else eval l / rv
  | `Prep_exp_PERC_prep_exp (l, _, r) ->
      let rv = eval r in if rv = 0 then 0 else eval l mod rv
  | `Prep_exp_BARBAR_prep_exp (l, _, r) ->
      if eval l <> 0 then 1 else if eval r <> 0 then 1 else 0
  | `Prep_exp_AMPAMP_prep_exp (l, _, r) ->
      if eval l = 0 then 0 else if eval r <> 0 then 1 else 0
  | `Prep_exp_BAR_prep_exp (l, _, r) -> eval l lor eval r
  | `Prep_exp_HAT_prep_exp (l, _, r) -> eval l lxor eval r
  | `Prep_exp_AMP_prep_exp (l, _, r) -> eval l land eval r
  | `Prep_exp_EQEQ_prep_exp (l, _, r) -> if eval l = eval r then 1 else 0
  | `Prep_exp_BANGEQ_prep_exp (l, _, r) -> if eval l <> eval r then 1 else 0
  | `Prep_exp_GT_prep_exp (l, _, r) -> if eval l > eval r then 1 else 0
  | `Prep_exp_GTEQ_prep_exp (l, _, r) -> if eval l >= eval r then 1 else 0
  | `Prep_exp_LTEQ_prep_exp (l, _, r) -> if eval l <= eval r then 1 else 0
  | `Prep_exp_LT_prep_exp (l, _, r) -> if eval l < eval r then 1 else 0
  | `Prep_exp_LTLT_prep_exp (l, _, r) -> eval l lsl eval r
  | `Prep_exp_GTGT_prep_exp (l, _, r) -> eval l asr eval r

(* Flatten [#if]/[#ifdef]/[#elif]/[#else] inside a [block_item list] by picking
   the active branch using [eval_preproc_expression] and [known_defines]. The
   parser calls this on function-body block lists so each [#if/#else] inside
   a function body is reduced to the single set of statements it would expand
   to under the C preprocessor, rather than being silently substituted with a
   [CNull] body (which is what happens if the unresolved directive is fed to
   the body inference). [#define]/[#undef]/[#include] inside a function body
   are not handled here — they're rare and stay as inert [block_item]s that
   [aux_block_item] already drops. *)
let rec expand_block_items items =
  List.concat_map expand_block_item items

and expand_block_item (item : block_item) : block_item list =
  match item with
  | `Prep_if (_, condition, _, body, else_opt, _) ->
      let value = eval_preproc_expression (known_defines ()) condition in
      if value <> 0 then expand_block_items body
      else expand_else_branch else_opt
  | `Prep_ifdef (directive, name_tok, body, else_opt, _) ->
      let name = U.token_to_string name_tok in
      let is_defined = StrSet.mem name (known_defines ()) in
      let take_body =
        if preproc_ifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then expand_block_items body
      else expand_else_branch else_opt
  | other -> [other]

and expand_else_branch = function
  | None -> []
  | Some (`Prep_else (_, body)) -> expand_block_items body
  | Some (`Prep_elif_5b2d46e (_, condition, _, body, else_opt)) ->
      let value = eval_preproc_expression (known_defines ()) condition in
      if value <> 0 then expand_block_items body
      else expand_else_branch else_opt
  | Some (`Prep_elif_b56056c (directive, name_tok, body, else_opt)) ->
      let name = U.token_to_string name_tok in
      let is_defined = StrSet.mem name (known_defines ()) in
      let take_body =
        if preproc_elifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then expand_block_items body
      else expand_else_branch else_opt

let rec strip_outer_parens s =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then
    strip_outer_parens (String.sub s 1 (len - 2))
  else s

let is_c_identifier s =
  let n = String.length s in
  n > 0
  && (match s.[0] with 'A'..'Z' | 'a'..'z' | '_' -> true | _ -> false)
  && (let rec ok i =
        i = n
        ||
        (match s.[i] with
         | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ok (i + 1)
         | _ -> false)
      in ok 1)

(* Whether a parsed macro body behaves like a function body: control never
   leaves it other than by completing ([return], [goto], or a [break] /
   [continue] not enclosed by a loop or [switch] of the body itself), and it
   only assigns its parameters, its own locals and the globals declared before
   it (e.g. rlang's [r_preserve] assigns [_r_placeholder]). Otherwise its meaning
   depends on the expansion site (the caller's labels, loops and locals):
   e.g. utf8's [#define TRY(x) do { if ((err = (x))) goto exit; } while (0)].
   Labels are not represented after parsing, so every [goto] leaves the body. *)
let behaves_like_function params body =
  let rec escapes ~in_loop ~in_switch e =
    match snd e with
    | A.Return _ | A.Goto _ -> true
    | A.Break -> not (in_loop || in_switch)
    | A.Next -> not in_loop
    | A.While _ | A.For _ ->
        List.exists (escapes ~in_loop:true ~in_switch) (A.children e)
    | A.Switch _ ->
        List.exists (escapes ~in_loop ~in_switch:true) (A.children e)
    | _ -> List.exists (escapes ~in_loop ~in_switch) (A.children e)
  in
  let rec root (_, e) =
    match e with
    | A.Id s -> [ s ]
    | A.FieldAccess (e, _) -> root e
    | _ -> []
  in
  let rec fold (written, declared) e =
    let acc =
      match snd e with
      | A.VarAssign (lhs, _)
      | A.Unop (("pre++" | "pre--" | "post++" | "post--"), lhs) ->
          (root lhs @ written, declared)
      | A.VarDeclare (_, (_, A.Id s)) -> (written, s :: declared)
      | _ -> (written, declared)
    in
    List.fold_left fold acc (A.children e)
  in
  let written, declared = fold ([], []) body in
  let own =
    StrSet.union (A.bv_params params) (StrSet.of_list declared)
    |> StrSet.union !globals
  in
  (not (escapes ~in_loop:false ~in_switch:false body))
  && List.for_all (fun s -> StrSet.mem s own) written

let parse_preproc_function_def helpers (func_def : preproc_function_def) =
  let ((loc1, _), name, params, body, (loc2, _)) = func_def in
  let name = U.token_to_string name in
  let _, params, (end_param, _) = params in
  (* Mirror how C variadic parameters are handled in [Parser.aux_param]
     ([`Vari_param _ -> A.Vararg]): a [...] in a function-like macro becomes
     [A.Vararg] so the macro types as [any -> ret] (see
     [PAst.param.Vararg]'s doc comment and [C_interface.is_variadic_params]).

     A macro body is an expression or a statement, and either only makes sense
     inside a function: it is parsed as the body of a wrapper function, first
     as an expression ([return (BODY);], the macro then returning its value,
     e.g. [#define KEEP(x) PROTECT(x)] or [(x)]), else as a statement ([BODY;],
     e.g. [do { ... } while (0)]). A body that parses but does not behave like
     a function body ([behaves_like_function]) depends on the expansion site:
     the macro is left unbound. When neither parses, fall back to a
     declaration-style [Seq []] body. Combined with the
     [--fallback-c-signature] pipeline default, the macro still binds at
     [any -> any] instead of being dropped — preventing the cascade of
     "unbound variable" errors at call sites. *)
  let aux_prep_param (p : anon_choice_stmt_id_d3c4b5f) : A.param =
    match p with
    | `Id (_, s) -> A.Param (Ast.Any, s)
    | `DOTDOTDOT _ -> A.Vararg
  in
  try
    let params =
      Option.fold ~none:[] ~some:(fun (p1, others) ->
        aux_prep_param p1 :: List.map (fun (_, p) -> aux_prep_param p) others
      ) params
    in
    let pos_body = U.locs_to_pos end_param loc2 in
    let declaration_body = (pos_body, A.Seq []) in
    (* The body of [wrapper] when [src] parses without error. *)
    let wrapper = "__nativesem_macro_body" in
    let parse_in_function src =
      let res = helpers.parse_string src in
      if res.stat.error_count > 0 then None
      else
        match res.program with
        | None -> None
        | Some tu ->
            helpers.parse_translation_unit tu
            |> List.find_map (function
                | _, A.Fundef (_, n, _, body) when n = wrapper -> Some body
                | _ -> None)
    in
    Hashtbl.remove expansions name ;
    let body = match body with
      | None -> Some (pos_body, A.Return None)
      | Some (_loc3, comp) ->
          let as_expression = Printf.sprintf "int %s(void) { return (%s); }" wrapper comp in
          let as_statement = Printf.sprintf "int %s(void) { %s; }" wrapper comp in
          let function_like (body, inner) =
            if behaves_like_function params inner then Some body
            else begin
              (* A statement body expands in place: textual expansion opens no
                 scope. A body with a [goto] stays unbound: [goto] is not
                 modeled, so its expansion would follow the paths after the
                 jump as if it had not happened (utf8's [TRY]). *)
              let inner = match inner with
                | pos, A.Block stmts -> (pos, A.Seq stmts)
                | e -> e
              in
              let rec has_goto (_, e as node) =
                match e with
                | A.Goto _ -> true
                | _ -> List.exists has_goto (A.children node)
              in
              if not (has_goto inner) then
                Hashtbl.replace expansions name (params, inner) ;
              None
            end
          in
          (match parse_in_function as_expression with
           | Some ((_, A.Return (Some e)) | (_, A.Block [ (_, A.Return (Some e)) ])
                   as body) ->
               function_like (body, e)
           | _ ->
               (match parse_in_function as_statement with
                | Some ((pos, _) as body) ->
                    (* A statement macro has no value. *)
                    let stmt = (pos, A.Seq [ body ; (pos, A.Return None) ]) in
                    (match function_like (stmt, body) with
                     | Some _ as b -> b
                     | None -> None)
                | None -> Some declaration_body))
    in
    Option.map (fun body ->
      (U.locs_to_pos loc1 loc2, A.MacroDef (name, params, body))) body
  with
  | Failure msg ->
      if !warn_unsupported then
        Printf.printf "Not supported yet: failed to parse preprocessor function body for %s: %s\n" name msg;
      None

(* The expansion of [NAME(args)] when [NAME] is in [expansions]: its body,
   the arguments substituted for the parameters (several times if a parameter
   occurs several times, as in C). [__VA_ARGS__] stands for the arguments
   after the named ones, spliced into an argument list. A parameter also
   names a field ([p->F]) or a declared variable when its argument is an
   identifier; when it is not, or with a wrong number of arguments, there is
   no expansion. *)
let expand_macro name args =
  match Hashtbl.find_opt expansions name with
  | None -> None
  | Some (params, body) ->
      let named = List.filter_map (function A.Param (_, n) -> Some n | A.Vararg -> None) params in
      let variadic = List.mem A.Vararg params in
      let n = List.length named in
      if List.length args < n || (not variadic && List.length args > n) then None
      else
        let bound = List.combine named (List.filteri (fun i _ -> i < n) args) in
        let extra = List.filteri (fun i _ -> i >= n) args in
        let ident_of p = match List.assoc_opt p bound with
          | Some (_, A.Id a) -> Some (Some a)
          | Some _ -> Some None
          | None -> None
        in
        let ok = ref true in
        let splice args =
          List.concat_map (function
            | (_, A.Id "__VA_ARGS__") -> extra
            | a -> [ a ]) args
        in
        let subst (pos, e) =
          let e = match e with
            | A.Call (f, args) -> A.Call (f, splice args)
            | A.MacroCall (m, args) -> A.MacroCall (m, splice args)
            | A.FieldAccess (r, f) ->
                (match ident_of f with
                 | Some (Some a) -> A.FieldAccess (r, a)
                 | Some None -> ok := false ; e
                 | None -> e)
            | A.VarDeclare (ty, (p, A.Id d)) ->
                (match ident_of d with
                 | Some (Some a) -> A.VarDeclare (ty, (p, A.Id a))
                 | Some None -> ok := false ; e
                 | None -> e)
            | e -> e
          in
          (pos, e)
        in
        (* Parameters first, top-down: a declared name or a field is not an
           occurrence of the parameter as a value. *)
        let rec values (pos, e) =
          match e with
          | A.Id p when List.mem_assoc p bound -> List.assoc p bound
          | A.Id "__VA_ARGS__" when extra <> [] ->
              List.fold_left (fun acc a -> (pos, A.Comma (acc, a))) (List.hd extra) (List.tl extra)
          | A.Call (f, args) ->
              (pos, A.Call (values f, List.concat_map (function
                 | (_, A.Id "__VA_ARGS__") as a -> [ a ]
                 | a -> [ values a ]) args))
          | A.MacroCall (m, args) ->
              (pos, A.MacroCall (m, List.concat_map (function
                 | (_, A.Id "__VA_ARGS__") as a -> [ a ]
                 | a -> [ values a ]) args))
          | A.FieldAccess (r, f) -> (pos, A.FieldAccess (values r, f))
          | A.VarDeclare (ty, d) -> (pos, A.VarDeclare (ty, d))
          | _ -> A.map_children values (pos, e)
        in
        let body = values body |> A.map subst in
        if !ok then Some body else None

let parse_preproc_define (_callbacks : parser_callbacks) (prepoc : preproc_def) : A.top_level_unit option =
  let ((loc1, _), name_tok, value_opt, _nl) = prepoc in
  let name = U.token_to_string name_tok in
  let parse_char_literal s =
    let len = String.length s in
    if len = 3 && s.[0] = '\'' && s.[2] = '\'' then Some (A.CChar s.[1])
    else None
  in
  let parse_define_value s =
    let s = strip_outer_parens s in
    let len = String.length s in
    if len = 0 then None
    else
      match s with
      | "NULL" | "nullptr" -> Some A.CNull
      | "true" | "TRUE" -> Some (A.CBool true)
      | "false" | "FALSE" -> Some (A.CBool false)
      | _ when len >= 2 && s.[0] = '"' && s.[len - 1] = '"' ->
          Some (A.CStr (String.sub s 1 (len - 2)))
      | _ ->
          begin
            match parse_char_literal s with
            | Some c -> Some c
            | None -> U.parse_define_literal s
          end
  in
  match value_opt with
  | None -> None
  | Some (_, raw_value) ->
      let raw_value = String.trim raw_value in
      let stripped = strip_outer_parens raw_value in
      if is_c_identifier stripped && stripped <> name then begin
        A.remember_define_alias name stripped;
        None
      end else
      begin
        let try_const_eval () =
          match Const_eval.eval_define_string raw_value with
          | Some (Const_eval.VInt i) ->
              let c = A.CInt i in
              Const_eval.remember name (Const_eval.VInt i);
              Some (U.loc_to_pos loc1, A.Define (name, c))
          | Some (Const_eval.VFloat f) ->
              let c = A.CFloat f in
              Const_eval.remember name (Const_eval.VFloat f);
              Some (U.loc_to_pos loc1, A.Define (name, c))
          | None -> None
        in
        let mirror_const_eval = function
          | A.CInt i -> Const_eval.remember name (Const_eval.VInt i)
          | A.CFloat f -> Const_eval.remember name (Const_eval.VFloat f)
          | _ -> ()
        in
        begin try
          match parse_define_value raw_value with
          | Some c ->
              mirror_const_eval c;
              Some (U.loc_to_pos loc1, A.Define (name, c))
          | None ->
              begin match try_const_eval () with
              | Some _ as r -> r
              | None ->
                  if !warn_unsupported then
                    Printf.printf "Not supported yet: #define %s with empty/invalid value\n" name;
                  None
              end
        with Failure _ ->
          begin match try_const_eval () with
          | Some _ as r -> r
          | None ->
              if !warn_unsupported then
                Printf.printf "Not supported yet: #define %s with non-literal value `%s`\n" name raw_value;
              None
          end
        end
      end

(* Directories of the system headers being processed, innermost first. A
   quoted [#include "x.h"] inside a system header names a file next to it
   (curl.h pulls in "multi.h", "urlapi.h", ... this way); one in the package's
   own sources is not followed, since package mode parses every header of
   [src/] anyway. *)
let current_header_dirs : string list ref = ref []

let rec system_include callbacks (_loc, path) =
  system_include_relative callbacks (String.sub path 1 (String.length path - 2))

and system_include_relative callbacks relative =
  match resolve_header relative with
  | None ->
      Printf.eprintf "Could not find system header: %s\n" relative;
      []
  | Some full_path -> process_system_header callbacks full_path

(* A quoted include inside a system header: the including header's directory
   first, then the system search path, as a C compiler does. *)
and quoted_include_in_system_header callbacks dir relative =
  let next_to = Filename.concat dir relative in
  if Sys.file_exists next_to then process_system_header callbacks next_to
  else system_include_relative callbacks relative

and process_system_header callbacks full_path =
      if Hashtbl.mem processed_headers full_path then
        []
      else begin
        Hashtbl.add processed_headers full_path ();
        Printf.printf "Processing system header: %s\n" full_path;
        let cst = callbacks.parse_file full_path in
        (* Tree-sitter exposes parse failures on [stat.error_count] / [errors]
           rather than as CST nodes, so silent ERRORs would otherwise turn into
           empty Include items downstream and look like "the header had nothing
           to declare." Surface them explicitly with a sample location so the
           degradation is visible. *)
        if !warn_unsupported && cst.stat.error_count > 0 then begin
          let sample =
            match cst.errors with
            | e :: _ ->
                let r = e.Tree_sitter_error.start_pos.row + 1 in
                Printf.sprintf " (first at line %d)" r
            | [] -> ""
          in
          Printf.eprintf
            "Parse errors in system header %s: %d node(s) dropped%s\n"
            full_path cst.stat.error_count sample
        end;
        match cst.program with
        | None -> []
        | Some tree ->
          current_header_dirs := Filename.dirname full_path :: !current_header_dirs;
          let items =
            Fun.protect
              ~finally:(fun () -> current_header_dirs := List.tl !current_header_dirs)
              (fun () -> callbacks.parse_translation_unit tree)
          in
          [Mlsem.Common.Position.dummy, A.Include items]
      end

let fold_top_level_items aux_item defines items =
  let defines, rev_items =
    List.fold_left
      (fun (defines, rev_items) item ->
        let defines, current = aux_item defines item in
        (defines, List.rev_append (List.rev current) rev_items))
      (defines, []) items
  in
  (defines, List.rev rev_items)

let rec top_level_items ~parser_callbacks ~top_level_item ~top_level_block_item defines items =
  fold_top_level_items
    (top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item)
    defines items

and top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines items =
  fold_top_level_items
    (top_level_block_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item)
    defines items

and recover_cplusplus_linkage_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body =
  (* Tree-sitter parses the common guarded linkage wrapper
     [#ifdef __cplusplus extern "C" { #endif ... #ifdef __cplusplus } #endif]
     as a [Link_spec] whose declaration list contains the C declarations.
     In C mode the [__cplusplus] branch is false, but those declarations are
     not semantically inside the conditional after preprocessing. Recover only
     that structural parser artifact, and keep dropping any other C++-only
     branch content. *)
  let linkage_items =
    List.filter (function `Link_spec _ -> true | _ -> false) body
  in
  top_level_block_items
    ~parser_callbacks ~top_level_item ~top_level_block_item
    defines linkage_items

and top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch =
  match else_branch with
  | `Prep_else (_else_tok, body) ->
      top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
  | `Prep_elif_5b2d46e (_elif_tok, condition, _newline, body, else_opt) ->
      let value = eval_preproc_expression defines condition in
      if value <> 0 then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)
  | `Prep_elif_b56056c (directive, name_tok, body, else_opt) ->
      let name = U.token_to_string name_tok in
      let is_defined = StrSet.mem name defines in
      let take_body =
        if preproc_elifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else if name = "__cplusplus" && Option.is_none else_opt then
        recover_cplusplus_linkage_items
          ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)

and top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines item =
  match item with
  | `Prep_ifdef (directive, name_tok, body, else_opt, _endif_tok) ->
      let name = U.token_to_string name_tok in
      let is_defined = StrSet.mem name defines in
      let take_body =
        if preproc_ifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else if name = "__cplusplus" && Option.is_none else_opt then
        recover_cplusplus_linkage_items
          ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)
  | `Prep_if (_if_tok, condition, _newline, body, else_opt, _endif_tok) ->
      let value = eval_preproc_expression defines condition in
      if value <> 0 then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)
  | `Prep_def prepoc_def ->
      let name = preproc_define_name prepoc_def in
      if StrMap.mem name default_predefined_define_constants then
        (defines, [])
      else
        let defines = StrSet.add name defines in
        remember_seen_define_name name;
        remove_define_constant name;
        A.remove_define_alias name;
        let item = parse_preproc_define parser_callbacks prepoc_def in
        begin
          match item with
          | Some (_, A.Define (name, value)) -> remember_define_constant name value
          | _ -> ()
        end;
        (defines,
         match item with
         | None -> []
         | Some item -> [item])
  | `Prep_func_def func_def ->
      let (_, name_tok, _, _, _) = func_def in
      let name = U.token_to_string name_tok in
      remember_seen_define_name name;
      let item = parse_preproc_function_def parser_callbacks func_def in
      function_macros := StrSet.add name !function_macros;
      (defines,
       match item with
       | None -> []
       | Some item -> [item])
  | `Prep_incl (_, `System_lib_str path, _) ->
      (defines, system_include parser_callbacks path)
  | `Prep_incl (_, `Str_lit (_, parts, _), _) when !current_header_dirs <> [] ->
      let relative =
        parts |> List.map (function
          | `Imm_tok_prec_p1_pat_c7f65b4 tok | `Esc_seq tok -> U.token_to_string tok)
        |> String.concat ""
      in
      (defines,
       quoted_include_in_system_header parser_callbacks (List.hd !current_header_dirs) relative)
  | `Prep_incl include_item ->
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: non-system include\n";
        Boilerplate.map_top_level_item () (`Prep_incl include_item) |> Raw_tree.to_channel stdout
      end;
      (defines, [])
  | `Prep_call (directive_tok, arg_opt, _nl) ->
      (* Tree-sitter emits [Prep_call] for any preprocessor directive it has no
         specific grammar rule for: [#endif] stranded by mispaired
         [#ifdef]/[#endif] (most commonly the [extern "C"] linkage guards seen
         in CRAN headers), plus [#pragma], [#warning], [#error], [#line],
         [#ident], [#include_next], [#undef], etc. Most of these are inert for
         type checking and we drop them silently; [#undef] is the exception,
         undoing a [#define]. *)
      let directive =
        U.token_to_string directive_tok
        |> String.trim
        |> fun s ->
          if String.length s > 0 && s.[0] = '#'
          then String.trim (String.sub s 1 (String.length s - 1))
          else s
      in
      (match directive, arg_opt with
       | "undef", Some (_, arg) ->
           (* The name is no longer defined: [#ifdef] is false again, a
              constant or alias is forgotten, and a function-like macro stops
              being one, so later [NAME(...)] are calls again (vctrs [#undef]s
              its code-generating macros after use). *)
           let name = String.trim arg in
           function_macros := StrSet.remove name !function_macros;
           Hashtbl.remove expansions name;
           seen_define_names := StrSet.remove name !seen_define_names;
           remove_define_constant name;
           A.remove_define_alias name;
           (StrSet.remove name defines, [])
       | _ -> (defines, []))
  | _ ->
      top_level_item defines item

  and top_level_block_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines item =
  match item with
  | `Prep_if prep_if ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_if prep_if)
  | `Prep_ifdef prep_ifdef ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_ifdef prep_ifdef)
  | `Prep_incl prep_include ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_incl prep_include)
  | `Prep_def prep_def ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_def prep_def)
  | `Prep_func_def prep_func_def ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_func_def prep_func_def)
  | `Prep_call prep_call ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_call prep_call)
    | _ ->
      top_level_block_item defines item

let with_test_state f =
  let saved_warn = !warn_unsupported in
  let saved_predefined = !predefined_defines in
  let saved_include_dirs = !include_dirs in
  let saved_define_constants = !define_constants in
  let saved_const_eval = !(Const_eval.constants) in
  let saved_seen_defines = !seen_define_names in
  let saved_globals = !globals in
  Fun.protect
    ~finally:(fun () ->
      warn_unsupported := saved_warn;
      predefined_defines := saved_predefined;
      include_dirs := saved_include_dirs;
      define_constants := saved_define_constants;
      Const_eval.constants := saved_const_eval;
      seen_define_names := saved_seen_defines;
      globals := saved_globals;
      reset_processed_headers ();
      A.reset_define_aliases ())
    (fun () ->
      set_warn_unsupported false;
      reset_processed_headers ();
      A.reset_define_aliases ();
      reset_define_constants ();
      Const_eval.reset ();
      reset_seen_define_names ();
      globals := StrSet.empty;
      f ())


let parse_translation_unit_exn source =
  match Parse.string source with
  | { Parsing_result.program = Some translation_unit; _ } -> translation_unit
  | _ -> failwith "Failed to parse test translation unit"


let first_top_level_item_exn source =
  match parse_translation_unit_exn source with
  | item :: _ -> item
  | [] -> failwith "Expected a top-level item"


let first_preproc_if_condition_exn source =
  match first_top_level_item_exn source with
  | `Prep_if (_, condition, _, _, _, _) -> condition
  | _ -> failwith "Expected a #if item"


let first_preproc_define_exn source =
  match first_top_level_item_exn source with
  | `Prep_def define -> define
  | _ -> failwith "Expected a #define item"


let make_test_callbacks () =
  let top_level_item_stub defines _ = (defines, []) in
  let top_level_block_item_stub defines _ = (defines, []) in
  let top_level_statement_stub _ = (Mlsem.Common.Position.dummy, A.Seq []) in
  let rec parse_translation_unit translation_unit =
    snd
      (top_level_items
         ~parser_callbacks
         ~top_level_item:top_level_item_stub
         ~top_level_block_item:top_level_block_item_stub
         (current_predefined_defines ())
         translation_unit)
  and parser_callbacks =
    {
      parse_string = Parse.string;
      top_level_statement = top_level_statement_stub;
      parse_file = Parse.file;
      parse_translation_unit;
    }
  in
  parser_callbacks


let preprocess_source_with_defines defines source =
  let parser_callbacks = make_test_callbacks () in
  snd
    (top_level_items
       ~parser_callbacks
       ~top_level_item:(fun defines _ -> (defines, []))
       ~top_level_block_item:(fun defines _ -> (defines, []))
       defines
       (parse_translation_unit_exn source))


let strip_positions items =
  List.map snd items


let with_temp_header header_name contents f =
  let dir = Filename.temp_file "rctyping-preprocessor" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let header_path = Filename.concat dir header_name in
  let oc = open_out header_path in
  output_string oc contents;
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      Sys.remove header_path;
      Unix.rmdir dir)
    (fun () -> f dir)


let%test "eval_preproc_expression handles identifiers unary operators and chars" =
  with_test_state (fun () ->
    let expr =
      first_preproc_if_condition_exn
        "#if FOO && !BAZ && ('A' == 65) && (~0 < 0) && (+3 == 3) && (-1 < 0)\n#endif\n"
    in
    eval_preproc_expression (StrSet.of_list ["FOO"]) expr <> 0)


let%test "eval_preproc_expression covers arithmetic logical bitwise and shifts" =
  with_test_state (fun () ->
    let expr =
      first_preproc_if_condition_exn
        "#if ((1 + 2) == 3) && ((5 - 3) == 2) && ((2 * 3) == 6) && ((7 / 0) == 0) && ((7 % 0) == 0) && ((1 || 0) == 1) && ((1 && 2) == 1) && ((1 | 2) == 3) && ((3 ^ 1) == 2) && ((3 & 1) == 1) && ((2 == 2) == 1) && ((2 != 3) == 1) && ((3 > 2) == 1) && ((3 >= 3) == 1) && ((2 <= 2) == 1) && ((2 < 3) == 1) && ((1 << 3) == 8) && ((8 >> 1) == 4)\n#endif\n"
    in
    eval_preproc_expression StrSet.empty expr <> 0)


let%test "parse_preproc_define recognizes numeric string and char literals" =
  with_test_state (fun () ->
  (match parse_preproc_define (make_test_callbacks ()) (first_preproc_define_exn "#define COUNT 12\n") with
   | Some (_, A.Define ("COUNT", A.CInt 12)) -> true
     | _ -> false)
  && (match parse_preproc_define (make_test_callbacks ()) (first_preproc_define_exn "#define MSG \"ok\"\n") with
    | Some (_, A.Define ("MSG", A.CStr "ok")) -> true
        | _ -> false)
    && (match parse_preproc_define (make_test_callbacks ()) (first_preproc_define_exn "#define CH 'x'\n") with
        | Some (_, A.Define ("CH", A.CChar 'x')) -> true
        | _ -> false))


let%test "top_level_items records string constants and identifier aliases" =
  with_test_state (fun () ->
    let items =
      preprocess_source_with_defines
        StrSet.empty
        "#define GREETING \"hi\"\n#define HELLO GREETING\n"
    in
      strip_positions items = [A.Define ("GREETING", A.CStr "hi")]
    && resolve_defined_string "GREETING" = Some "hi"
    && A.resolve_alias "HELLO" = "GREETING")


let%test "top_level_items skips predefined fallback constants and folds arithmetic" =
  with_test_state (fun () ->
    let items = preprocess_source_with_defines StrSet.empty "#define PRIu64 12\n#define SUM (1 + 2)\n" in
    strip_positions items = [A.Define ("SUM", A.CInt 3)]
    && resolve_defined_string "PRIu64" = Some "lu")


let%test "top_level_items selects branches for ifdef ifndef and elif" =
  with_test_state (fun () ->
    let ifdef_items =
      preprocess_source_with_defines
        (StrSet.of_list ["FOO"])
        "#ifdef FOO\n#define YES 1\n#else\n#define NO 0\n#endif\n"
    in
    let ifndef_items =
      preprocess_source_with_defines
        StrSet.empty
        "#ifndef FOO\n#define MISSING 1\n#else\n#define PRESENT 1\n#endif\n"
    in
    let elif_items =
      preprocess_source_with_defines
        StrSet.empty
        "#if 0\n#define NOPE 0\n#elif 1\n#define YES 2\n#else\n#define ALT 3\n#endif\n"
    in
    strip_positions ifdef_items = [A.Define ("YES", A.CInt 1)]
    && strip_positions ifndef_items = [A.Define ("MISSING", A.CInt 1)]
    && strip_positions elif_items = [A.Define ("YES", A.CInt 2)])


let%test "top_level_items ignores non-system includes" =
  with_test_state (fun () ->
    preprocess_source_with_defines StrSet.empty "#include \"local.h\"\n" = [])


let%test "top_level_items silently drops stray and unsupported directives" =
  with_test_state (fun () ->
    let source =
      "#endif\n#pragma once\n#warning hello\n#error oops\n#line 42 \"x.c\"\n#ident \"$Id$\"\n"
    in
    preprocess_source_with_defines StrSet.empty source = [])


let contains_substring ~haystack ~needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec scan i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else scan (i + 1)
    in
    scan 0


let capture_stdout f =
  flush stdout;
  let saved = Unix.dup Unix.stdout in
  let read_fd, write_fd = Unix.pipe () in
  Unix.dup2 write_fd Unix.stdout;
  Unix.close write_fd;
  let result = Fun.protect
    ~finally:(fun () ->
      flush stdout;
      Unix.dup2 saved Unix.stdout;
      Unix.close saved)
    f
  in
  let buf = Buffer.create 256 in
  let chunk = Bytes.create 1024 in
  let rec drain () =
    match Unix.read read_fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n -> Buffer.add_subbytes buf chunk 0 n; drain ()
    | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
  in
  Unix.set_nonblock read_fd;
  drain ();
  Unix.close read_fd;
  (result, Buffer.contents buf)


let%test "top_level_items honours #undef" =
  with_test_state (fun () ->
    (* How many ordinary items (here, [int x;]) survive preprocessing. *)
    let count source =
      let marker = (Mlsem.Common.Position.dummy, A.Include []) in
      let keep defines _ = (defines, [ marker ]) in
      let items, _ =
        capture_stdout (fun () ->
          snd (top_level_items ~parser_callbacks:(make_test_callbacks ())
                 ~top_level_item:keep ~top_level_block_item:keep
                 StrSet.empty (parse_translation_unit_exn source)))
      in
      List.length (List.filter (fun i -> i == marker) items)
    in
    count "#define FOO 1\n#ifdef FOO\nint x;\n#endif\n" = 1
    && count "#define FOO 1\n#undef FOO\n#ifdef FOO\nint x;\n#endif\n" = 0)


let%test "system includes are parsed once and cached" =
  with_test_state (fun () ->
    with_temp_header "sample.h" "#define FROM_HEADER 7\n" (fun dir ->
      set_include_dirs [dir];
      let items =
        preprocess_source_with_defines
          StrSet.empty
          "#include <sample.h>\n#include <sample.h>\n"
      in
      match items with
          | [_, A.Include nested_items] -> strip_positions nested_items = [A.Define ("FROM_HEADER", A.CInt 7)]
      | _ -> false))
