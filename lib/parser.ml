open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
open Mlsem.Common
module U = Parser_utils

module StrSet = Preprocessor.StrSet

let warn_unsupported = ref true

let set_warn_unsupported value =
  warn_unsupported := value;
  Preprocessor.set_warn_unsupported value

let default_include_dirs = Preprocessor.default_include_dirs

let set_predefined_defines = Preprocessor.set_predefined_defines

let set_include_dirs = Preprocessor.set_include_dirs

let print_res (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) = 
  Printf.printf "Errors: %d. Lines with errors: %d / %d\n" res.stat.error_count
    res.stat.error_line_count res.stat.total_line_count ;
    List.iter (fun e -> 
      Printf.printf "Error: %s\n" (Tree_sitter_error.to_string e)
    ) res.errors ;
   match res.program with
  | None -> ()
  | Some prog ->
    Boilerplate.dump_extras res.extras ;
    let tree = Boilerplate.map_translation_unit () prog in
    Raw_tree.to_channel stdout tree 

let print_top_level_item (item: top_level_item) =
  let tree = Boilerplate.map_top_level_item () item in
  Raw_tree.to_channel stdout tree

let process_res res = res

let empty_decl_prefix_macros = [
  "CURL_EXTERN";
]

(* Macros standing for [_Noreturn]: rewritten to it (padded to the same width)
   rather than blanked, so the parser knows the function never returns. *)
let noreturn_prefix_macros = [
  "r_no_return";
]

let is_c_ident_char = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> true
  | _ -> false

let token_matches_at source i token =
  let source_len = String.length source in
  let token_len = String.length token in
  i + token_len <= source_len
  && String.sub source i token_len = token
  && (i = 0 || not (is_c_ident_char source.[i - 1]))
  && (i + token_len = source_len || not (is_c_ident_char source.[i + token_len]))

(* True iff position [i] in [source] is the macro-name slot of a [#define]
   directive: scan back across whitespace, then across [#define], to a line
   start (or to file start). Used to keep [sanitize_empty_decl_prefix_macros]
   from stripping the token in lines like [#define r_no_return ...], which
   would otherwise reshape the directive (e.g. turning it into a function-like
   macro of whatever followed). *)
let preceded_by_define source i =
  let rec skip_ws j =
    if j < 0 then j
    else
      match source.[j] with
      | ' ' | '\t' -> skip_ws (j - 1)
      | _ -> j
  in
  (* [#define NAME], also with blanks between [#] and [define] as in
     [#  define CURL_EXTERN] (curl.h): blanking [NAME] there would leave an
     empty [#  define] and a parse error. *)
  let directive = "define" in
  let dlen = String.length directive in
  let after_ws = skip_ws (i - 1) in
  let end_of_directive = after_ws + 1 in
  let start_of_directive = end_of_directive - dlen in
  start_of_directive >= 0
  && String.sub source start_of_directive dlen = directive
  && (let hash = skip_ws (start_of_directive - 1) in
      hash >= 0 && source.[hash] = '#')

(* True iff position [i] is on a preprocessor directive line ([#ifndef FOO],
   [#undef FOO], a continuation line of a multi-line [#if]): blanking a macro
   name there would leave an empty directive. *)
let on_directive_line source i =
  let rec line_start j = if j > 0 && source.[j - 1] <> '\n' then line_start (j - 1) else j in
  (* The previous physical line, if it ends with a backslash continuation. *)
  let continued_from start =
    if start < 2 then None
    else
      let rec last_non_blank j = if j >= 0 && (source.[j] = ' ' || source.[j] = '\t' || source.[j] = '\r') then last_non_blank (j - 1) else j in
      let k = last_non_blank (start - 2) in
      if k >= 0 && source.[k] = '\\' then Some (line_start k) else None
  in
  let rec logical_start start =
    match continued_from start with Some s -> logical_start s | None -> start
  in
  let start = logical_start (line_start i) in
  let len = String.length source in
  let rec first j = if j < len && (source.[j] = ' ' || source.[j] = '\t') then first (j + 1) else j in
  let f = first start in
  f < len && source.[f] = '#'

let sanitize_empty_decl_prefix_macros source =
  let len = String.length source in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then
      ()
    else
      match List.find_opt (token_matches_at source i) empty_decl_prefix_macros with
      | Some token when not (preceded_by_define source i || on_directive_line source i) ->
          let token_len = String.length token in
          Buffer.add_string buf (String.make token_len ' ');
          loop (i + token_len)
      | _ ->
      match List.find_opt (token_matches_at source i) noreturn_prefix_macros with
      | Some token when not (preceded_by_define source i || on_directive_line source i) ->
          let kw = "_Noreturn" in
          let token_len = String.length token in
          Buffer.add_string buf kw;
          Buffer.add_string buf (String.make (max 0 (token_len - String.length kw)) ' ');
          loop (i + token_len)
      | _ ->
          Buffer.add_char buf source.[i];
          loop (i + 1)
  in
  loop 0;
  Buffer.contents buf

(* Index of the first occurrence of [sub] in [s]. *)
let find_sub s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec go i =
    if i + lsub > ls then None
    else if String.sub s i lsub = sub then Some i
    else go (i + 1)
  in
  go 0

(* [Some (directive, rest)] for a preprocessor line: [#  ifdef  FOO // x]
   gives [("ifdef", "FOO")]; comments after the directive are dropped. *)
let directive_of_line line =
  let line = String.trim line in
  if String.length line = 0 || line.[0] <> '#' then None
  else
    let body = String.trim (String.sub line 1 (String.length line - 1)) in
    let cut_at sep s =
      match find_sub s sep with Some i -> String.sub s 0 i | None -> s in
    let body = body |> cut_at "//" |> cut_at "/*" |> String.trim in
    let n = String.length body in
    let rec word_end i = if i < n && (match body.[i] with 'a'..'z' -> true | _ -> false) then word_end (i + 1) else i in
    let i = word_end 0 in
    Some (String.sub body 0 i, String.trim (String.sub body i (n - i)))

(* Blank out the C++-only blocks [#ifdef __cplusplus ... #endif] (also
   [#if defined(__cplusplus)]) that have no [#else]/[#elif]. NativeSem only
   reads C, where [__cplusplus] is never defined, and tree-sitter parses
   before any preprocessing: the [extern "C" {] such a block opens near the top
   of most library headers, closed by another such block at the end, reads as
   an unbalanced brace and the rest of the header is dropped (all of curl.h).
   Lines are replaced by blanks so positions are unchanged. *)
let blank_cplusplus_blocks source =
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let n = Array.length lines in
  let is_cplusplus_if = function
    | Some ("ifdef", "__cplusplus") -> true
    | Some ("if", ("defined(__cplusplus)" | "defined (__cplusplus)" | "defined __cplusplus")) -> true
    | _ -> false
  in
  (* Index of the [#endif] closing the block opened at [start], or [None] if
     the block has an [#else]/[#elif] or is not closed. *)
  let block_end start =
    let rec go i depth =
      if i >= n then None
      else match directive_of_line lines.(i) with
        | Some (("if" | "ifdef" | "ifndef"), _) -> go (i + 1) (depth + 1)
        | Some ("endif", _) -> if depth = 0 then Some i else go (i + 1) (depth - 1)
        | Some (("else" | "elif" | "elifdef" | "elifndef"), _) when depth = 0 -> None
        | _ -> go (i + 1) depth
    in
    go (start + 1) 0
  in
  let rec scan i =
    if i < n then
      if is_cplusplus_if (directive_of_line lines.(i)) then
        match block_end i with
        | Some j ->
            for k = i to j do lines.(k) <- String.make (String.length lines.(k)) ' ' done;
            scan (j + 1)
        | None -> scan (i + 1)
      else scan (i + 1)
  in
  scan 0;
  String.concat "\n" (Array.to_list lines)

let sanitize source = source |> blank_cplusplus_blocks |> sanitize_empty_decl_prefix_macros

(**
  Parse a file using FrontC and return the AST

  @param filename The path to the C file to parse
*)
let parse_file filename =
  let source = In_channel.with_open_text filename In_channel.input_all in
  let res = Parse.string (sanitize source) in
  process_res res

let parse_string s =
  let res = Parse.string (sanitize s) in 
  process_res res


  let gen_id = 
    let id =ref 0 in 
    fun () -> 
      let current_id = !id in 
      id := current_id + 1 ;
      current_id

(* Taken from E-Sh4rk/typed-r *)
let conv_pos = U.conv_pos


let loc_to_pos = U.loc_to_pos

(** Given to locations, create a position using the start of
   the first loc and the end of the second loc *)
let locs_to_pos = U.locs_to_pos

let exprs_to_pos (expr1: A.e) (expr2: A.e) : Position.t =
  let pos1, _ = expr1 in
  let pos2, _ = expr2 in
  Position.join pos1 pos2


let aux_primitive_type t =
  match t with
  | "void" -> Ast.Void
  | "char" -> Ast.Char
  | "short" | "int" | "long"
  | "size_t" | "ssize_t" | "ptrdiff_t" | "intptr_t" | "uintptr_t"
  | "int8_t" | "int16_t" | "int32_t" | "int64_t"
  | "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t"
  | "off_t" | "pid_t" | "mode_t" | "time_t" | "clock_t"
    -> Ast.Int
  | "float" | "double" -> Ast.Float
  | "bool" -> Ast.Bool
  | _ -> failwith ("Unknown primitive type: " ^ t)

let token_to_string (_loc, s) = s

type init_step =
  | InitIndex of A.e
  | InitField of string

type init_plan =
  | InitConst of A.const
  | InitDynamic of (init_step list * A.e) list

let rec placeholder_const_of_ctype ty =
  match ty with
  | Ast.Void | Ast.NoReturn -> A.CNull
  | Ast.Int -> A.CNa
  | Ast.Float -> A.CFloat 0.
  | Ast.Char -> A.CChar '\000'
  | Ast.Bool -> A.CBool false
  | Ast.Ptr _ | Ast.SEXP | Ast.Any | Ast.FunPtr _ -> A.CNull
  | Ast.Array (elem_ty, _) -> A.CArray [placeholder_const_of_ctype elem_ty]
  | Ast.Struct _ | Ast.Union _ | Ast.Enum _ | Ast.Typeref _ -> A.CNull

let top_level_initializer_count ((_, initializers, _, _) : initializer_list) =
  match initializers with
  | None -> 0
  | Some (_, inits) -> 1 + List.length inits

let parse_c_int_literal = U.parse_c_int_literal

let int_of_char_literal = U.int_of_char_literal

type enum_counter =
  | EnumInit
  | EnumKnown of int
  | EnumUnknown

let rec aux_struct_fields name fields =
  (* The third return is [Some (params, variadic)] when a function declarator
     was seen, capturing the function-pointer signature so the caller can
     build an [Ast.FunPtr] field. In that case the returned [int] applies to
     the return type (outer [Poin_field_decl] wrappers); the inner
     parenthesized-pointer level is discarded because it just signals
     function-pointer-ness, not a return-type indirection. *)
  let rec aux_field_decl_name (decl : field_declarator)
      : int * string * (Ast.ctype list * bool) option =
    match decl with
    | `Id (_loc, s) -> (0, s, None)
    | `Poin_field_decl (_, _, _, _, d) ->
        let level, field_name, fp = aux_field_decl_name d in
        (level + 1, field_name, fp)
    | `Array_field_decl (d, _, _, _, _) ->
        let level, field_name, fp = aux_field_decl_name d in
        (level + 1, field_name, fp)
    | `Paren_field_decl (_, _, d, _) -> aux_field_decl_name d
    | `Attr_field_decl (d, _) -> aux_field_decl_name d
    | `Func_field_decl (inner_decl, param_list) ->
        let _, field_name, _ = aux_field_decl_name inner_decl in
        let param_tys, variadic = funptr_params_of_list param_list in
        (0, field_name, Some (param_tys, variadic))
  in
  let aux_field_decl (field_decl : field_declaration_declarator) =
    let first_decl, first_bits, other_decls = field_decl in
    let all_decls =
      (first_decl, first_bits)
      :: List.map (fun (_, decl, bits) -> (decl, bits)) other_decls
    in
    all_decls
  in
  (* We keep an explicit collector instead of a direct [List.map] because each
     input item can produce 0, 1, or many fields (e.g. preprocessor entries are
     skipped, and a declaration may contain several declarators), while also
     emitting warnings for unsupported cases. *)
  let rec collect acc = function
    | [] -> List.rev acc
    | item :: rest ->
        begin
          match item with
          | `Field_decl (decl_spec, field_decl, _attrs, _semi) ->
              let base_ty = aux_decl_spec decl_spec in
              let fields_from_item =
                match field_decl with
                | None -> []
                | Some fd ->
                    aux_field_decl fd
                    |> List.map (fun (decl, bits) ->
                           (match bits with
                           | None -> ()
                           | Some _ when !warn_unsupported ->
                               Printf.printf
                                 "Not supported yet: bitfield width is ignored in struct field declaration\n"
                           | Some _ -> ());
                           let level, field_name, fp = aux_field_decl_name decl in
                           let ty = match fp with
                             | None -> Ast.build_ptr level base_ty
                             | Some (params, variadic) ->
                                 let ret_ty = Ast.build_ptr level base_ty in
                                 Ast.FunPtr (ret_ty, params, variadic)
                           in
                           (ty, field_name))
              in
              collect (List.rev_append fields_from_item acc) rest
          | `Prep_def _ | `Prep_func_def _ | `Prep_call _
          | `Prep_if_in_field_decl_list _ | `Prep_ifdef_in_field_decl_list _ ->
              if !warn_unsupported then
                Printf.printf
                  "Not supported yet: preprocessor directives inside struct fields\n";
              collect acc rest
        end
  in
  Ast.Struct (name, collect [] fields)

and aux_struct struc = 
  match struc with 
  |`Id_opt_field_decl_list ((_loc, name), Some (_,fields,_)) -> aux_struct_fields name fields
  (* Anonymous struct: give it a fresh name. [Ast.typeof_ctype] and
     [PAst.resolve_ctype] detect recursion by struct name, so a shared name
     would make an anonymous struct nested in another one look like a
     back-edge to it (and the outer struct an uninhabited recursive type).
     '#' cannot occur in a C tag, so the name never clashes with a real one. *)
  | `Field_decl_list (_,fields,_) -> aux_struct_fields ("anon#" ^ string_of_int (gen_id ())) fields
  (* Named reference to an already declared struct, e.g. `struct Point p;`.
     We keep the struct tag and resolve it later through DeclMap. *)
  | `Id_opt_field_decl_list ((_loc, name), None) -> Ast.Struct (name, [])

and aux_enum_enumerator local_env counter ((name_tok, explicit_value) : enumerator) =
  let module CE = Const_eval in
  let name = token_to_string name_tok in
  let computed_value, next_counter, local_env' =
    match explicit_value with
    | Some (_, expr) ->
        begin match CE.eval_c_expression ~local_env expr with
        | Some (CE.VInt value) ->
            CE.remember name (CE.VInt value);
            (Some value, EnumKnown value,
             CE.StrMap.add name (CE.VInt value) local_env)
        | Some (CE.VFloat _) | None ->
            if !warn_unsupported then
              Printf.eprintf
                "Warning: enum enumerator %s has a non-constant initializer; value left unknown\n"
                name;
            (None, EnumUnknown, local_env)
        end
    | None ->
        begin match counter with
        | EnumInit ->
            CE.remember name (CE.VInt 0);
            (Some 0, EnumKnown 0,
             CE.StrMap.add name (CE.VInt 0) local_env)
        | EnumKnown previous ->
            let value = previous + 1 in
            CE.remember name (CE.VInt value);
            (Some value, EnumKnown value,
             CE.StrMap.add name (CE.VInt value) local_env)
        | EnumUnknown -> (None, EnumUnknown, local_env)
        end
  in
  ((name, computed_value), next_counter, local_env')

and aux_enum_collect local_env counter acc = function
  | [] -> (List.rev acc, counter, local_env)
  | item :: rest ->
      begin match item with
      | `Enum_COMMA (enumerator, _) ->
          let enumerator, counter, local_env =
            aux_enum_enumerator local_env counter enumerator
          in
          aux_enum_collect local_env counter (enumerator :: acc) rest
      | `Prep_if_in_enum_list _ | `Prep_ifdef_in_enum_list _ | `Prep_call_COMMA _ ->
          if !warn_unsupported then
            Printf.printf
              "Not supported yet: preprocessor directives inside enum declarations\n";
          aux_enum_collect local_env counter acc rest
      end

and aux_enum_fields name enum_list =
  let _, leading_items, trailing_item, _ = enum_list in
  let local_env = Const_eval.StrMap.empty in
  let enumerators, counter, local_env =
    aux_enum_collect local_env EnumInit [] leading_items
  in
  let enumerators =
    match trailing_item with
    | Some (`Enum enumerator) ->
        let enumerator, _, _ = aux_enum_enumerator local_env counter enumerator in
        enumerators @ [enumerator]
    | Some (`Prep_if_in_enum_list_no_comma _)
    | Some (`Prep_ifdef_in_enum_list_no_comma _)
    | Some (`Prep_call _) ->
        if !warn_unsupported then
          Printf.printf
            "Not supported yet: preprocessor directives inside enum declarations\n";
        enumerators
    | None -> enumerators
  in
  Ast.Enum (name, enumerators)

and aux_enum enum_spec =
  match enum_spec with
  | `Id_opt_COLON_prim_type_opt_enum_list ((_, name), _, Some enum_list) ->
      aux_enum_fields name enum_list
  | `Id_opt_COLON_prim_type_opt_enum_list ((_, name), _, None) ->
      Ast.Enum (name, [])
  | `Enum_list enum_list ->
      aux_enum_fields "" enum_list

and aux_sized_type_spec (_sized : sized_type_specifier) : Ast.ctype =
  let aux_choice_type = function
    | `Prim_type (_loc, s) -> aux_primitive_type s
    | `Id (_loc, s) when s = "SEXP" -> Ast.SEXP
    | `Id (_, s) -> Ast.Typeref s
  in
  match _sized with
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (_, Some t, _)
  | `Rep1_choice_signed_rep_type_qual_opt_choice_id_rep_choice_signed (_, _, Some t, _) ->
      aux_choice_type t
  (* E.g. "unsigned", "long long": defaults to integer family. *)
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (_, None, _)
  | `Rep1_choice_signed_rep_type_qual_opt_choice_id_rep_choice_signed (_, _, None, _) ->
      Ast.Int

and aux_type_spec (type_spec : type_specifier)  =
  match type_spec with
  | `Prim_type tok -> let (_loc, s) = tok in aux_primitive_type s
  | `Id (_loc, s) when s = "SEXP" -> Ast.SEXP
  | `Id (_, s) -> Ast.Typeref s
  | `Struct_spec (_,_, _,struc, _) -> aux_struct struc
  | `Union_spec (_, _, body, _) ->
      (* Same body as a struct; typed nominally like one ([Ast.Structs]). *)
      (match aux_struct body with
       | Ast.Struct (name, fields) -> Ast.Union (name, fields)
       | ty -> ty)
  | `Enum_spec (_, enum_spec, _) -> aux_enum enum_spec
  | `Sized_type_spec sized -> aux_sized_type_spec sized
  | _ ->
      (* Unknown type specifier (e.g. tree-sitter [Macro_type_spec] for
         [__typeof__(...)], [decltype(...)], [__attribute_nonnull__(...)]),
         common in glibc multiarch headers. Previously failwith'd; that
         crashed the whole package as soon as any header pulled one of these
         in via a struct field. Degrade to [Ast.Any] — downstream treats it
         as opaque, which is the right behavior for specifiers we cannot
         interpret. *)
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: type specifier; using Ast.Any\n";
        let tree = Boilerplate.map_type_specifier () type_spec in
        Raw_tree.to_channel stderr tree
      end;
      Ast.Any

and aux_decl_spec decl_spec = 
  let (before, type_spec, after) = decl_spec in
  let ty = aux_type_spec type_spec in
  if ty = Ast.Void && List.exists is_noreturn_modifier (before @ after) then Ast.NoReturn
  else ty

(* [_Noreturn], [noreturn] or [__attribute__((noreturn))] (also spelled
   [__noreturn__]) among a declaration's modifiers. *)
and is_noreturn_modifier = function
  | `Type_qual (`X__Nore _ | `Nore _) -> true
  | `Attr_spec (_, _, ((_, args, _) : argument_list), _) ->
      let rec is_noreturn_id (e : expression) =
        match e with
        | `Choice_cond_exp (`Id (_, ("noreturn" | "__noreturn__"))) -> true
        | `Choice_cond_exp (`Paren_exp (_, `Exp e, _)) -> is_noreturn_id e
        | _ -> false
      in
      let names_noreturn = function `Exp e -> is_noreturn_id e | `Comp_stmt _ -> false in
      (match args with
       | None -> false
       | Some (first, rest) -> List.exists names_noreturn (first :: List.map snd rest))
  | _ -> false

and aux_global_linkage decl_spec =
  let before, _, after = decl_spec in
  let is_static = function
    | `Stor_class_spec (`Static _) -> true
    | _ -> false
  in
  let is_extern = function
    | `Stor_class_spec (`Extern _) -> true
    | _ -> false
  in
  if List.exists is_static before || List.exists is_static after then
    A.Static
  else if List.exists is_extern before || List.exists is_extern after then
    A.Extern
  else
    A.Definition

(* For pointers, arrays, parenthesized and function-style abstract declarators.
   Arrays decay to pointers in expression contexts, so they add one pointer
   level like [Abst_poin_decl]. Function-pointer abstract declarators —
   "T (*)(P1, P2, …)" used in casts and sizeof — match the parenthesized
   pointer pattern below and produce an [Ast.FunPtr]; other [Abst_func_decl]
   shapes fall back to [Ast.Any]. *)
and aux_abstract_declarator (base_type: Ast.ctype) (abs_decl: abstract_declarator option) =
  let aux = function
    | `Abst_poin_decl (_,_,_,inner) ->
        let inner_ty = aux_abstract_declarator base_type inner in
        (* If the inner declarator already resolved to a function pointer,
           the outer [*] applies to the *return* type (e.g. "T* (*)(…)"
           parses as [Abst_poin_decl] around an [Abst_func_decl]). *)
        (match inner_ty with
         | Ast.FunPtr (ret, params, variadic) ->
             Ast.FunPtr (Ast.Ptr ret, params, variadic)
         | _ -> Ast.Ptr inner_ty)
    | `Abst_array_decl (decl,_,_,_,_) -> Ast.Ptr (aux_abstract_declarator base_type decl)
    | `Abst_paren_decl (_, _, decl, _) -> aux_abstract_declarator base_type (Some decl)
    | `Abst_func_decl (Some (`Abst_paren_decl (_, _, `Abst_poin_decl _, _)), param_list) ->
        (* Function-pointer abstract declarator: "T (*)(params)". The inner
           parenthesized pointer marks function-pointer-ness; [base_type]
           (plus any wrappers from outer [Abst_poin_decl]s the caller will
           apply) is the return type. *)
        let param_tys, variadic = funptr_params_of_list param_list in
        Ast.FunPtr (base_type, param_tys, variadic)
    | `Abst_func_decl (decl, _) ->
        if !warn_unsupported then
          Printf.eprintf
            "Not supported yet: function declarator in abstract declarator; approximating as an opaque pointer\n";
        aux_abstract_declarator Ast.Any decl
  in
  Option.fold ~none:base_type
    ~some:aux abs_decl

and aux_type_descriptor (type_desc: type_descriptor) : Ast.ctype =
  let (_qualifiers1, type_spec, _qualifiers2, abstract_decl) = type_desc in
  let base_type = aux_type_spec type_spec in
  (* For now, ignore qualifiers and abstract declarators
     TODO: Handle const, volatile, pointers, arrays, etc. *)
  aux_abstract_declarator base_type abstract_decl

and aux_decl_name (decl : declarator) : int * string =
  match decl with
  | `Id tok -> (0, token_to_string tok)
  | `Func_decl (decl, _, _, _) ->  aux_decl_name decl
  | `Poin_decl (_,_,_,_,decl) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  (* Array just adds a pointer indirection currently. We could also create an array type for the C side*)
  | `Array_decl (decl,_,_,_,_) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  | `Paren_decl (_, _, decl, _) -> aux_decl_name decl
  | _ -> Boilerplate.map_declarator () decl |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: function name"

(* True when [decl] is a parenthesized-pointer declarator wrapping any name,
   i.e. the function-pointer marker "(\*name)". *)
and is_parenthesized_pointer_declarator (decl : declarator) =
  match decl with
  | `Paren_decl (_, _, `Poin_decl _, _) -> true
  | `Attr_decl (decl, _) -> is_parenthesized_pointer_declarator decl
  | _ -> false

(* Third return is [Some (params, variadic)] when the declarator includes a
   function declarator (typedef of a function-pointer type). The [int] then
   counts only the outer pointer wrappers, which apply to the return type. *)
and aux_type_declarator (td : type_declarator)
    : int * string * (Ast.ctype list * bool) option =
  match td with
  | `Id tok -> (0, token_to_string tok, None)
  | `Poin_type_decl (_, _, _, _, inner) ->
      let (level, name, fp) = aux_type_declarator inner in (level + 1, name, fp)
  | `Array_type_decl (inner, _, _, _, _) ->
      let (level, name, fp) = aux_type_declarator inner in (level + 1, name, fp)
  | `Paren_type_decl (_, _, inner, _) -> aux_type_declarator inner
  | `Attr_type_decl (inner, _) -> aux_type_declarator inner
  | `Func_type_decl (inner, param_list) ->
      let _, name, _ = aux_type_declarator inner in
      let param_tys, variadic = funptr_params_of_list param_list in
      (0, name, Some (param_tys, variadic))
  | `Choice_signed _ | `Prim_type _ ->
      Printf.eprintf "Not supported yet: primitive in type declarator\n";
      (0, "__prim__", None)

and aux_type_definition ((_, _, type_def_ty, (first_decl, rest_decls), _, _) : type_definition) =
  let (before, type_spec, after) = type_def_ty in
  let base_ty = aux_type_spec type_spec in
  (* [typedef _Noreturn void ( *T)(...)] (rlang's [r_stop_internal_t]). *)
  let base_ty =
    if base_ty = Ast.Void
       && List.exists (fun q -> is_noreturn_modifier (`Type_qual q)) (before @ after)
    then Ast.NoReturn else base_ty in
  (* [typedef struct { ... } T;]: the anonymous struct (or union) takes the
     name [T], which its types print. Decided once, from the first plain
     declarator, so that [typedef struct { ... } T, *PT;] names one struct. *)
  let base_ty =
    match base_ty with
    | (Ast.Struct (anon, fields) | Ast.Union (anon, fields))
      when String.starts_with ~prefix:"anon#" anon ->
        let plain =
          first_decl :: List.map snd rest_decls
          |> List.find_map (fun td ->
              match aux_type_declarator td with
              | 0, name, None -> Some name
              | _ -> None)
        in
        (match plain, base_ty with
         | Some name, Ast.Union _ -> Ast.Union (name, fields)
         | Some name, _ -> Ast.Struct (name, fields)
         | None, _ -> base_ty)
    | _ -> base_ty
  in
  let aux_one td =
    let (ptr_level, name, fp) = aux_type_declarator td in
    let ty = match fp with
      | None -> Ast.build_ptr ptr_level base_ty
      | Some (params, variadic) ->
          let ret_ty = Ast.build_ptr ptr_level base_ty in
          Ast.FunPtr (ret_ty, params, variadic)
    in
    (Mlsem.Common.Position.dummy, A.TypeDecl (name, ty))
  in
  aux_one first_decl :: List.map (fun (_, td) -> aux_one td) rest_decls

(* The remaining helpers below are chained into the same recursion because
   parameter parsing now detects function-pointer parameter shapes:
   [aux_param] uses [function_pointer_global_signature] to recognise a
   function-pointer declarator and emit an [Ast.FunPtr] parameter type, and
   [function_pointer_global_signature] uses [aux_extract_params] (which
   iterates over [aux_param]) to recover the param ctype list. Struct fields
   and typedefs also reach into [funptr_params_of_list] for the same purpose. *)
and aux_param (p: anon_choice_param_decl_4ac2852) : A.param =
  match p with
  | `Param_decl (decl_spec, Some (`Decl decl), _) ->
    let base_ty = aux_decl_spec decl_spec in
    (match function_pointer_global_signature decl with
     | Some (ret_level, params, variadic, name) ->
         let ret_ty = Ast.build_ptr ret_level base_ty in
         A.Param (Ast.FunPtr (ret_ty, params, variadic), name)
     | None ->
         let level, name = aux_decl_name decl in
         A.Param (Ast.build_ptr level base_ty, name))
  | `Param_decl (decl_spec, Some (`Abst_decl decl), _) ->
    (* Abstract declarator: parameter with a complex type but no name, as in
       function-pointer typedefs ("typedef void (*sighandler_t)(int);" — the
       inner [int] is named, but more elaborate forms like
       "typedef int (*cmp_t)(const void *, const void *);" leave the inner
       params unnamed). We synthesise a name and reuse [aux_abstract_declarator]
       to build the parameter ctype. *)
    let base_ty = aux_decl_spec decl_spec in
    let ty = aux_abstract_declarator base_ty (Some decl) in
    A.Param (ty, "anon_param_" ^ string_of_int (gen_id ()))
  | `Param_decl (decl_spec, None, _) ->
    let ty = aux_decl_spec decl_spec in
    A.Param (ty, "anon_param_" ^ string_of_int (gen_id ()))
  | `Vari_param _ -> A.Vararg

(** Drop bare [void] parameters (the C [f(void)] = "no parameters" form).
    [Vararg] is preserved. *)
and drop_void_params lst =
  List.filter (function
    | A.Param (Ast.Void, _) -> false
    | _ -> true) lst

(** Extract parameter list from a [parameter_list] CST node. *)
and aux_extract_params ((_loc1, content, _loc2) : parameter_list) : A.param list =
  match content with
  | `Opt_choice_param_decl_rep_COMMA_choice_param_decl (Some (p1, params)) ->
      let all = (aux_param p1) :: (List.map (fun (_, p) -> aux_param p) params) in
      drop_void_params all
  | _ -> []

(* Function-pointer signature extraction.
   For a declarator that names a function pointer, return
   [(ret_pointer_level, param_ctypes, variadic, name)]:
   - [ret_pointer_level] is the number of outer [Poin_decl] wrappers, which
     applies to the return type (e.g. "r_obj* fp" with parenthesized-pointer
     body gives ret_pointer_level=1).
   - [param_ctypes] is the param-list ctypes (Vararg filtered out).
   - [variadic] is true iff the param list contained a [Vararg].
   - [name] is the identifier under the inner parenthesized-pointer
     indirection.
   Recognised shapes are nested [Poin_decl] wrappers around a [Func_decl]
   whose inner declarator is a parenthesized pointer-to-name declarator,
   optionally with [Attr_decl] wrappers. *)
and funptr_params_of_list param_list =
  let params = aux_extract_params param_list in
  let variadic = List.exists (function A.Vararg -> true | _ -> false) params in
  let param_tys =
    List.filter_map (function A.Param (ty, _) -> Some ty | A.Vararg -> None) params
  in
  (param_tys, variadic)

and function_pointer_global_signature (decl : declarator) =
  match decl with
  | `Poin_decl (_, _, _, _, inner) ->
      Option.map
        (fun (lvl, params, variadic, name) -> (lvl + 1, params, variadic, name))
        (function_pointer_global_signature inner)
  | `Attr_decl (inner, _) -> function_pointer_global_signature inner
  | `Func_decl (inner, param_list, _, _) when is_parenthesized_pointer_declarator inner ->
      (try
        let name = snd (aux_decl_name inner) in
        let param_tys, variadic = funptr_params_of_list param_list in
        Some (0, param_tys, variadic, name)
      with _ -> None)
  | _ -> None

let function_pointer_declaration_declarator_signature
    (decl : declaration_declarator) =
  match decl with
  | `Func_decl_decl (name_decl, param_list, _, _)
    when is_parenthesized_pointer_declarator name_decl ->
      (try
        let name = snd (aux_decl_name name_decl) in
        let param_tys, variadic = funptr_params_of_list param_list in
        Some (0, param_tys, variadic, name)
      with _ -> None)
  | `Poin_decl (_, _, _, _, inner) ->
      Option.map
        (fun (lvl, params, variadic, name) -> (lvl + 1, params, variadic, name))
        (function_pointer_global_signature inner)
  | _ -> None

let function_pointer_global_name decl =
  Option.map (fun (_, _, _, n) -> n) (function_pointer_global_signature decl)

let function_pointer_declaration_declarator_name decl =
  Option.map (fun (_, _, _, n) -> n) (function_pointer_declaration_declarator_signature decl)

(** Check whether a [declarator] contains a function declarator. *)
let rec has_func_decl (decl: declarator) =
  match decl with
  | `Func_decl _ -> true
  | `Poin_decl (_, _, _, _, inner) -> has_func_decl inner
  | _ -> false

let rec aux_params (decl: declarator) : int * A.param list =
  match decl with
  | `Func_decl (_, (_loc1, `Opt_choice_param_decl_rep_COMMA_choice_param_decl (Some (p1, params)), _loc2), _,_) ->
     let all = (aux_param p1) :: (List.map (fun (_, p) -> aux_param p) params) in
     0, drop_void_params all
  | `Poin_decl (_,_,_,_,decl) -> let level, params = aux_params decl in (level + 1, params)
  | _ -> (0, [])


let rec aux_expression (expr: expression) : A.e =
  match expr with 
  | `Choice_cond_exp e -> aux_not_bin_expression e
  | `Bin_exp e -> aux_bin_expression e
and aux_bin_expression (e: binary_expression) =
  let build_expr op e1 e2 = 
    let e1 = aux_expression e1 in
    let e2 = aux_expression e2 in
    (exprs_to_pos e1 e2, A.Binop (op, (e1, e2)))
  in
  match e with 
  | `Exp_PLUS_exp (e1, _, e2) -> build_expr "+" e1 e2
  | `Exp_DASH_exp (e1, _, e2) -> build_expr "-" e1 e2
  | `Exp_STAR_exp (e1, _, e2) -> build_expr "*" e1 e2
  | `Exp_SLASH_exp (e1, _, e2) -> build_expr "/" e1 e2
  | `Exp_PERC_exp (e1, _, e2) -> build_expr "%" e1 e2
  | `Exp_BARBAR_exp (e1, _, e2) -> build_expr "||" e1 e2
  | `Exp_AMPAMP_exp (e1, _, e2) -> build_expr "&&" e1 e2
  | `Exp_BAR_exp (e1, _, e2) -> build_expr "|" e1 e2
  | `Exp_HAT_exp (e1, _, e2) -> build_expr "^" e1 e2
  | `Exp_AMP_exp (e1, _, e2) -> build_expr "&" e1 e2
  | `Exp_EQEQ_exp (e1, _, e2) -> build_expr "==" e1 e2
  | `Exp_BANGEQ_exp (e1, _, e2) -> build_expr "!=" e1 e2
  | `Exp_GT_exp (e1, _, e2) -> build_expr ">" e1 e2
  | `Exp_GTEQ_exp (e1, _, e2) -> build_expr ">=" e1 e2
  | `Exp_LTEQ_exp (e1, _, e2) -> build_expr "<=" e1 e2
  | `Exp_LT_exp (e1, _, e2) -> build_expr "<" e1 e2
  | `Exp_LTLT_exp (e1, _, e2) -> build_expr "<<" e1 e2
  | `Exp_GTGT_exp (e1, _, e2) -> build_expr ">>" e1 e2

and aux_string (s: string_) = 
  let unescape  = function
    | `Imm_tok_prec_p1_pat_c7f65b4 (loc, s) -> (loc,s)
    | `Esc_seq (loc, s) -> (loc,s)
  in
  let string_literal_parts ((prefix, escaped, (end_loc, _)) : string_literal) =
    let pos_str = List.map unescape escaped in
    let str = String.concat "" (List.map snd pos_str) in
    let start_loc =
      match prefix with
      | `LDQUOT (loc, _) -> loc
      | `UDQUOT_c163aae (loc, _) -> loc
      | `UDQUOT_df3447d (loc, _) -> loc
      | `U8DQUOT (loc, _) -> loc
      | `DQUOT (loc, _) -> loc
    in
    (start_loc, end_loc, str)
  in
  let identifier_parts ((loc, name) as identifier) =
    match Preprocessor.resolve_defined_string name with
    | Some str -> (loc, loc, str)
    | None ->
        failwith
          ("Not supported yet: concatenated strings containing unresolved identifiers: "
          ^ token_to_string identifier)
  in
  match s with 
  | `Str_lit str_lit ->
    let start_loc, end_loc, str = string_literal_parts str_lit in
    (locs_to_pos start_loc end_loc, A.Const (A.CStr str))
  | `Conc_str (head, tail) ->
    let start_loc, end_loc, str =
      match head with
      | `Id_str_lit (identifier, str_lit) ->
        let start_loc, _, first = identifier_parts identifier in
        let _, end_loc, second = string_literal_parts str_lit in
        (start_loc, end_loc, first ^ second)
      | `Str_lit_str_lit (str1, str2) ->
        let start_loc, _, first = string_literal_parts str1 in
        let _, end_loc, second = string_literal_parts str2 in
        (start_loc, end_loc, first ^ second)
      | `Str_lit_id (str_lit, identifier) ->
        let start_loc, _, first = string_literal_parts str_lit in
        let _, end_loc, second = identifier_parts identifier in
        (start_loc, end_loc, first ^ second)
    in
    let end_loc, str =
      List.fold_left
        (fun (_current_end, acc) fragment ->
          match fragment with
          | `Str_lit str_lit ->
            let _, end_loc, fragment = string_literal_parts str_lit in
            (end_loc, acc ^ fragment)
          | `Id identifier ->
            let _, end_loc, fragment = identifier_parts identifier in
            (end_loc, acc ^ fragment))
        (end_loc, str)
        tail
    in
    (locs_to_pos start_loc end_loc, A.Const (A.CStr str))
and aux_num_lit  s = 
  let has_float_syntax s =
    let rec loop i =
      if i >= String.length s then false
      else
        match s.[i] with
        | '.' | 'e' | 'E' | 'p' | 'P' -> true
        | _ -> loop (i + 1)
    in
    loop 0
  in
  let strip_float_suffix s =
    if String.length s = 0 then s
    else
      match s.[String.length s - 1] with
      | 'f' | 'F' | 'l' | 'L' -> String.sub s 0 (String.length s - 1)
      | _ -> s
  in
  let parse_int s =
    parse_c_int_literal s
  in
  let parse_float s =
    let core = strip_float_suffix s in
    Float.of_string core
  in
  if has_float_syntax s then
    A.Const (A.CFloat (parse_float s))
  else
    A.Const
      (try A.CInt (parse_int s)
       with Failure _ -> A.CFloat (parse_float s))

and aux_char_lit (c: char_literal) =
  let (start, chars, (loc2, _)) = c in
  let loc1 =
    match start with
    | `LSQUOT (loc, _)
    | `USQUOT_d861d39 (loc, _)
    | `USQUOT_2701bdc (loc, _)
    | `U8SQUOT (loc, _)
    | `SQUOT (loc, _) -> loc
  in
  let values = U.decode_c_char_literal_values chars in
  let is_plain_char =
    match (start, values) with
    | (`SQUOT _, [v]) when v >= 0 && v <= 255 -> true
    | _ -> false
  in
  let const =
    match values with
    | [v] when is_plain_char -> A.CChar (Char.chr v)
    | _ ->
        (* C multi-char constants are implementation-defined; use a stable byte packing. *)
        let packed = List.fold_left (fun acc v -> (acc lsl 8) lor (v land 0xFF)) 0 values in
        A.CInt packed
  in
  (locs_to_pos loc1 loc2, A.Const const)
    
and aux_not_bin_expression (e : expression_not_binary) = 
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Num_lit (loc, s) -> (loc_to_pos loc, aux_num_lit s)
  | `Char_lit c -> aux_char_lit c
  | `Null _ -> (Position.dummy, A.Const A.CNull)
  | `Call_exp  call -> aux_call_expression call
  | `Str s -> aux_string s
  | `Assign_exp (e1, `EQ _, e2) -> 
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      (exprs_to_pos lhs rhs, A.VarAssign (lhs, rhs))
  | `Assign_exp (e1, op, e2) -> 
      (* Expand a op= e into a = a op e *)
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      let pos = exprs_to_pos lhs rhs in
      let binop = 
        match op with 
        | `PLUSEQ _ -> "+"
        | `DASHEQ _ -> "-"
        | `STAREQ _ -> "*"
        | `SLASHEQ _ -> "/"
        | `PERCEQ _ -> "%"
        | `AMPEQ _ -> "&"
        | `BAREQ _ -> "|"
        | `HATEQ _ -> "^"
        | `LTLTEQ _ -> "<<"
        | `GTGTEQ _ -> ">>"
        | `EQ _ -> failwith "Unreachable: `EQ handled in previous case"
      in
      let rhs = (exprs_to_pos lhs rhs, A.Binop (binop, (lhs, rhs))) in
      (pos, A.VarAssign (lhs, rhs))
  | `Un_exp (op, e1) -> 
      let loc1,op = match op with 
        | `DASH (loc, _) -> (loc, "-")
        | `BANG (loc, _) -> (loc, "!")
        | `TILDE (loc, _) -> (loc, "~")
        | `PLUS (loc, _) -> (loc, "+")
      in
      let expr = aux_expression e1 in 
      (Position.join (loc_to_pos loc1) (fst expr),
      A.Unop (op, expr))
  | `Subs_exp expr -> 
      aux_subscription_expression expr
  | `Cond_exp (cond, _, Some (`Exp then_), _, else_) -> 
      let cond = aux_expression cond in
      let then_ = aux_expression then_ in
      let else_ = aux_expression else_ in
      let pos = exprs_to_pos cond then_ in
      let pos = Position.join pos (fst else_) in
      (pos, A.Ite (cond, then_, else_))
  | `Update_exp expr -> aux_update_expression expr
  | `Paren_exp expr -> aux_paren_expr expr
  | `True (loc,_) -> (loc_to_pos loc, A.Const (A.CBool true))
  | `False (loc,_) -> (loc_to_pos loc, A.Const (A.CBool false))
  | `Cast_exp ((loc1,_), typ, _, expr) ->
      let e = aux_expression expr in
      let cast_type = aux_type_descriptor typ in
      let pos = Position.join (loc_to_pos loc1) (fst e) in
      (pos, A.Cast (cast_type, e))
  | `Poin_exp expr -> aux_pointer_expression expr
  | `Field_exp expr -> aux_field_expression expr
  | `Sizeof_exp ((loc1,_), arg) ->
      let sizeof_id = (loc_to_pos loc1, A.Id "sizeof") in
      begin
        match arg with
        | `Exp e ->
            let e = aux_expression e in
            let pos = Position.join (loc_to_pos loc1) (fst e) in
            (pos, A.Call (sizeof_id, [e]))
        | `LPAR_type_desc_RPAR ((_loc_lp, _), ty, (loc_rp, _)) ->
            (* Lower sizeof(type) to sizeof((type)0) to stay in expression space. *)
            let ty = aux_type_descriptor ty in
            let zero = (loc_to_pos loc1, A.Const (A.CInt 0)) in
            let cast_zero = (locs_to_pos loc1 loc_rp, A.Cast (ty, zero)) in
            (locs_to_pos loc1 loc_rp, A.Call (sizeof_id, [cast_zero]))
      end
  | `Comp_lit_exp ((loc_lp, _), ty, (_loc_rp, _), init_list) ->
      (* C99 compound literal: `(type){ init... }`. Lower to `(type)init` so
         that the type is preserved for the enclosing expression. For
         constant initializers we keep the values; for dynamic ones we lose
         them but preserve the type (consistent with sizeof lowering). *)
      let ct = aux_type_descriptor ty in
      let ((l1, _), _, _, (l2, _)) = init_list in
      let pos_init = locs_to_pos l1 l2 in
      let init_expr =
        match aux_initializer_list_plan init_list with
        | InitConst c -> (pos_init, A.Const c)
        | InitDynamic _ ->
            (* Dynamic initializer (e.g., designated fields referencing
               variables, like `(struct r_lazy){ .x = some_var, ... }`).
               We drop the initializer values and substitute CInt 0 as a
               placeholder; the surrounding Cast still carries the type, so
               consumers see the compound literal's declared type. *)
            if !warn_unsupported then
              Printf.eprintf
                "Not supported yet: compound literal with non-constant initializers; dropping values but preserving type\n";
            (pos_init, A.Const (A.CInt 0))
      in
      let pos = Position.join (loc_to_pos loc_lp) (fst init_expr) in
      (pos, A.Cast (ct, init_expr))
  | _ ->
    Boilerplate.map_expression_not_binary () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: expresion_not_binary"

and aux_field_expression (field_expr: field_expression) =
  (* Desugar [p->x] as a deref followed by field projection: the typer's
     FieldRead projects from a record value, not from a pointer. Without
     this rewrite, [p->x] reaches typing as FieldRead(p, x) with p typed
     as a pointer-to-record intersected with a fresh inference variable,
     which cannot be projected and yields "untypeable projection". The
     synthesized dereference makes pointer-to-record field access work. *)
  let expr, op, (loc2, field_name) = field_expr in
  let e = aux_expression expr in
  let pos = Position.join (fst e) (loc_to_pos loc2) in
  let base = match op with
    | `DOT _ -> e
    | `DASHGT (loc_arrow, _) ->
        let deref_pos = Position.join (fst e) (loc_to_pos loc_arrow) in
        (deref_pos, A.Unop ("*", e))
  in
  (pos, A.FieldAccess (base, field_name))
and aux_update_expression (e: update_expression) =
  (* [pre++] / [post++]: PAst lowers them to assignments, whose value is the
     new or the old one. *)
  let (fix, op, e) = match e with
  | `Choice_DASHDASH_exp (op, e) -> ("pre", op, aux_expression e)
  | `Exp_choice_DASHDASH (e,op) -> ("post", op, aux_expression e)
  in
  let op = match op with
    | `DASHDASH (loc, _) -> (loc, fix ^ "--")
    | `PLUSPLUS (loc, _) -> (loc, fix ^ "++")
  in
  let pos = Position.join (loc_to_pos (fst op)) (fst e) in
  (pos, A.Unop (snd op, e))
and aux_subscription_expression (subs: subscript_expression) =
  let expr, _, index, _ = subs in
  let arr = aux_expression expr in
  let idx = aux_expression index in
  let pos = Position.join (fst arr) (fst idx) in
  (pos, A.Call ((pos, A.Id "[]"), [arr; idx])) (* A usual function? Or a specific operator*)
and aux_pointer_expression (poin: pointer_expression) =
  let op, expr = poin in
  let loc,op = match op with 
    | `AMP (loc, _) -> (loc, "&")
    | `STAR (loc, _) -> (loc, "*")
  in

  let e = aux_expression expr in
  let pos = Position.join (loc_to_pos loc) (fst e) in
  (pos, A.Unop (op, e))
and aux_assign_left_expression (e: assignment_left_expression) : A.e =
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Subs_exp expr -> aux_subscription_expression expr (*Like that so far, but may require special treatment *)
  | `Poin_exp expr -> aux_pointer_expression expr
  | `Field_exp expr -> aux_field_expression expr
  | _ -> (
    Boilerplate.map_assignment_left_expression () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: left side of assignment must be an identifier"
  )
and aux_call_expression (call: call_expression) =
  let expr, args = call in
  let func_ast = aux_expression expr in
  let pos1,_ = func_ast in 
  let pos2, args_ast = aux_argument_list args in
  let pos = Position.join pos1 pos2 in
  match expr with
  | `Choice_cond_exp (`Id (_, name)) when Preprocessor.is_function_macro name ->
      (* [NAME(...)] with [NAME] a function-like macro: an expansion. A
         parenthesized callee [(NAME)(...)] is a [Paren_exp], hence a call of
         the variable or function [NAME], as in C. *)
      (match Preprocessor.expand_macro name args_ast with
       | Some expansion -> expansion
       | None -> (pos, A.MacroCall (name, args_ast)))
  | _ -> (pos, A.Call (func_ast, args_ast))
and aux_argument (arg: anon_choice_exp_f079e30) =
  match arg with 
  | `Exp e -> aux_expression e
  | `Comp_stmt comp -> aux_body comp
and aux_argument_list (arg_list: argument_list) =
  let (l1, _),args,(l2,_) = arg_list in
  let pos = locs_to_pos l1 l2 in
  match args with 
  | None -> (pos, [])
  | Some (arg1, others) ->
    (pos, aux_argument arg1 ::  List.map (fun (_, arg) -> aux_argument arg) others)

and  aux_comma_expression (expr: anon_choice_exp_55b4dba) =
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let _ = aux_expression e1 in
      aux_comma_expression e2 (* ? *)

and aux_return_statement (ret: return_statement)  =
  let (l1, _), expr_opt, (l2,_) = ret in
  let pos = locs_to_pos l1 l2 in
  match expr_opt with
  | None -> (pos, A.Return None)
  | Some expr -> (pos, Return (Some (aux_comma_expression expr)))

and aux_expression_statement (expr_stmt: expression_statement) =
  match expr_stmt with 
  | (Some comma_expr, _) -> aux_comma_expression comma_expr
  | (None, (loc,_)) -> (loc_to_pos loc, A.Return None)

and aux_for_cond (stmts: for_statement_body) =
  let decl,cond,_,incr = stmts in
  let init = match decl with 
  | `Decl decl -> aux_declarations decl
  | `Opt_choice_exp_SEMI expr -> aux_expression_statement expr
  in
  let cond = Option.map aux_comma_expression cond in 
  let incr = Option.map aux_comma_expression incr in
  init, cond, incr

and aux_for_statement (for_stmt: for_statement) =
  let (loc1,_),_,for_exprs,_,stmt = for_stmt in
  let init, cond, incr = aux_for_cond for_exprs in
  let body = aux_statement stmt in 
  (Position.join (loc_to_pos loc1) (fst body), A.For (init, cond, incr, body))

and aux_while_statement (while_stmt: while_statement) =
  let (loc1, _),cond,body = while_stmt in
  let b = aux_statement body in
  let pos = Position.join (loc_to_pos loc1) (fst b) in
  pos,A.While (aux_paren_expr cond, b)

and aux_do_statement (do_stmt: do_statement) =
  (* Transform do { body } while (cond) into while(true) { body; if(!cond) break; }
     The previous approach Seq([body, While(cond, body)]) duplicated the body, which
     placed break statements from the first copy outside any loop, causing orphan
     Ret(BLoop) nodes during the MLsem control-flow transformation. *)
  let (loc_do, _), body, _, cond, (loc_semi, _) = do_stmt in
  let pos = locs_to_pos loc_do loc_semi in
  let body_ast = aux_statement body in
  let cond_ast = aux_paren_expr cond in
  let true_cond = (Mlsem.Common.Position.dummy, A.Const (A.CBool true)) in
  let break_stmt = (Mlsem.Common.Position.dummy, A.Break) in
  let cond_check = (Mlsem.Common.Position.dummy, A.If (cond_ast, (Mlsem.Common.Position.dummy, A.Const A.CNull), Some break_stmt)) in
  (* [do { B } while (0)], the idiom of statement macros, runs [B] once: it is
     [B] when no [break] or [continue] of [B] targets it. As a loop, the
     paths where it does not run at all would be followed too. *)
  let rec targets_loop (_, e as node) =
    match e with
    | A.Break | A.Next -> true
    | A.While _ | A.For _ | A.Switch _ -> false
    | _ -> List.exists targets_loop (A.children node)
  in
  match cond_ast with
  | _, A.Const (A.CInt 0 | A.CBool false) when not (targets_loop body_ast) -> body_ast
  | _ ->
  let loop_body = (pos, A.Seq [body_ast; cond_check]) in
  (pos, A.While (true_cond, loop_body))

and aux_compound_stmt (stmt: compound_statement) =
  let (l1,_),block_items,(l2,_) = stmt in
  let block_items = Preprocessor.expand_block_items block_items in
  let stmts = List.map aux_block_item block_items in
  (locs_to_pos l1 l2, A.Block stmts)

and aux_switch_statement (switch_stmt: switch_statement) =
  let (loc1,_),expr,cases = switch_stmt in
  let expr  = aux_paren_expr expr in
  let body = aux_compound_stmt cases in

  match body with
  | pos, A.Block lst ->
      (* Filter out non-case/non-default items that can appear in the switch
         compound statement (e.g. preprocessor directives turned into Const CNull,
         or declarations). Only Case and Default are meaningful to the switch.
         TODO: actually support the preprocessor inside here*)
      let lst = List.filter (fun (_, item) ->
        match item with
        | A.Case _ | A.Default _ -> true
        | _ -> false
      ) lst in
      (Position.join (loc_to_pos loc1) pos, A.Switch (expr, lst))
  | _ -> failwith "Unexpected body in switch statement"

and aux_labeled_statement (((loc_label, label), (loc_colon, _), body) as labeled_stmt : labeled_statement) =
  (* [label: stmt]: [stmt], the target of the [goto label]s
     ([PAst.label_blocks]). *)
  match body with
  | `Stmt stmt -> (locs_to_pos loc_label loc_colon, A.Labeled (label, aux_statement stmt))
  | `Decl _ ->
      if !warn_unsupported then begin
        Printf.eprintf
          "Not supported yet: declaration after label `%s`; emitting Noop\n" label;
        let tree = Boilerplate.map_labeled_statement () labeled_stmt in
        Raw_tree.to_channel stderr tree
      end;
      (locs_to_pos loc_label loc_colon, A.Const A.CNull)

and aux_non_case_statement (stmt: non_case_statement) =
  match stmt with
  | `Ret_stmt ret -> aux_return_statement ret
  | `Exp_stmt exp -> aux_expression_statement exp
  | `If_stmt if_stmt -> aux_if_statement if_stmt
  | `Comp_stmt comp -> aux_body comp
  | `For_stmt for_stmt -> aux_for_statement for_stmt
  | `While_stmt while_stmt -> aux_while_statement while_stmt
  | `Do_stmt do_stmt -> aux_do_statement do_stmt
  | `Brk_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, A.Break)
  | `Cont_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, A.Next)
  | `Goto_stmt ((l1, _), label, (l2, _)) -> (locs_to_pos l1 l2, A.Goto (token_to_string label))
  | `Labe_stmt labeled_stmt -> aux_labeled_statement labeled_stmt
  | `Switch_stmt st -> aux_switch_statement st
  | `Attr_stmt _ -> failwith "Not supported yet: attribute statements"
  | _ -> failwith "Not supported yet: Seh_try and Seh_leave statements"

and aux_top_level_statement (stmt: top_level_statement) =
  match stmt with 
  | `Top_level_exp_stmt (Some exp, (loc2,_)) -> 
      let e = aux_not_bin_expression exp in
      let pos = Position.join (fst e) (loc_to_pos loc2) in
      (pos, A.Return (Some e))
  | _ -> failwith "Not supported yet: top level statements without expression"

and aux_case_statement (case_stmt: case_statement) =
  let (case, _, body) = case_stmt in 
  let case_body = List.map (function 
   | `Choice_attr_stmt st -> aux_non_case_statement st
   | `Decl decls -> aux_declarations decls
   | `Type_defi _typs -> failwith "Type definitions in case statements are not supported yet"
  ) body
  in
  let first_loc = 
    match case_body with
    | [] -> Position.dummy
    | (pos, _) :: _ -> pos
  in
  let last_loc = 
    match List.rev case_body with
    | [] -> Position.dummy
    | (pos, _) :: _ -> pos
  in
  let case_body = (Position.join first_loc last_loc, A.Seq case_body) in
  match case with 
  | `Case_exp ((loc1,_), e) ->
    let e = aux_expression e in
    let pos = Position.join (loc_to_pos loc1) last_loc in
    (pos, A.Case (e, case_body))
  | `Defa ((loc1,_)) ->
    let pos = Position.join (loc_to_pos loc1) last_loc in
    (pos, A.Default (case_body))


and aux_statement (stmt: statement) =
 match stmt with 
 | `Case_stmt st -> aux_case_statement st
 | `Choice_attr_stmt st -> aux_non_case_statement st

and aux_initializer_list_plan (init_list: initializer_list) =
  let rec const_of_expr e =
    match snd (aux_expression e) with
    | A.Const c -> Some c
    | _ -> None
  and const_of_pair (pair : initializer_pair) =
    match pair with
    | `Rep1_choice_subs_desi_EQ_choice_exp (_, _, _)
    | `Id_COLON_choice_exp (_, _, _) -> None
  and const_of_initializer (init : anon_choice_init_pair_1a6981e) =
    match init with
    | `Exp e -> const_of_expr e
    | `Init_list nested -> const_of_initializer_list nested
    | `Init_pair pair -> const_of_pair pair
  and const_of_initializer_list ((_, initializers, _, _) : initializer_list) =
    match initializers with
    | None -> Some (A.CArray [])
    | Some (init1, inits) ->
        let rec gather acc = function
          | [] -> Some (A.CArray (List.rev acc))
          | init :: tl ->
              begin
                match const_of_initializer init with
                | Some c -> gather (c :: acc) tl
                | None -> None
              end
        in
        gather [] (init1 :: List.map snd inits)
  in
  match const_of_initializer_list init_list with
  | Some c -> InitConst c
  | None ->
      let rec rhs_to_dynamic base_path (init : anon_choice_exp_3078596) =
        match init with
        | `Exp e -> [ (base_path, aux_expression e) ]
        | `Init_list nested -> dynamic_of_initializer_list base_path nested
      and path_with_designator path d =
        match d with
        | `Subs_desi (_, idx, _) -> path @ [InitIndex (aux_expression idx)]
        | `Field_desi (_, (_, field_name)) -> path @ [InitField field_name]
        | `Subs_range_desi _ -> failwith "Not supported yet: subscript range designators"
      and item_to_dynamic base_path seq_index (init : anon_choice_init_pair_1a6981e) =
        let default_path = base_path @ [InitIndex (Position.dummy, A.Const (A.CInt seq_index))] in
        match init with
        | `Exp e -> [ (default_path, aux_expression e) ]
        | `Init_list nested -> dynamic_of_initializer_list default_path nested
        | `Init_pair pair ->
            begin
              match pair with
              | `Rep1_choice_subs_desi_EQ_choice_exp (designators, _, rhs) ->
                  let path = List.fold_left path_with_designator base_path designators in
                  rhs_to_dynamic path rhs
              | `Id_COLON_choice_exp ((_, field_name), _, rhs) ->
                  rhs_to_dynamic (base_path @ [InitField field_name]) rhs
            end
      and dynamic_of_initializer_list base_path ((_, initializers, _, _) : initializer_list) =
        match initializers with
        | None -> []
        | Some (init1, inits) ->
            let all_inits = init1 :: List.map snd inits in
            let rec aux idx acc = function
              | [] -> List.rev acc
              | init :: tl ->
                  let leaves = item_to_dynamic base_path idx init in
                  aux (idx + 1) (List.rev_append leaves acc) tl
            in
            aux 0 [] all_inits
      in
      InitDynamic (dynamic_of_initializer_list [] init_list)

and aux_initializer_list (init_list: initializer_list) =
  let plan = aux_initializer_list_plan init_list in
  let ((l1, _), _, _, (l2, _)) = init_list in
  let pos = locs_to_pos l1 l2 in
  match plan with
  | InitConst c -> (pos, A.Const c)
  | InitDynamic _ ->
      failwith "Internal error: dynamic initializer list cannot be converted to a single expression"

and aux_declaration typ (decl: anon_choice_opt_ms_call_modi_decl_decl_opt_gnu_asm_exp_2fa2f9e) =
  match decl with 
  |`Init_decl (declr, _, init) -> 
      let level,name = aux_decl_name declr in
      let name = A.Id name in
      let typ = Ast.build_ptr level typ in
      let var_decl = (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, name))) in
      let rec lhs_from_steps base (steps : init_step list) =
        match steps with
        | [] -> base
        | InitIndex idx :: tl ->
            let lhs = (Position.dummy, A.Call ((Position.dummy, A.Id "[]"), [base; idx])) in
            lhs_from_steps lhs tl
        | InitField field :: tl ->
            let lhs = (Position.dummy, A.FieldAccess (base, field)) in
            lhs_from_steps lhs tl
      in
      begin
        match init with
        | `Exp expr ->
            let e = aux_expression expr in
            (* Model C's implicit [void* -> T*] conversion at initialization.
               When the declared type is a concrete (non-[void]) pointer,
               coerce the initializer to it, narrowing e.g. [malloc]'s [c_ptr]
               to the declared [char*]. Skipped when the target is [void*]
               itself: that is the top pointer ("no pointee assertion"), and
               its [Ast] encoding is a pointer-to-[void] that would *clobber* a
               precise initializer rather than widen it — so [void *s = f()]
               keeps [f]'s precise return type. *)
            let e =
              match typ with
              | Ast.Ptr Ast.Void -> e
              | Ast.Ptr _ -> (Position.dummy, A.CoerceNarrow (typ, e))
              | _ -> e
            in
            let var_assign = (Mlsem.Common.Position.dummy, A.VarAssign ((Mlsem.Common.Position.dummy, name), e)) in
            (Mlsem.Common.Position.dummy, A.Seq [var_decl; var_assign])
        | `Init_list init_list ->
            begin
              match aux_initializer_list_plan init_list with
              | InitConst c ->
                  let e = (Position.dummy, A.Const c) in
                  let var_assign = (Mlsem.Common.Position.dummy, A.VarAssign ((Mlsem.Common.Position.dummy, name), e)) in
                  (Mlsem.Common.Position.dummy, A.Seq [var_decl; var_assign])
              | InitDynamic entries ->
                  let base = (Position.dummy, name) in
                  (* Decide whether the aggregate is a record (struct/union) or
                     an array from the leading designator of the entries: a
                     field designator ([.f = v]) means a record, an index
                     designator ([[i] = v]) means an array. A struct seeded as
                     a null-filled array would make [base] a [*c_null], so the
                     subsequent [base.f = v] writes — lowered to [RecUpd], whose
                     domain is an (open) record — have nothing to write into and
                     the whole declaration types as [untypeable application]. *)
                  let is_record_init =
                    List.exists
                      (fun (steps, _) ->
                        match steps with
                        | InitField _ :: _ -> true
                        | _ -> false)
                      entries
                  in
                  let init_empty =
                    if is_record_init then
                      (* Seed [base] with the empty closed record [{}]. It is a
                         subtype of [RecUpd]'s open-record domain, so each
                         [base.f = v] write extends it, accumulating the
                         designated fields. *)
                      let empty_record =
                        (Position.dummy,
                         A.Cast (Ast.Struct ("", []),
                                 (Position.dummy, A.Const A.CNull)))
                      in
                      (Position.dummy, A.VarAssign (base, empty_record))
                    else
                      let element_ty =
                        match typ with
                        | Ast.Ptr elem_ty -> elem_ty
                        | _ -> Ast.Any
                      in
                      let placeholder = placeholder_const_of_ctype element_ty in
                      let init_len = top_level_initializer_count init_list in
                      let bootstrap_vals =
                        List.init init_len (fun _ -> placeholder) in
                      (Position.dummy,
                       A.VarAssign
                         (base, (Position.dummy, A.Const (A.CArray bootstrap_vals))))
                  in
                  let assigns =
                    List.map
                      (fun (steps, rhs) ->
                        let lhs = lhs_from_steps base steps in
                        (Position.dummy, A.VarAssign (lhs, rhs)))
                      entries
                  in
                  (Mlsem.Common.Position.dummy, A.Seq (var_decl :: init_empty :: assigns))
            end
      end
  | `Opt_ms_call_modi_decl_decl_opt_gnu_asm_exp (_, declr, _) ->
      let (level, name, fp) = aux_decl2_name declr in
      let typ = match fp with
        | None -> Ast.build_ptr level typ
        | Some (params, variadic) ->
            let ret_ty = Ast.build_ptr level typ in
            Ast.FunPtr (ret_ty, params, variadic)
      in
      (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, A.Id name)))
and aux_declarations ((decl_type, decl1, decls, _loc2): declaration) =
  let typ = aux_decl_spec decl_type in
  (* A [static] local keeps its value from one call to the next: on entry, it
     holds any value of its type, not its initializer (which only sets the
     value of the first call). *)
  let static_value d =
    match d with
    | _, A.Seq ((p, A.VarDeclare (ty, ((_, A.Id _) as name))) :: _)
    | (p, A.VarDeclare (ty, ((_, A.Id _) as name))) ->
        let any_value = (p, A.Cast (ty, (p, A.Const A.CNull))) in
        (p, A.Seq [ (p, A.VarDeclare (ty, name)) ; (p, A.VarAssign (name, any_value)) ])
    | d -> d
  in
  let aux d =
    let d = aux_declaration typ d in
    if aux_global_linkage decl_type = A.Static then static_value d else d
  in
  (Mlsem.Common.Position.dummy, A.Seq (aux decl1 :: List.map (fun (_, d) -> aux d) decls))
(* Returns (return_ptr_level, name, fp_signature_opt). When the third
   component is [Some (params, variadic)] the declarator names a function
   pointer (or a local function prototype, which we model the same way);
   the [int] then counts only the outer pointer wrappers and applies to
   the return type. *)
and aux_decl2_name (decl: declaration_declarator) =
  match decl  with
  | `Id tok -> (0, token_to_string tok, None)
  | `Poin_decl (_,_,_,_,decl) -> let (level, name) = aux_decl_name decl in (level + 1, name, None)
  | `Array_decl (decl,_,_,_,_) -> let (level, name) = aux_decl_name decl in (level + 1, name, None)
  | `Paren_decl (_, _, decl, _) -> let (level, name) = aux_decl_name decl in (level, name, None)
  | `Attr_decl (decl, _) -> let (level, name) = aux_decl_name decl in (level, name, None)
  | `Func_decl_decl (inner_decl, param_list, _, _) ->
      let (_, name) = aux_decl_name inner_decl in
      let param_tys, variadic = funptr_params_of_list param_list in
      (0, name, Some (param_tys, variadic))
and aux_block_item (item : block_item) =
  match item with
  | `Stmt stmt -> aux_statement stmt
  | `Decl decls -> aux_declarations decls
  | `Attr_stmt _attr_stmt -> failwith "Not supported yet: attribute statements"
  | `Type_defi _ -> (Mlsem.Common.Position.dummy, Const CNull) (* Type definitions don't produce values *)
  | `Empty_decl _ -> (Mlsem.Common.Position.dummy, Const CNull)

  | `Prep_if _ | `Prep_ifdef _ | `Prep_incl _ | `Prep_def _
  | `Prep_func_def _ | `Prep_call _ ->
      (Mlsem.Common.Position.dummy, Const CNull) (*TODO: Handle preprocessor! *)
  | `Func_defi _ -> failwith "Function definitions inside a block not supported"
  | _ -> failwith "Not supported yet: old function defi or link spec"

and aux_body body =
  let (l1,_),block_items,(l2,_) = body in
  let block_items = Preprocessor.expand_block_items block_items in
  let stmts = List.map aux_block_item block_items in
  (* A lone declaration keeps its [Block]: the block is the scope it is
     declared in, and a bare [Seq] declares nothing, so the variable would
     fall through to an (immutable) unknown name. *)
  let rec declares (_, e) = match e with
    | A.VarDeclare _ -> true
    | A.Seq es -> List.exists declares es
    | _ -> false
  in
  if Utils.is_singleton stmts && not (declares (List.hd stmts)) then
    List.hd stmts
  else
    (locs_to_pos l1 l2, A.Block stmts)

and aux_paren_expr (p_expr: parenthesized_expression) =
  let (l1,_), expr, (l2,_) = p_expr in
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) ->
      let ast1 = aux_expression e1 in
      let ast2 = aux_comma_expression e2 in
      (locs_to_pos l1 l2, A.Comma (ast1,ast2))
  | `Comp_stmt comp -> aux_body comp

and aux_if_statement (if_stmt: if_statement) =
  let (l1,_), cond, then_stmt, else_opt = if_stmt in
  let ast_cond = aux_paren_expr cond in
  let ast_then = aux_statement then_stmt in
  let ast_else = Option.map (fun (_,st) -> aux_statement st) else_opt in
  let l2 = Option.map
    (fun (pos,_) -> Position.end_of_position pos)
    ast_else
    |> Option.value ~default:(Position.end_of_position @@ fst ast_then)
  in

  (Position.lex_join (conv_pos l1.start) l2,A.If (ast_cond, ast_then, ast_else))

let rec body_has_trailing_return ((_, expr) : A.e) =
  match expr with
  | A.Return _ -> true
  | A.Seq [] | A.Block [] -> false
  | A.Seq exprs | A.Block exprs -> body_has_trailing_return (List.hd (List.rev exprs))
  | _ -> false

exception VariadicParameter of string

let ensure_void_return body =
  if body_has_trailing_return body then body
  else
    let pos, expr = body in
    let trailing_return = (pos, A.Return None) in
    match expr with
    | A.Block exprs -> (pos, A.Block (exprs @ [trailing_return]))
    | A.Seq exprs -> (pos, A.Seq (exprs @ [trailing_return]))
    | _ -> (pos, A.Block [body; trailing_return])

let rec preprocessor_callbacks () =
  {
    Preprocessor.parse_string = parse_string;
    top_level_statement = aux_top_level_statement;
    parse_file;
    parse_translation_unit = aux_translation_unit;
  }

and aux_top_level_items defines items =
  Preprocessor.top_level_items
    ~parser_callbacks:(preprocessor_callbacks ())
    ~top_level_item:aux_top_level_item_non_preproc
    ~top_level_block_item:aux_top_level_block_item_non_preproc
    defines items

and aux_top_level_block_items defines items =
  Preprocessor.top_level_block_items
    ~parser_callbacks:(preprocessor_callbacks ())
    ~top_level_item:aux_top_level_item_non_preproc
    ~top_level_block_item:aux_top_level_block_item_non_preproc
    defines items

and aux_top_level_item_non_preproc defines (item : top_level_item) =
  match item with
  | `Func_defi (_, decl_spec, _, decl, body) ->
      let _,name = aux_decl_name decl in
      (* the part with the parameters can also carry the information about pointers for the return type! *)
      let level, params = aux_params decl in
      let return_type = Ast.build_ptr level (aux_decl_spec decl_spec) in
      let body = aux_body body in
      let body =
        match return_type with
        | Ast.Void -> ensure_void_return body
        | _ -> body
      in
      (defines, [Mlsem.Common.Position.dummy, A.Fundef (return_type, name, params, body)])
  | `Empty_decl (type_spec, _tok) ->
      let s = aux_type_spec type_spec in
      (match s with
       | Ast.Struct (name, _) | Ast.Union (name, _) | Ast.Enum (name, _) ->
           (defines, [Mlsem.Common.Position.dummy, A.TypeDecl (name, s)])
       | Ast.Typeref _ ->
           (* Bare identifier as an empty declaration. In system headers this
              is overwhelmingly glibc linkage/attribute macros that expand to
              nothing under C (__BEGIN_DECLS, __END_DECLS, __THROW, …). Since
              we don't run a macro expander, tree-sitter sees them as
              identifiers and classifies them as empty declarations. They
              carry no binding, so drop silently. *)
           (defines, [])
       | _ ->
           if !warn_unsupported then
             Printf.printf "Empty declaration is not a struct/union/enum; ignoring (ctype=%s)\n"
               (Ast.show_ctype s);
           (defines, [Mlsem.Common.Position.dummy, A.TypeDecl ("", s)]))
  | `Type_defi type_def ->
      (defines, aux_type_definition type_def)
  | `Decl (decl_spec, first_decl, more_decls, _semi) ->
      let base_ty = aux_decl_spec decl_spec in
      let linkage = aux_global_linkage decl_spec in
      let try_func_of_decl (decl: anon_choice_opt_ms_call_modi_decl_decl_opt_gnu_asm_exp_2fa2f9e) =
        match decl with
        | `Opt_ms_call_modi_decl_decl_opt_gnu_asm_exp (_, declr, _) ->
            begin match declr with
            | `Func_decl_decl (name_decl, param_list, _, attrs) ->
                if is_parenthesized_pointer_declarator name_decl then
                  None
                else begin match (try Some (aux_decl_name name_decl) with _ -> None) with
                | None -> None
                | Some (level, name) ->
                    (try
                      let params = aux_extract_params param_list in
                      (* [void exit(int) __attribute__((noreturn));] (glibc):
                         the attribute follows the declarator. *)
                      let base_ty =
                        if level = 0 && base_ty = Ast.Void
                           && List.exists (fun a -> is_noreturn_modifier (`Attr_spec a)) attrs
                        then Ast.NoReturn else base_ty in
                      let ret_ty = Ast.build_ptr level base_ty in
                      Some (Mlsem.Common.Position.dummy,
                            A.Fundef (ret_ty, name, params,
                                      (Mlsem.Common.Position.dummy, A.Seq [])))
                    with _ -> None)
                end
            | `Poin_decl (_, _, _, _, inner_decl)
              when has_func_decl inner_decl
                && Option.is_none
                     (function_pointer_declaration_declarator_name declr) ->
                (* "int* foo(int)" — function returning a pointer. We
                   distinguish it from "r_obj* (*fp)(...)" which is a
                   function-pointer global with pointer return type:
                   the latter is dropped here so [try_global_of_decl] can
                   pick it up as a [GlobalVar]. *)
                begin match (try Some (aux_decl_name inner_decl) with _ -> None) with
                | None -> None
                | Some (_, name) ->
                    (try
                      let level, params = aux_params inner_decl in
                      let ret_ty = Ast.build_ptr (level + 1) base_ty in
                      Some (Mlsem.Common.Position.dummy,
                            A.Fundef (ret_ty, name, params,
                                      (Mlsem.Common.Position.dummy, A.Seq [])))
                    with _ -> None)
                end
            | _ -> None
            end
        | _ -> None
      in
      (* For non-function declarators, emit a [GlobalVar] carrying the
         declared type. Function prototypes and function pointers were
         already consumed by [try_func_of_decl] above, so we only see
         true variable shapes here. Initializers are intentionally
         dropped — only the declared type seeds the binding. *)
      let try_global_of_decl (decl: anon_choice_opt_ms_call_modi_decl_decl_opt_gnu_asm_exp_2fa2f9e) =
        let emit level name =
          let ty = Ast.build_ptr level base_ty in
          Preprocessor.remember_global name ;
          Some (Mlsem.Common.Position.dummy, A.GlobalVar (linkage, name, ty))
        in
        (* [ret_level] applies to the return type (number of outer
           [Poin_decl] wrappers around the parenthesized-pointer body). The
           function pointer itself is a callable, so we emit it as [FunPtr],
           which [Ast.typeof_ctype] translates to an attribute-wrapped arrow. *)
        let emit_function_pointer ret_level params variadic name =
          let ret_ty = Ast.build_ptr ret_level base_ty in
          let ty = Ast.FunPtr (ret_ty, params, variadic) in
          Preprocessor.remember_global name ;
          Some (Mlsem.Common.Position.dummy, A.GlobalVar (linkage, name, ty))
        in
        match decl with
        | `Init_decl (declr, _, _init) ->
            (* [Init_decl] takes a [declarator], which uses [`Func_decl] for
               function shapes. The parser for function definitions uses the
               other branch; here we reach this only for initialized globals.
               A function-pointer global with an initializer like
               [r_obj* (*fp)(r_obj*) = NULL;] parses as
               [Poin_decl(Func_decl(Paren_decl(Poin_decl(Id "fp")), ...))]
               (the outer Poin_decl comes from the [r_obj*] return type).
               [function_pointer_global_signature] handles all the wrapping
               shapes and recovers the full return / params / variadic info. *)
            (match function_pointer_global_signature declr with
             | Some (ret_level, params, variadic, name) ->
                 emit_function_pointer ret_level params variadic name
             | None ->
                 (match declr with
                  | `Func_decl _ -> None
                  | `Poin_decl (_, _, _, _, inner) when has_func_decl inner -> None
                  | `Id _ | `Poin_decl _ | `Array_decl _ | `Paren_decl _ | `Attr_decl _ ->
                      (try let (level, name) = aux_decl_name declr in emit level name
                       with _ -> None)))
        | `Opt_ms_call_modi_decl_decl_opt_gnu_asm_exp (_, declr, _) ->
            (* [declaration_declarator] uses [`Func_decl_decl] for functions
               and prototypes. Those were handled by [try_func_of_decl]; any
               that survive a failed match there (e.g. variadic) are dropped.
               Function-pointer globals with possibly pointer-typed return —
               either "(*fp)(...)" or "r_obj* (*fp)(...)" — are caught up
               front so the [Poin_decl ... has_func_decl] arm below does not
               drop them as if they were function declarations. *)
            (match function_pointer_declaration_declarator_signature declr with
             | Some (ret_level, params, variadic, name) ->
                 emit_function_pointer ret_level params variadic name
             | None ->
                 (match declr with
                  | `Func_decl_decl _ -> None
                  | `Poin_decl (_, _, _, _, inner) when has_func_decl inner -> None
                  | `Id _ | `Poin_decl _ | `Array_decl _ | `Paren_decl _ | `Attr_decl _ ->
                      (* By construction, none of the surviving arms hit the
                         [Func_decl_decl] branch in [aux_decl2_name], so its
                         function-pointer signature is always [None] here. *)
                      (try
                        let (level, name, _fp) = aux_decl2_name declr in
                        emit level name
                       with _ -> None)))
      in
      let all_decls = first_decl :: List.map snd more_decls in
      let items =
        List.filter_map
          (fun d ->
            match try_func_of_decl d with
            | Some _ as item -> item
            | None -> try_global_of_decl d)
          all_decls
      in
      (defines, items)
  | `Link_spec (_, _, body) ->
      (match body with
       | `Func_defi func_def -> aux_top_level_item_non_preproc defines (`Func_defi func_def)
       | `Decl decl -> aux_top_level_item_non_preproc defines (`Decl decl)
       | `Decl_list (_, items, _) -> aux_top_level_block_items defines items)
  | `Prep_ifdef _ | `Prep_if _ | `Prep_def _ | `Prep_func_def _ | `Prep_incl _ ->
      failwith "internal error: preprocessor item reached non-preprocessor top-level handler"
  | _ ->
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: top level item\n";
        print_top_level_item item
      end;
      (defines, [])

and aux_top_level_block_item_non_preproc defines (item : block_item) =
  match item with
  | `Func_defi func_def -> aux_top_level_item_non_preproc defines (`Func_defi func_def)
  | `Old_style_func_defi old_func_def -> aux_top_level_item_non_preproc defines (`Old_style_func_defi old_func_def)
  | `Link_spec link_spec -> aux_top_level_item_non_preproc defines (`Link_spec link_spec)
  | `Decl decl -> aux_top_level_item_non_preproc defines (`Decl decl)
  | `Attr_stmt attr_stmt -> aux_top_level_item_non_preproc defines (`Attr_stmt attr_stmt)
  | `Type_defi type_def -> aux_top_level_item_non_preproc defines (`Type_defi type_def)
  | `Empty_decl empty_decl -> aux_top_level_item_non_preproc defines (`Empty_decl empty_decl)
  | `Prep_if _ | `Prep_ifdef _ | `Prep_incl _ | `Prep_def _ | `Prep_func_def _
  | `Prep_call _ ->
      failwith "internal error: preprocessor block item reached non-preprocessor handler"
  | `Stmt _ ->
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: statement inside top level preprocessor branch\n";
        Boilerplate.map_block_item () item |> Tree_sitter_run.Raw_tree.to_channel stderr
      end;
      (defines, [])

and aux_translation_unit tree =
  (* Note: we intentionally do NOT reset [define_constants] here. A package's
     [.h] files are parsed before its [.c] files (see [Package.get_c_files]),
     and source files reference [#define]s established in the headers (e.g.
     [#define R_PRI_SSIZE "d"] in [rlang-types.h] is used by [arg.c] as
     [`"%" R_PRI_SSIZE "\n"`]). Resetting here would force each file to
     rediscover every macro in isolation, which defeats the local-include
     strategy of parsing the full source tree. The same logic is why
     [processed_headers] also accumulates across files. *)
  snd (aux_top_level_items (Preprocessor.current_predefined_defines ()) tree)

let to_ast (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) =
  match res.program with
  | None -> []
  | Some translation_unit ->
    aux_translation_unit translation_unit

let%test "aux_num_lit parses integer long suffix" =
  aux_num_lit "1L" = A.Const (A.CInt 1)

let%test "aux_num_lit parses hexadecimal integer" =
  aux_num_lit "0x2a" = A.Const (A.CInt 42)

let%test "aux_num_lit parses hexadecimal integer with suffixes" =
  aux_num_lit "0x2aUL" = A.Const (A.CInt 42)

let%test "aux_num_lit parses c-style octal" =
  aux_num_lit "077" = A.Const (A.CInt 63)

let%test "aux_num_lit parses float suffix" =
  match aux_num_lit "3.5f" with
  | A.Const (A.CFloat f) -> Float.abs (f -. 3.5) < 1e-12
  | _ -> false

let%test "aux_num_lit parses hexadecimal float" =
  match aux_num_lit "0x1.fp3" with
  | A.Const (A.CFloat f) -> Float.abs (f -. 15.5) < 1e-12
  | _ -> false

let%test "preproc define with literal creates PAst.Define" =
  let res = parse_string "#define ONE 1L\nint f() { return ONE; }" in
  match to_ast res with
  | (_, A.Define ("ONE", A.CInt 1)) :: _ -> true
  | _ -> false

let%test "concatenated string resolves identifier from literal define" =
  let res = parse_string "#define HELLO \"hello\"\nchar *f() { return HELLO \" world\"; }" in
  match to_ast res with
  | [ (_, A.Define ("HELLO", A.CStr "hello"));
      (_, A.Fundef (_, "f", _, (_, A.Return (Some (_, A.Const (A.CStr "hello world")))))) ] -> true
  | _ -> false

let%test "valueless define still enables subsequent ifdef" =
  let res =
    parse_string
      "#define FLAG\n#ifdef FLAG\nint f() { return 1; }\n#else\nint g() { return 2; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "ifndef keeps else branch skipped for undefined macro" =
  let res =
    parse_string
      "#ifndef FLAG\nint f() { return 1; }\n#else\nint g() { return 2; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "elifndef branch is selected when prior branches fail" =
  let res =
    parse_string
      "#ifdef FIRST\nint a() { return 1; }\n#elifndef SECOND\nint b() { return 2; }\n#else\nint c() { return 3; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "b", _, _))] -> true
  | _ -> false

let%test "linux predefined macros are enabled by default" =
  let res =
    parse_string
      "#ifdef __linux__\nint f() { return 1; }\n#else\nint g() { return 2; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "empty declaration prefix macro is ignored before parsing" =
  let res = parse_string "CURL_EXTERN CURL *curl_easy_init(void);\n" in
  res.stat.error_count = 0
  &&
  match to_ast res with
  | [(_, A.Fundef (Ast.Ptr (Ast.Typeref "CURL"), "curl_easy_init", [], (_, A.Seq [])))] -> true
  | _ -> false

let%test "empty declaration prefix macro only matches standalone token" =
  let res = parse_string "int CURL_EXTERN_value(void);\n" in
  res.stat.error_count = 0
  &&
  match to_ast res with
  | [(_, A.Fundef (Ast.Int, "CURL_EXTERN_value", [], (_, A.Seq [])))] -> true
  | _ -> false

let%test "direct C++ linkage specification exposes enclosed declarations" =
  let res = parse_string "extern \"C\" { int c_api(void); }\n" in
  res.stat.error_count = 0
  &&
  match to_ast res with
  | [(_, A.Fundef (Ast.Int, "c_api", [], (_, A.Seq [])))] -> true
  | _ -> false

let%test "skipped cplusplus guard recovers linkage declarations" =
  let res =
    parse_string
      "#ifdef __cplusplus\nextern \"C\" {\n#endif\nCURL_EXTERN CURL *curl_easy_init(void);\n#ifdef __cplusplus\n}\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (Ast.Ptr (Ast.Typeref "CURL"), "curl_easy_init", [], (_, A.Seq [])))] -> true
  | _ -> false

let%test "variadic declaration emits a Fundef ending in Vararg" =
  let prev = !warn_unsupported in
  Fun.protect
    ~finally:(fun () -> set_warn_unsupported prev)
    (fun () ->
      set_warn_unsupported false;
      let res =
        parse_string
          "int curl_easy_getinfo(int curl, int info, ...);\n"
      in
      res.stat.error_count = 0
      &&
      match to_ast res with
      | [(_, A.Fundef (Ast.Int, "curl_easy_getinfo", params, (_, A.Seq [])))] ->
          (match List.rev params with
           | A.Vararg :: _ -> true
           | _ -> false)
      | _ -> false)

let%test "variadic function-like macro emits a MacroDef ending in Vararg" =
  let prev = !warn_unsupported in
  Fun.protect
    ~finally:(fun () -> set_warn_unsupported prev)
    (fun () ->
      set_warn_unsupported false;
      let res =
        parse_string
          "int sink(const char* fmt, ...);\n\
           #define wrap(FMT, ...) sink(FMT, __VA_ARGS__)\n"
      in
      match to_ast res with
      | tops ->
          List.exists
            (function
              | _, A.MacroDef ("wrap", params, _) ->
                  (match List.rev params with
                   | A.Vararg :: _ -> true
                   | _ -> false)
              | _ -> false)
            tops)

(* The calls in the body of [fname] among [tops], as [`Macro]/[`Call] of the
   callee's name. *)
let calls_in fname tops =
  let rec calls (_, e) =
    match e with
    | A.MacroCall (n, args) -> (`Macro, n) :: List.concat_map calls args
    | A.Call ((_, A.Id n), args) -> (`Call, n) :: List.concat_map calls args
    | A.Call (f, args) -> calls f @ List.concat_map calls args
    | A.Return (Some e) | A.Unop (_, e) -> calls e
    | A.Binop (_, (e1, e2)) -> calls e1 @ calls e2
    | A.Block es | A.Seq es -> List.concat_map calls es
    | _ -> []
  in
  List.concat_map (function
      | _, (A.Fundef (_, n, _, body) | A.MacroDef (n, _, body)) when n = fname -> calls body
      | _ -> []) tops

let%test "a macro call is a MacroCall, a parenthesized one a Call" =
  let res =
    parse_string
      "int f(int x, int y);\n\
       #define f(x) (x)\n\
       int g(int x) { return f(x) + (f)(x, 1); }\n"
  in
  calls_in "g" (to_ast res) = [ (`Macro, "f") ; (`Call, "f") ]

let%test "#undef ends a function-like macro" =
  let prev = !warn_unsupported in
  Fun.protect
    ~finally:(fun () -> set_warn_unsupported prev)
    (fun () ->
      set_warn_unsupported false;
      let res =
        parse_string
          "#define m(x) (x)\n\
           int a(int x) { return m(x); }\n\
           #undef m\n\
           int b(int x) { return m(x); }\n"
      in
      let tops = to_ast res in
      calls_in "a" tops = [ (`Macro, "m") ] && calls_in "b" tops = [ (`Call, "m") ])

let%test "quoted include is ignored instead of crashing" =
  let res =
    parse_string
      "#include \"local.h\"\nint f() { return 1; }\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "void function gets trailing return on fallthrough" =
  let res = parse_string "void f() { 1; }" in
  match to_ast res with
  | [(_, A.Fundef (Ast.Void, "f", [],
      (_, A.Block [(_, A.Const (A.CInt 1)); (_, A.Return None)])))] -> true
  | _ -> false

let%test "void function does not duplicate explicit return" =
  let res = parse_string "void f() { return; }" in
  match to_ast res with
  | [(_, A.Fundef (Ast.Void, "f", [], (_, A.Return None)))] -> true
  | _ -> false

let%test "a labeled statement keeps its statement" =
  let res = parse_string "int f() { exit: return 0; }" in
  match to_ast res with
  | [(_, A.Fundef (Ast.Int, "f", [],
      (_, A.Labeled ("exit", (_, A.Return (Some (_, A.Const (A.CInt 0))))))))]
  | [(_, A.Fundef (Ast.Int, "f", [],
      (_, A.Block [(_, A.Labeled ("exit", (_, A.Return (Some (_, A.Const (A.CInt 0))))))])))] -> true
  | _ -> false
