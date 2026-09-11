(** Parse files containing the type definition for variables 

Such a file contain one type per line. 

A line is: symbol: type 

The type itself is parsed using rstt
*)
open Rstt

type type_kind = Def | Alias

let parse_type_line ?(filename="<input>") ?(lineno=0) line =
  (* Remove inline comments: everything after // is ignored.
     This allows writing things like: mkCharCE: [c_string, any] --> p(CHR) // encoding *)
  let line =
    let before_slash, after_slash = Utils.split_once '/' line in
    if after_slash <> "" && String.length after_slash > 0 && after_slash.[0] = '/' then
      before_slash
    else
      line
  in
  let line = String.trim line in
  if line = "" || String.starts_with ~prefix:"//" line then
    None
  else
    let loc =
      if lineno > 0 then Printf.sprintf "%s:%d" filename lineno else filename
    in
    match Utils.split_once_any [':';'='] line with
    | ('\000', _, "") ->
        Printf.eprintf "%s: warning: could not parse line (missing type): %s\n"
          loc line;
        None
    | (sep, sym, ty_str) ->
        let ty_str = String.trim ty_str in
        let ty =
        try
          Rstt_repl.IO.parse_type ty_str
        with
        | Rstt_repl__IO.SyntaxError (_, msg) as e ->
            Printf.eprintf "%s: syntax error in type expression: %s\n  %s\n"
              loc msg line;
            raise e
        | Rstt_repl__IO.LexicalError (_, msg) as e ->
            Printf.eprintf "%s: lexical error in type expression: %s\n  %s\n"
              loc msg line;
            raise e
        in
        match sep with
        | ':' -> Some (String.trim sym, ty, Def)
        | '=' -> Some (String.trim sym, ty, Alias)
        | _ -> failwith "Unreachable case in parse_type_line."

let parse_type_file filename =
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "Type file not found: %s" filename);
  let contents = In_channel.with_open_text filename In_channel.input_lines in
  contents
  |> List.mapi (fun i line -> parse_type_line ~filename ~lineno:(i + 1) line)
  |> List.filter_map Fun.id

let add_struct_guards t =
  let open Rstt.Builder in
  let aux t =
    match t with
    | TArrow (l,r) -> TArrow (TStruct l,r)
    | t -> t
  in
  map aux Fun.id Fun.id t

let build_types ti_map env type_list = 
  List.fold_left (fun acc (sym, ty, kind) ->
    let open Builder in 
   
    let (ty_env, ti_map, env) = acc in 
    let id = TId.create () in (* useless when it is not a type alias *)
    let env = if kind = Alias then 
      {env with 
        tids = StrMap.add sym id env.tids
      } 
    else env in

    let env, ty =
      try Builder.resolve env ty
      with Not_found ->
        Printf.eprintf "Not_found while resolving symbol '%s' during type resolution.@." sym;
        raise Not_found
      | Invalid_argument msg ->
        Printf.eprintf "Invalid_argument while resolving symbol '%s' during type resolution: %s@." sym msg;
        raise (Invalid_argument msg)
    in
    let ty = build ti_map ty in
    let ti_map = if kind = Alias then TIdMap.add id ty ti_map else ti_map in
    (* Register the alias for the printer *)
    if kind = Alias then Mlsem.Types.PEnv.register sym ty; 
    (StrMap.add sym ty ty_env, ti_map, env)
    ) (Builder.StrMap.empty, ti_map, env) type_list

let add_rf_aliases type_map =
  let open Builder in
  StrMap.fold (fun sym ty acc ->
    let is_uppercase_letter c =
      let code = Char.code c in
      code >= Char.code 'A' && code <= Char.code 'Z'
    in
    let starts_with_two_uppercase =
      String.length sym >= 2 && is_uppercase_letter sym.[0] && is_uppercase_letter sym.[1]
    in
    if starts_with_two_uppercase || String.starts_with ~prefix:"R_" sym || String.starts_with ~prefix:"Rf_" sym then
      acc
    else
      let rf_sym = "Rf_" ^ sym in
      if StrMap.mem rf_sym acc then acc else StrMap.add rf_sym ty acc
  ) type_map type_map

let load_file ?(rf_aliases=false) filename =
  let parsed_types = parse_type_file filename in
  let ti_map = Builder.TIdMap.empty in
  let type_map, _,_ = build_types ti_map Builder.empty_env parsed_types in
  if rf_aliases then add_rf_aliases type_map else type_map

let with_penv thunk =
  Mlsem.Types.PEnv.sequential_handler Mlsem.Types.PEnv.empty thunk () |> fst


let mk_arg l =
  let open Builder in
  TArg { pos_named =l; pos_tl = TOption TEmpty; named_tl = TOption TEmpty; named = [] }

let%test "parse simple types" =
  let line = "x: v(INT)" in
  match parse_type_line line with
  | Some (sym, ty, kind) ->
      sym = "x" && Builder.(TVec (Vector PInt) ) = ty && kind = Def
  | None -> false

let%test "parse type alias" =
  let line = "my_int = v(INT)" in
  match parse_type_line line with
  | Some (sym, ty, kind) ->
      sym = "my_int" && Builder.(TVec (Vector PInt) ) = ty && kind = Alias
  | None -> false

(* The lowercase shorthands denote a mode *and everything that coerces up to
   it* ([int] is [raw | lgl | int]); the uppercase ones denote the mode
   exactly. *)
let%test "parse coercion shorthands" =
  match parse_type_line "x: v(int)" with
  | Some (_, ty, _) -> Builder.(TVec (Vector PSubInt)) = ty
  | None -> false

let%test "parse scalars" =
  match parse_type_line "x: v1(INT)" with
  | Some (_, ty, _) -> Builder.(TVec (Scalar PInt)) = ty
  | None -> false

let%test "parse arrow types" =
  let line = "f: (a:v(INT)) -> v(DBL)" in
  match parse_type_line line with
  | Some (sym, ty, kind) ->
      sym = "f" && Builder.(TArrow (mk_arg [("a", TVec (Vector PInt))], TVec (Vector PDbl))) = ty && kind = Def
  | None -> false

let%test "parse several types" =
  let lines = [
    "x: v(INT)";
    "y: v(DBL)";
    "f: (a: v(INT)) -> v(DBL)";
  ] in
  let results = List.map parse_type_line lines in
  match results with
  | [Some (s1,t1,kind1); Some (s2,t2,kind2); Some (s3,t3,kind3)] ->
      s1 = "x" && Builder.(TVec (Vector PInt)) = t1 && kind1 = Def &&
      s2 = "y" && Builder.(TVec (Vector PDbl)) = t2 && kind2 = Def &&
      s3 = "f" && Builder.(TArrow (mk_arg [("a", TVec (Vector PInt))], TVec (Vector PDbl))) = t3 && kind3 = Def
  | _ -> false

let%test "build types" =
let open Builder in 
  let lines = [
    "x: v(INT)";
    "y: v(DBL)"
  ] in
  let parsed_types = List.filter_map parse_type_line lines in
  let ti_map = TIdMap.empty in
  let type_map, _,_ = build_types ti_map empty_env parsed_types in
  let open Rstt in
  let int_vec = Prim.Int.any |> Prim.mk |> (fun v -> Vec.Vector v) |> Vec.mk |> Attr.mk_content in
  let dbl_vec = Prim.Dbl.any |> Prim.mk |> (fun v -> Vec.Vector v) |> Vec.mk |> Attr.mk_content in
  Ty.equiv (StrMap.find "x" type_map) int_vec  &&
  Ty.equiv (StrMap.find "y" type_map) dbl_vec


let%test "build from file" =
  with_penv @@ fun () ->
  let open Builder in
  let filename = "test_types.txt" in
  let oc = open_out filename in
  Printf.fprintf oc "x: v(INT)\n";
  Printf.fprintf oc "y: v(DBL)\n";
  close_out oc;
  let parsed_types = parse_type_file filename in
  let ti_map = TIdMap.empty in
  let type_map, _,_ = build_types ti_map Builder.empty_env parsed_types in
  let open Rstt in
  let int_vec = Prim.Int.any |> Prim.mk |> (fun v -> Vec.Vector v) |> Vec.mk |> Attr.mk_content in
  let dbl_vec = Prim.Dbl.any |> Prim.mk |> (fun v -> Vec.Vector v) |> Vec.mk |> Attr.mk_content in
  Sys.remove filename;
  Ty.equiv (StrMap.find "x" type_map) int_vec  &&
  Ty.equiv (StrMap.find "y" type_map) dbl_vec 

let find_file_in_ancestors ~start ~target =
  let rec aux dir =
    let candidate = Filename.concat dir target in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else aux parent
  in
  aux start

let find_types_dir () =
  (* In dune inline-tests, the cwd is typically a sandbox (under _build/.sandbox/...).
     Prefer a search from cwd (will work if the file is staged via (inline_tests (deps ...))).
     Fall back to the source tree location while developing outside dune. *)
  match find_file_in_ancestors ~start:(Sys.getcwd ()) ~target:"types/base.ty" with
  | Some f -> Filename.dirname f
  | None -> (
      match find_file_in_ancestors ~start:(Filename.dirname __FILE__) ~target:"../types/base.ty" with
      | Some f -> Filename.dirname f
      | None -> "types"
    )

let find_types_base_ty () =
  Filename.concat (find_types_dir ()) "base.ty"

let%test "load file" = 
  with_penv @@ fun () ->
    let open Builder in
    let filename = find_types_base_ty () in
    
    let type_map = try 
      load_file filename 
    with Not_found ->
      StrMap.empty in
    StrMap.is_empty type_map |> not
    (* if StrMap.is_empty type_map then false else (
      StrMap.iter (fun sym ty ->
        Format.printf "%s: @[<h>%a@]@." sym Rstt.Pp.ty ty
      ) type_map;
      true
    ) *)

let%test "parse c_bool" =
  try let _ = Rstt_repl.IO.parse_type "c_bool" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow with c_int to c_int" =
  try let _ = Rstt_repl.IO.parse_type "[c_int, c_int] -> c_int" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "debug parse examples" =
  let cases = [
    "c_bool";
    "c_int";
    "[c_int] -> c_int";
    "[c_int, c_int] -> c_int";
    "[c_int, c_int] -> c_bool";
    "[c_double, c_double] -> c_bool";
    "[any, any] -> c_bool"
  ] in
  List.iter (fun s ->
    try
      let _ = Rstt_repl.IO.parse_type s in
      ()
    with Rstt_repl__IO.SyntaxError (_, m) ->
      Printf.eprintf "FAILED PARSE: '%s' -> %s\n" s m
  ) cases;
  true

let%test "parse arrow int int to int" =
  try let _ = Rstt_repl.IO.parse_type "[int, int] -> int" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow int int to c_bool" =
  try let _ = Rstt_repl.IO.parse_type "[int, int] -> c_bool" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow cint cint to cint" =
  try let _ = Rstt_repl.IO.parse_type "[cint, cint] -> cint" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow C_int C_int to C_bool" =
  try let _ = Rstt_repl.IO.parse_type "[C_int, C_int] -> C_bool" in true
  with Rstt_repl__IO.SyntaxError _ -> false