(** Generate a call graph of the functions from the PAst

But maybe it is better rather do a dependence graph of the includes in C. That should be enough. 
The interesting feature of the call graph is that we can filter out calls that are not defined
*)

module Callgraph = struct
  type t = {
    name_to_id: (string, int) Hashtbl.t;
    mutable id_to_name: string array;
    mutable succ: int list array;
  }

  let create ?(capacity = 16) () =
    {
      name_to_id = Hashtbl.create capacity;
      id_to_name = Array.make 0 "";
      succ = Array.make 0 [];
    }

  let add_node t name =
    match Hashtbl.find_opt t.name_to_id name with
    | Some id -> id
    | None ->
        let id = Array.length t.id_to_name in
        Hashtbl.add t.name_to_id name id;
        t.id_to_name <- Array.append t.id_to_name [| name |];
        t.succ <- Array.append t.succ [| [] |];
        id

  let add_edge t ~caller ~callee =
    let caller_id = add_node t caller in
    let callee_id = add_node t callee in
    t.succ.(caller_id) <- callee_id :: t.succ.(caller_id)

  let of_adjacency edges =
    let g = create ~capacity:(List.length edges) () in
    List.iter
      (fun (caller, callees) ->
        let caller_id = add_node g caller in
        let callee_ids = List.map (add_node g) callees in
        g.succ.(caller_id) <- List.rev_append callee_ids g.succ.(caller_id))
      edges;
    g

  let node_count t = Array.length t.id_to_name

  let has_node t name = Hashtbl.mem t.name_to_id name

  let id_of_name t name = Hashtbl.find_opt t.name_to_id name

  let name_of_id t id =
    if id < 0 || id >= Array.length t.id_to_name then None
    else Some t.id_to_name.(id)

  let successors t id =
    if id < 0 || id >= Array.length t.succ then [] else t.succ.(id)

  let successors_by_name t name =
    match id_of_name t name with
    | None -> []
    | Some id ->
        List.filter_map (name_of_id t) (successors t id)

  let iter_nodes t f = Array.iter f t.id_to_name

  let iter_edges t f =
    Array.iteri
      (fun caller_id callees ->
        List.iter (fun callee_id -> f caller_id callee_id) callees)
      t.succ

  let fold_edges t init f =
    let acc = ref init in
    iter_edges t (fun caller_id callee_id -> acc := f !acc caller_id callee_id);
    !acc

  let dfs_from_id t start_id =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let rec visit acc id =
      if id < 0 || id >= n || visited.(id) then acc
      else (
        visited.(id) <- true;
        let acc' = id :: acc in
        List.fold_left visit acc' t.succ.(id))
    in
    List.rev (visit [] start_id)

  let dfs_from_name t name =
    match id_of_name t name with
    | None -> []
    | Some id -> dfs_from_id t id

  let bfs_from_id t start_id =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let queue = Queue.create () in
    let acc = ref [] in
    if start_id >= 0 && start_id < n then (
      visited.(start_id) <- true;
      Queue.add start_id queue);
    while not (Queue.is_empty queue) do
      let id = Queue.take queue in
      acc := id :: !acc;
      List.iter
        (fun succ_id ->
          if succ_id >= 0 && succ_id < n && not visited.(succ_id) then (
            visited.(succ_id) <- true;
            Queue.add succ_id queue))
        t.succ.(id)
    done;
    List.rev !acc

  let bfs_from_name t name =
    match id_of_name t name with
    | None -> []
    | Some id -> bfs_from_id t id

  let scc t =
    (* Kosaraju's algorithm: order by finish time, then DFS on transposed graph. *)
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let order = ref [] in
    let rec dfs1 id =
      if not visited.(id) then (
        visited.(id) <- true;
        List.iter dfs1 t.succ.(id);
        order := id :: !order)
    in
    for id = 0 to n - 1 do
      dfs1 id
    done;
    let rev_succ = Array.make n [] in
    for u = 0 to n - 1 do
      List.iter (fun v -> rev_succ.(v) <- u :: rev_succ.(v)) t.succ.(u)
    done;
    Array.fill visited 0 n false;
    let rec dfs2 acc id =
      if visited.(id) then acc
      else (
        visited.(id) <- true;
        List.fold_left dfs2 (id :: acc) rev_succ.(id))
    in
    let components = ref [] in
    List.iter
      (fun id ->
        if not visited.(id) then
          let comp = dfs2 [] id in
          components := comp :: !components)
      !order;
    List.rev !components

  let topo_sort t =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let result = ref [] in
    (* More efficient than calling the dfs_from_id multiple times as 
    we can share the visited array. *)
    let rec visit id =
      if not visited.(id) then (
        visited.(id) <- true;
        List.iter visit t.succ.(id);
        result := id :: !result)
    in
    for id = 0 to n - 1 do
      visit id
    done;
    List.rev !result

  let topo_sort_names t =
    List.filter_map (name_of_id t) (topo_sort t)

  let in_degree t =
    let n = Array.length t.id_to_name in
    let deg = Array.make n 0 in
    iter_edges t (fun _caller callee -> deg.(callee) <- deg.(callee) + 1);
    deg

  let topo_sort_kahn t =
    let deg = in_degree t in
    let queue = Queue.create () in
    Array.iteri (fun id d -> if d = 0 then Queue.add id queue) deg;
    let result = ref [] in
    while not (Queue.is_empty queue) do
      let u = Queue.take queue in
      result := u :: !result;
      List.iter
        (fun v ->
          deg.(v) <- deg.(v) - 1;
          if deg.(v) = 0 then Queue.add v queue)
        t.succ.(u)
    done;
    List.rev !result

  (** Compute all nodes reachable from a list of starting node IDs *)
  let reachable_from_ids t start_ids =
    let n = Array.length t.id_to_name in
    let visited = Array.make n false in
    let result = ref [] in
    let rec visit id =
      if id >= 0 && id < n && not visited.(id) then (
        visited.(id) <- true;
        result := id :: !result;
        List.iter visit t.succ.(id))
    in
    List.iter visit start_ids;
    List.rev !result

  (** Compute all nodes reachable from a list of starting node names *)
  let reachable_from_names t start_names =
    let start_ids = List.filter_map (id_of_name t) start_names in
    let reachable_ids = reachable_from_ids t start_ids in
    List.filter_map (name_of_id t) reachable_ids

  (* Inline tests *)
  let%test "reachable from single node" =
    let g = create () in
    let _a = add_node g "a" in
    let _b = add_node g "b" in
    let _c = add_node g "c" in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"b" ~callee:"c";
    let reachable = reachable_from_names g ["a"] in
    reachable = ["a"; "b"; "c"]

  let%test "reachable from multiple nodes" =
    let g = create () in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"c" ~callee:"d";
    add_edge g ~caller:"b" ~callee:"e";
    let reachable = reachable_from_names g ["a"; "c"] in
    List.sort String.compare reachable = ["a"; "b"; "c"; "d"; "e"]

  let%test "reachable with cycles" =
    let g = create () in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"b" ~callee:"c";
    add_edge g ~caller:"c" ~callee:"a";
    let reachable = reachable_from_names g ["a"] in
    List.sort String.compare reachable = ["a"; "b"; "c"]

  let%test "reachable from disconnected nodes" =
    let g = create () in
    add_edge g ~caller:"a" ~callee:"b";
    add_edge g ~caller:"c" ~callee:"d";
    let reachable = reachable_from_names g ["x"; "y"] in
    reachable = []
end


(** Collect all defined function names from a list of PAst definitions *)
let collect_fun_names defs_list =
  let rec collect_unit acc (_pos, unit') =
    match unit' with
    | PAst.Fundef (_, fname, _, _) | PAst.MacroDef (fname, _, _) -> PAst.StrSet.add fname acc
    | PAst.Include items -> List.fold_left collect_unit acc items
    | _ -> acc
  in
  List.fold_left (fun acc defs ->
    List.fold_left collect_unit acc defs
  ) PAst.StrSet.empty defs_list

(** Build a call graph from a single PAst top-level unit *)
let rec of_past_unit ~fun_names _t (_pos, unit') =
  match unit' with
  | PAst.Fundef (_, fname, _, body) | PAst.MacroDef (fname, _, body) ->
      (* A macro and a function of the same name share a node: the graph only
         orders definitions, and [topo_sort] schedules both.
         Always register the defining function as a node, even if its body has
         no callees in [fun_names]. Otherwise leaf functions (e.g. platform
         stubs returning a constant) would be dropped by [keep_reachable]
         even when they are entry points themselves. *)
      let _ = Callgraph.add_node _t fname in
      let callees = PAst.extract_calls_from_expr ~fun_names body in
      List.iter (fun callee -> Callgraph.add_edge _t ~caller:fname ~callee) callees
  | PAst.TypeDecl _ -> ()
  | PAst.GlobalVar _ -> ()
  | PAst.Define _ -> ()
  | PAst.Include items -> List.iter (of_past_unit ~fun_names _t) items

(** Build a call graph from a PAst definition (list of top-level units) *)
let of_past defs =
  let fun_names = collect_fun_names [defs] in
  let t = Callgraph.create ~capacity:(List.length defs) () in
  List.iter (of_past_unit ~fun_names t) defs;
  t

(** Build a call graph from multiple PAst definitions *)
let of_past_list defs_list =
  let fun_names = collect_fun_names defs_list in
  let total = List.fold_left (fun acc defs -> acc + List.length defs) 0 defs_list in
  let t = Callgraph.create ~capacity:total () in
  List.iter (List.iter (of_past_unit ~fun_names t)) defs_list;
  t

let keep_reachable t entry_points =
  let reachable_names = Callgraph.reachable_from_names t entry_points in
  let module StrSet = Set.Make(String) in
  let reachable_set = StrSet.of_list reachable_names in
  let new_graph = Callgraph.create ~capacity:(List.length reachable_names) () in
  (* First add every reachable name as a node so leaf nodes (no outgoing
     edges) survive the filter. [add_edge] alone would drop reachable leaves
     because nodes are only created as a side effect of edge insertion. *)
  List.iter (fun n -> let _ = Callgraph.add_node new_graph n in ()) reachable_names;
  (* Add edges only between reachable nodes *)
  List.iter (fun caller ->
    let callees = Callgraph.successors_by_name t caller in
    List.iter (fun callee ->
      if StrSet.mem callee reachable_set then
        Callgraph.add_edge new_graph ~caller ~callee
    ) callees
  ) reachable_names;
  new_graph

(* --- Graphviz (.dot) export ----------------------------------------------- *)

(** Sanitize a string for use as a dot identifier (only [[A-Za-z0-9_]]
    chars survive; everything else becomes [_]). Used for cluster ids. *)
let sanitize_id s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> Buffer.add_char b c
    | _ -> Buffer.add_char b '_') s;
  Buffer.contents b

(** Escape a string for use inside a quoted dot label/id. *)
let escape_dot s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' | '\\' -> Buffer.add_char b '\\'; Buffer.add_char b c
    | _ -> Buffer.add_char b c) s;
  Buffer.contents b

(** Wrap a long C identifier across multiple lines for use as a dot label.
    Splits at underscores and packs segments into lines of at most
    ~[max_line] characters. Names <= [max_line] are returned unchanged.
    Returns a literal "\\n" sequence (which dot interprets as a newline
    inside a quoted label). *)
let wrap_label ?(max_line=14) name =
  if String.length name <= max_line then name
  else
    let parts = String.split_on_char '_' name in
    if List.length parts <= 1 then name
    else
      let lines = ref [] in
      let cur = Buffer.create max_line in
      List.iter (fun p ->
        let part_len = String.length p in
        if Buffer.length cur = 0 then
          Buffer.add_string cur p
        else if Buffer.length cur + 1 + part_len > max_line then begin
          (* Keep the trailing underscore on the current line so readers
             see this is a single identifier broken across lines. *)
          Buffer.add_char cur '_';
          lines := Buffer.contents cur :: !lines;
          Buffer.clear cur;
          Buffer.add_string cur p
        end else begin
          Buffer.add_char cur '_';
          Buffer.add_string cur p
        end
      ) parts;
      if Buffer.length cur > 0 then lines := Buffer.contents cur :: !lines;
      String.concat "\\n" (List.rev !lines)

(** Emit the call graph in Graphviz .dot format.

    - [name]: top-level digraph identifier (default ["callgraph"]).
    - [file_of_node]: optional callback returning the source file a node
      came from. When supplied, nodes are grouped into [subgraph cluster_*]
      blocks per file.
    - [entry_points]: nodes that should be visually marked as roots
      ([shape=doublecircle]).
    - [highlight_scc]: when [true] (default), members of non-trivial
      strongly-connected components get a fill color; trivial SCCs (a
      single node with no self-loop) are left uncolored. Useful for
      spotting cycles like [df_proxy <-> vec_proxy_equal] at a glance. *)
let to_dot
    ?(name="callgraph")
    ?(file_of_node=(fun _ -> None))
    ?(entry_points=[])
    ?(highlight_scc=true)
    t =
  let module StrSet = Set.Make(String) in
  let module StrMap = Map.Make(String) in
  let n = Callgraph.node_count t in
  (* Compute per-node SCC index and component sizes when highlighting. *)
  let scc_index = Array.make n (-1) in
  let scc_size_of_idx = ref [||] in
  if highlight_scc then begin
    let comps = Callgraph.scc t in
    let sizes = Array.make (List.length comps) 0 in
    List.iteri (fun i comp ->
      sizes.(i) <- List.length comp;
      List.iter (fun id -> scc_index.(id) <- i) comp
    ) comps;
    scc_size_of_idx := sizes
  end;
  let palette =
    [| "lightcoral"; "lightskyblue"; "palegreen"; "khaki"; "lightpink";
       "lightsalmon"; "paleturquoise"; "plum"; "wheat"; "thistle" |]
  in
  let color_of_id id =
    if not highlight_scc then None
    else
      let i = scc_index.(id) in
      if i < 0 then None
      else
        let size = (!scc_size_of_idx).(i) in
        if size <= 1 then None
        else Some palette.(i mod Array.length palette)
  in
  let entry_set = List.fold_left (fun s e -> StrSet.add e s) StrSet.empty entry_points in
  let node_attrs name id =
    let parts = ref [] in
    let wrapped = wrap_label name in
    if wrapped <> name then
      (* The label already contains literal [\n] separators that dot must
         see as the two-character escape sequence; don't run it through
         [escape_dot] (which would double the backslashes and turn the
         line breaks into a literal "\n" in the output). C identifier
         characters never need escaping themselves. *)
      parts := (Printf.sprintf "label=\"%s\"" wrapped) :: !parts;
    let is_entry = StrSet.mem name entry_set in
    let scc_fill = color_of_id id in
    (* Composition rule for [style] / [fillcolor]:
       - SCC member only         => filled with SCC color
       - Entry point only        => filled with [lightyellow]
       - Both                    => filled with SCC color + [bold] outline
                                     (fill is overridden by SCC; the bold
                                     outline keeps the entry-point signal)
       - Entry point alone is already "bold,filled" so the double-marker
         case is just an additive style. *)
    let style_parts = ref [] in
    if is_entry then style_parts := "bold" :: !style_parts;
    (match scc_fill with
     | Some c ->
         parts := ("fillcolor=" ^ c) :: !parts;
         style_parts := "filled" :: !style_parts
     | None ->
         if is_entry then begin
           parts := "fillcolor=lightyellow" :: !parts;
           style_parts := "filled" :: !style_parts
         end);
    (match !style_parts with
     | [] -> ()
     | [s] -> parts := ("style=" ^ s) :: !parts
     | xs -> parts := (Printf.sprintf "style=\"%s\"" (String.concat "," xs)) :: !parts);
    match !parts with
    | [] -> ""
    | xs -> " [" ^ String.concat "," xs ^ "]"
  in
  (* Per-cluster background tints — soft pastel palette, distinct from edge
     black and from SCC fill colors. Each file cluster cycles through this. *)
  let cluster_palette =
    [| "#fff8e1"; "#e3f2fd"; "#f3e5f5"; "#e8f5e9"; "#fce4ec";
       "#fff3e0"; "#e0f7fa"; "#f1f8e9"; "#fdedef"; "#ede7f6" |]
  in
  (* Group nodes by file (or "" for unknown). Iterate ids 0..n-1 to keep
     a stable, source-order layout per file. *)
  let by_file = ref StrMap.empty in
  for id = 0 to n - 1 do
    match Callgraph.name_of_id t id with
    | None -> ()
    | Some name ->
        let f = match file_of_node name with Some f -> f | None -> "" in
        let prev = try StrMap.find f !by_file with Not_found -> [] in
        by_file := StrMap.add f ((id, name) :: prev) !by_file
  done;
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Printf.sprintf "digraph %s {\n" (sanitize_id name));
  Buffer.add_string buf "  rankdir=LR;\n";
  (* [splines=ortho]: dot's default spline router fails
     ([routesplines: Pshortestpath failed]) on dense graphs with many
     clusters and produces SVGs that have nodes but no edges. Orthogonal
     routing bypasses that solver and renders reliably; [splines=line]
     also fails on the same inputs. *)
  Buffer.add_string buf "  splines=ortho;\n";
  Buffer.add_string buf "  node [shape=box,fontname=Courier];\n";
  let cluster_idx = ref 0 in
  StrMap.iter (fun file nodes ->
    let nodes = List.rev nodes in
    if file = "" then
      List.iter (fun (id, n) ->
        Buffer.add_string buf
          (Printf.sprintf "  \"%s\"%s;\n" (escape_dot n) (node_attrs n id))
      ) nodes
    else begin
      let bg = cluster_palette.(!cluster_idx mod Array.length cluster_palette) in
      incr cluster_idx;
      Buffer.add_string buf
        (Printf.sprintf "  subgraph cluster_%s {\n" (sanitize_id file));
      Buffer.add_string buf
        (Printf.sprintf
           "    label=\"%s\";\n    style=\"rounded,filled\";\n    bgcolor=\"%s\";\n    color=\"#888888\";\n    penwidth=2;\n"
           (escape_dot file) bg);
      List.iter (fun (id, n) ->
        Buffer.add_string buf
          (Printf.sprintf "    \"%s\"%s;\n" (escape_dot n) (node_attrs n id))
      ) nodes;
      Buffer.add_string buf "  }\n"
    end
  ) !by_file;
  (* Edges: emit at top level (dot understands cross-cluster edges fine).
     [iter_edges] yields ids; resolve back to names. *)
  Callgraph.iter_edges t (fun caller_id callee_id ->
    match Callgraph.name_of_id t caller_id, Callgraph.name_of_id t callee_id with
    | Some c, Some d ->
        Buffer.add_string buf
          (Printf.sprintf "  \"%s\" -> \"%s\";\n" (escape_dot c) (escape_dot d))
    | _ -> ());
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let topo_sort pasts call_graph =
  let past_map = Hashtbl.create (List.length pasts) in
  (* Function-like macros, apart: one may share its name with a function,
     and both must be typed. *)
  let macro_map = Hashtbl.create 16 in
  List.iter (fun (filename, past) ->
    match past with
      | _, PAst.MacroDef (name, _, _) -> Hashtbl.replace macro_map name (filename, past)
      | _, PAst.Fundef (_, name, _, (_, PAst.Seq [])) ->
          (* Declaration (empty body) — only register if no entry exists yet *)
          if not (Hashtbl.mem past_map name) then
            Hashtbl.replace past_map name (filename, past)
      | _, PAst.Fundef (_, name, _, _) ->
          (* Definition (has body) — always takes priority *)
          Hashtbl.replace past_map name (filename, past)
      | _, PAst.TypeDecl _ -> ()
      | _, PAst.GlobalVar _ -> ()
      | _, PAst.Define _ -> ()
      | _, PAst.Include _ -> ()
  ) pasts;
  let sorted_names = Callgraph.topo_sort_names call_graph in
  (* For a name with both, the function first: a macro over a function of the
     same name ([#define f(x) (f)(x, 0)]) calls it. *)
  List.concat_map (fun name ->
      List.filter_map (fun tbl -> Hashtbl.find_opt tbl name) [ past_map ; macro_map ])
    sorted_names


(* Inline tests *)

(* Inline test for keep_reachable *)
let%test "keep_reachable filters unreachable nodes" =
  let g = Callgraph.create () in
  Callgraph.add_edge g ~caller:"main" ~callee:"helper1";
  Callgraph.add_edge g ~caller:"helper1" ~callee:"util";
  Callgraph.add_edge g ~caller:"unused" ~callee:"dead_code";
  let filtered = keep_reachable g ["main"] in
  Callgraph.node_count filtered = 3 &&
  Callgraph.has_node filtered "main" &&
  Callgraph.has_node filtered "helper1" &&
  Callgraph.has_node filtered "util" &&
  not (Callgraph.has_node filtered "unused") &&
  not (Callgraph.has_node filtered "dead_code")

let%test "keep_reachable preserves edges" =
  let g = Callgraph.create () in
  Callgraph.add_edge g ~caller:"a" ~callee:"b";
  Callgraph.add_edge g ~caller:"b" ~callee:"c";
  let filtered = keep_reachable g ["a"] in
  Callgraph.successors_by_name filtered "a" = ["b"] &&
  Callgraph.successors_by_name filtered "b" = ["c"]

let%test "keep_reachable with multiple entry points" =
  let g = Callgraph.create () in
  Callgraph.add_edge g ~caller:"main1" ~callee:"shared";
  Callgraph.add_edge g ~caller:"main2" ~callee:"shared";
  Callgraph.add_edge g ~caller:"unused" ~callee:"dead";
  let filtered = keep_reachable g ["main1"; "main2"] in
  Callgraph.node_count filtered = 3 &&
  not (Callgraph.has_node filtered "unused")

let test_pos = Mlsem.Common.Position.dummy

let mk_e e' =
  (test_pos, e')

let mk_call name args =
  mk_e (PAst.Call (mk_e (PAst.Id name), args))

let mk_fundef ?(body = mk_e (PAst.Return None)) name =
  (test_pos, PAst.Fundef (Ast.Any, name, [], body))

let mk_decl name =
  mk_fundef ~body:(mk_e (PAst.Seq [])) name

let%test "callgraph duplicate nodes and invalid lookups are stable" =
  let g = Callgraph.create () in
  let id1 = Callgraph.add_node g "f" in
  let id2 = Callgraph.add_node g "f" in
  id1 = id2
  && Callgraph.node_count g = 1
  && Callgraph.id_of_name g "missing" = None
  && Callgraph.name_of_id g (-1) = None
  && Callgraph.name_of_id g 99 = None
  && Callgraph.successors g 99 = []
  && Callgraph.successors_by_name g "missing" = []

let%test "of_adjacency fold_edges and in_degree agree on a small DAG" =
  let g =
    Callgraph.of_adjacency
      [ ("a", ["b"; "c"]);
        ("b", ["c"]);
        ("c", []) ]
  in
  let edge_count = Callgraph.fold_edges g 0 (fun acc _ _ -> acc + 1) in
  let deg = Callgraph.in_degree g in
  match Callgraph.id_of_name g "a", Callgraph.id_of_name g "b", Callgraph.id_of_name g "c" with
  | Some a, Some b, Some c ->
      edge_count = 3
      && deg.(a) = 0
      && deg.(b) = 1
      && deg.(c) = 2
  | _ -> false

let%test "dfs bfs scc and kahn cover cyclic and acyclic cases" =
  let cyclic =
    Callgraph.of_adjacency
      [ ("a", ["b"]);
        ("b", ["c"]);
        ("c", ["a"]);
        ("d", []) ]
  in
  let acyclic =
    Callgraph.of_adjacency
      [ ("start", ["mid1"; "mid2"]);
        ("mid1", ["finish"]);
        ("mid2", ["finish"]);
        ("finish", []) ]
  in
  let cyclic_components =
    Callgraph.scc cyclic
    |> List.map (List.filter_map (Callgraph.name_of_id cyclic))
    |> List.map (List.sort String.compare)
    |> List.sort compare
  in
  let bfs_names =
    match Callgraph.id_of_name acyclic "start" with
    | None -> []
    | Some id -> Callgraph.bfs_from_id acyclic id |> List.filter_map (Callgraph.name_of_id acyclic)
  in
  let kahn_names = Callgraph.topo_sort_kahn acyclic |> List.filter_map (Callgraph.name_of_id acyclic) in
  let kahn_cycle = Callgraph.topo_sort_kahn cyclic in
  Callgraph.dfs_from_name cyclic "missing" = []
  && List.length bfs_names = 4
  && List.hd bfs_names = "start"
  && List.sort String.compare bfs_names = ["finish"; "mid1"; "mid2"; "start"]
  && cyclic_components = [["a"; "b"; "c"]; ["d"]]
  && List.length kahn_names = 4
  && List.hd kahn_names = "start"
  && List.mem "finish" kahn_names
  && kahn_cycle = [match Callgraph.id_of_name cyclic "d" with Some id -> id | None -> -1]

let%test "collect_fun_names includes nested include fundefs" =
  let defs =
    [ mk_fundef "top";
      (test_pos, PAst.Include [mk_fundef "nested"; (test_pos, PAst.Define ("X", PAst.CInt 1))]) ]
  in
  let names = collect_fun_names [defs] in
  PAst.StrSet.mem "top" names
  && PAst.StrSet.mem "nested" names
  && not (PAst.StrSet.mem "X" names)

let%test "of_past and of_past_list build edges from function bodies" =
  let caller_body = mk_e (PAst.Seq [mk_call "callee" []; mk_call "ext" []]) in
  let defs1 = [mk_fundef ~body:caller_body "caller"; mk_fundef "callee"] in
  let defs2 = [mk_fundef ~body:(mk_e (PAst.Seq [mk_call "caller" []])) "other"] in
  let single = of_past defs1 in
  let merged = of_past_list [defs1; defs2] in
  List.sort String.compare (Callgraph.successors_by_name single "caller") = ["callee"; "ext"]
  && Callgraph.has_node single "caller"
  && Callgraph.has_node single "callee"
  && Callgraph.has_node single "ext"
  && Callgraph.successors_by_name merged "other" = ["caller"]

let%test "topo_sort prefers function definitions over declarations" =
  let foo_decl = ("decl.c", mk_decl "foo") in
  let foo_def = ("def.c", mk_fundef ~body:(mk_e (PAst.Return (Some (mk_e (PAst.Const (PAst.CInt 1)))))) "foo") in
  let bar_def = ("bar.c", mk_fundef ~body:(mk_e (PAst.Seq [mk_call "foo" []; mk_e (PAst.Return None)])) "bar") in
  let graph = of_past_list [[snd foo_decl; snd foo_def; snd bar_def]] in
  let sorted = topo_sort [foo_decl; foo_def; bar_def] graph in
  List.mem foo_def sorted
  && not (List.mem foo_decl sorted)
  && List.mem bar_def sorted

let%test "to_dot emits nodes, edges, clusters, entry-point and SCC styling" =
  (* Graph: cyclic 2-SCC {a,b}, standalone leaf c reachable from a, plus
     standalone d that is an entry point but not in any non-trivial SCC.
     a, b live in foo.c; c, d live in bar.c. a and d are entry points. *)
  let g =
    Callgraph.of_adjacency
      [ ("a", ["b"; "c"]);
        ("b", ["a"]);
        ("c", []);
        ("d", []) ]
  in
  let file_of = function
    | "a" | "b" -> Some "foo.c"
    | "c" | "d" -> Some "bar.c"
    | _ -> None
  in
  let dot = to_dot ~file_of_node:file_of ~entry_points:["a"; "d"] g in
  let contains s = try ignore (Str.search_forward (Str.regexp_string s) dot 0); true
                   with Not_found -> false in
  contains "digraph callgraph"
  && contains "rankdir=LR"
  && contains "\"a\" -> \"b\""
  && contains "\"b\" -> \"a\""
  && contains "\"a\" -> \"c\""
  && contains "subgraph cluster_foo_c"
  && contains "subgraph cluster_bar_c"
  (* Entry point alone (d): [lightyellow] fill, [bold] in style. *)
  && contains "fillcolor=lightyellow"
  (* Entry point that's also in a non-trivial SCC (a): SCC color wins for
     fill; [bold] is added to keep the entry-point signal. The order
     within [style=".."] is implementation-defined. *)
  && contains "style=\"filled,bold\""
  && contains "fillcolor="
  && contains "}\n"

let%test "to_dot leaves trivial SCCs uncoloured when highlight_scc=true" =
  (* Pure DAG — every node is its own trivial SCC, none should be filled. *)
  let g = Callgraph.of_adjacency [ ("x", ["y"]); ("y", ["z"]); ("z", []) ] in
  let dot = to_dot g in
  not (try ignore (Str.search_forward (Str.regexp_string "fillcolor=") dot 0); true
       with Not_found -> false)

let%test "to_dot escapes quotes and backslashes in node names" =
  let g = Callgraph.of_adjacency [ ({|weird"name|}, [{|back\slash|}]); ({|back\slash|}, []) ] in
  let dot = to_dot g in
  let contains s = try ignore (Str.search_forward (Str.regexp_string s) dot 0); true
                   with Not_found -> false in
  contains {|"weird\"name"|}
  && contains {|"back\\slash"|}

let%test "wrap_label leaves short names alone and wraps long ones at underscores" =
  wrap_label "foo" = "foo"
  && wrap_label "vec_size" = "vec_size"
  (* Long, multi-underscore name: split into multiple lines, with [\n]
     literals between groups and trailing [_] kept on each non-final line. *)
  && wrap_label ~max_line:14 "vec_proxy_equal_impl" = "vec_proxy_\\nequal_impl"
  (* Long single-token name without underscores: unchanged (nothing to split). *)
  && wrap_label "abcdefghijklmnopqrstuvwxyz" = "abcdefghijklmnopqrstuvwxyz"

let%test "to_dot adds a wrapped label= attribute for long node names" =
  let g = Callgraph.of_adjacency [ ("vec_proxy_equal_impl", []); ("short", []) ] in
  let dot = to_dot g in
  let contains s = try ignore (Str.search_forward (Str.regexp_string s) dot 0); true
                   with Not_found -> false in
  (* Long name gets a label= attribute with a literal \n inside. *)
  contains "label=\"vec_proxy_\\nequal_impl\""
  (* Short name doesn't (would just duplicate the id). *)
  && not (contains "label=\"short\"")

let%test "to_dot tints file clusters with bgcolor and a darker border" =
  let g = Callgraph.of_adjacency [ ("foo", []); ("bar", []) ] in
  let file_of = function "foo" -> Some "a.c" | "bar" -> Some "b.c" | _ -> None in
  let dot = to_dot ~file_of_node:file_of g in
  let contains s = try ignore (Str.search_forward (Str.regexp_string s) dot 0); true
                   with Not_found -> false in
  contains "bgcolor=\"#"
  && contains "color=\"#888888\""
  && contains "style=\"rounded,filled\""


