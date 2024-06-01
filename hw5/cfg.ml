open Syntax 

module type Node = sig
  type instr = 
  | I_assign of string * aexp 
  | I_assume of bexp 
  | I_assert of bexp 
  | I_skip
  type t 
  val create_assign : string -> aexp -> t 
  val create_assume : bexp -> t 
  val create_assert : bexp -> t 
  val create_skip : unit -> t 
  val get_nodeid : t -> int 
  val get_instr : t -> instr 
  val to_string : t -> string
  val compare : t -> t -> int   
end

module Node : Node = struct
  type instr = 
  | I_assign of string * aexp 
  | I_assume of bexp 
  | I_assert of bexp 
  | I_skip
  type t = int * instr
  let new_id : unit -> int =
    let id =  ref 0 in 
      fun _ -> (id := !id + 1; !id)
  let create_assign x a = (new_id(), I_assign (x, a))
  let create_assume b = (new_id(), I_assume b)
  let create_assert b = (new_id(), I_assert b)
  let create_skip () = (new_id(), I_skip)
  let get_nodeid (id, _) = id
  let get_instr (_, instr) = instr
  let compare = Stdlib.compare
  let to_string n = 
    match n with
    | (id, I_assign (x, a)) -> string_of_int id ^ ": " ^ " " ^ x ^ " := " ^ string_of_aexp a
    | (id, I_assume b) -> string_of_int id ^ ": " ^ "assume"  ^ " " ^ string_of_bexp b
    | (id, I_assert b) -> string_of_int id ^ ": " ^ "assert"  ^ " " ^ string_of_bexp b
    | (id, I_skip) -> string_of_int id ^ ": " ^ "skip"
end

module VarMap = Map.Make(String)
module NodeSet = Set.Make(Node)
module NodeMap = Map.Make(Node)

module type Cfg = sig 
  type t 
  val empty : t 
  val nodesof : t -> Node.t list 
  val succs : Node.t -> t -> NodeSet.t
  val preds : Node.t -> t -> NodeSet.t
  val entries : t -> NodeSet.t 
  val add_node : Node.t -> t -> t
  val add_nodes : Node.t list -> t -> t
  val add_loophead : Node.t -> t -> t 
  val is_loophead : Node.t -> t -> bool 
  val add_edge : Node.t -> Node.t -> t -> t
  val print : t -> unit 
  val dot : t -> unit
end 

module Cfg : Cfg = struct
  type t = { nodes : NodeSet.t; succs : NodeSet.t NodeMap.t; preds : NodeSet.t NodeMap.t; loopheads : NodeSet.t }
  let empty = { nodes = NodeSet.empty; succs = NodeMap.empty; preds = NodeMap.empty; loopheads = NodeSet.empty }

  let nodesof : t -> Node.t list 
  =fun t -> NodeSet.elements t.nodes

  let succs : Node.t -> t -> NodeSet.t
  =fun n g -> try NodeMap.find n g.succs with _ -> NodeSet.empty

  let preds : Node.t -> t -> NodeSet.t
  =fun n g -> try NodeMap.find n g.preds with _ -> NodeSet.empty

  let entries : t -> NodeSet.t 
  =fun t -> NodeSet.filter (fun n -> NodeSet.is_empty (preds n t)) t.nodes

  let add_node : Node.t -> t -> t
  =fun n g -> { g with nodes = NodeSet.add n g.nodes }

  let add_nodes : Node.t list -> t -> t
  =fun ns g -> g |> (List.fold_right add_node ns)

  let add_loophead : Node.t -> t -> t 
  =fun n g -> {g with loopheads = NodeSet.add n g.loopheads }

  let is_loophead : Node.t -> t -> bool 
  =fun n g -> NodeSet.mem n g.loopheads 
    
  let (|>) x f = f x
  let add_edge : Node.t -> Node.t -> t -> t
  =fun n1 n2 g -> 
    g 
    |> add_nodes [n1;n2] 
    |> (fun g -> { g with succs = NodeMap.add n1 (NodeSet.add n2 (succs n1 g)) g.succs }) 
    |> (fun g -> { g with preds = NodeMap.add n2 (NodeSet.add n1 (preds n2 g)) g.preds }) 

  let print g = 
    print_endline "** Nodes **";
    NodeSet.iter (fun n -> 
      print_endline (Node.to_string n)
    ) g.nodes;
    print_endline "";
    print_endline "** Edges **";
    NodeMap.iter (fun n succs -> 
      NodeSet.iter (fun s ->
        print_endline (string_of_int (Node.get_nodeid n) ^ " -> " ^ string_of_int (Node.get_nodeid s))
      ) succs
    ) g.succs

  let dot g = 
    print_endline "digraph G {";
    NodeSet.iter (fun n -> 
      print_string (string_of_int (Node.get_nodeid n) ^ " ");
      print_string ("[label=\"" ^ Node.to_string n ^ "\"]");
      print_endline ""
    ) g.nodes;
    NodeMap.iter (fun n succs -> 
      NodeSet.iter (fun s ->
        print_endline (string_of_int (Node.get_nodeid n) ^ " -> " ^ string_of_int (Node.get_nodeid s))
      ) succs
    ) g.succs;
    print_endline "}"
end


let cmd2cfg : cmd -> Cfg.t 
=fun cmd -> 
  let rec helper : cmd -> Cfg.t -> Node.t * Cfg.t * Node.t
  =fun cmd cfg -> 
    match cmd with
    | Skip -> 
      let n = Node.create_skip () in
        (n, Cfg.add_node n cfg, n)
    | Assign (x, a) -> 
      let n = Node.create_assign x a in
        (n, Cfg.add_node n cfg, n)
    | Assert b -> 
      let n = Node.create_assert b in 
        (n, Cfg.add_node n cfg, n)
    | If (b,c1,c2) ->
      let entry = Node.create_skip () in
      let exit = Node.create_skip () in
      let n_true = Node.create_assume b in
      let n_false = Node.create_assume (Not b) in
      let (e1, cfg, x1) = helper c1 cfg in
      let (e2, cfg, x2) = helper c2 cfg in
      let cfg = 
        cfg 
        |> Cfg.add_edge entry n_true
        |> Cfg.add_edge entry n_false
        |> Cfg.add_edge n_true e1 
        |> Cfg.add_edge n_false e2 
        |> Cfg.add_edge x1 exit 
        |> Cfg.add_edge x2 exit in
        (entry, cfg, exit)
    | While (b, c) -> 
      let entry = Node.create_skip () in
      let exit = Node.create_skip () in 
      let n_true = Node.create_assume b in 
      let n_false = Node.create_assume (Not b) in
      let (e, cfg, x) = helper c cfg in 
      let cfg = 
        cfg 
        |> Cfg.add_edge entry n_true 
        |> Cfg.add_edge entry n_false 
        |> Cfg.add_edge n_true e 
        |> Cfg.add_edge x entry 
        |> Cfg.add_edge n_false exit
        |> Cfg.add_loophead entry in
        (entry, cfg, exit)
    | Seq cmds -> 
      let entry = Node.create_skip () in
      let exit = Node.create_skip () in
      let (cfg, prev_x) = 
        List.fold_left (fun (cfg, prev_x) cmd -> 
          let (e', cfg', x') = helper cmd cfg in
            (Cfg.add_edge prev_x e' cfg', x')
        ) (cfg, entry) cmds in 
        (entry, Cfg.add_edge prev_x exit cfg, exit) in
  let (_, cfg, _) = helper cmd Cfg.empty in
    cfg