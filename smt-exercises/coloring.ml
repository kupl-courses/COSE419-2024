type node = int 
type edge = node * node 
type graph = node list * edge list 
type color = int 

let instance1 : graph * color list = (
  ([1; 2; 3], [(1, 2); (2, 3)]), 
  [1; 2]
)

let coloring : graph * color list -> bool * (node * color) list option 
=fun _ -> (false, None) (* TODO *)

let run () = 
  match coloring instance1 with 
  | false, _ -> print_endline "No solution"
  | true, Some sol -> 
    List.iter (fun (n, c) -> 
      print_endline ("node " ^ string_of_int n ^ " |-> color " ^ string_of_int c)
    ) sol 
  | _ -> raise (Failure "XXX")