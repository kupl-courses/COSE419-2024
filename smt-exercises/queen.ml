open Smt 
open Utils 

type solution = int list 

let display_board : solution -> unit 
=fun cols -> 
  List.iter (fun row -> 
    List.iter (fun col -> 
      if col = List.nth cols row then print_string "1"
      else print_string "0" 
    ) (range 8); 
    print_endline ""
  ) (range 8) 

type exp = 
  | Q of int 
  | Int of int 
  | Sub of exp * exp 

type const = 
  | And of const list 
  | Or of const list 
  | Imply of const * const 
  | Le of exp * exp 
  | Neq of exp * exp 


let encode : unit -> const 
=fun () -> And []

let trans : const -> Fmla.t 
=fun _ -> Fmla.true_ () 

let model2solution : Model.t -> solution 
=fun _ -> [] 

let run () = 
  let c = encode () in 
  let f = trans c in 
  let _ = prerr_endline ("\n" ^ Fmla.to_string f) in 
  let v, model_opt = Solver.check_satisfiability [f] in 
  let _ = prerr_endline (Solver.string_of_satisfiability v) in 
    match model_opt with 
    | Some model -> display_board (model2solution model)
    | None -> ()
