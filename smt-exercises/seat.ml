open Smt 
(* open Utils  *)

(* constraint *)
type const = 
  | X of int * int 
  | True 
  | False 
  | And of const list 
  | Or of const list 
  | Imply of const * const 
  | Not of const 

let encode : unit -> const
=fun () -> True (* TODO *)

let (*rec*) trans : const -> Fmla.t 
=fun _ -> Fmla.true_ () (* TODO *)
  
let run () = 
  let c = encode () in 
  let f = trans c in 
  let _ = prerr_endline ("\n" ^ Fmla.to_string f) in 
  let v, model_opt = Solver.check_satisfiability [f] in 
  let _ = prerr_endline (Solver.string_of_satisfiability v) in 
    match model_opt with 
    | Some model -> prerr_endline (Model.to_string model)
    | None -> ()