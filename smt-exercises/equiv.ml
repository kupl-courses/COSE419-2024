(* open Utils *)
open Smt 
      
type var = string 
type exp = 
  | Var of var 
  | Not of exp 
  | And of exp * exp 
  | If of exp * exp * exp 

let e1 = If (And (Not (Var "a"), 
                  Not (Var "b")), 
             Var "h", 
             If (Not (Var "a"), 
                 Var "g", 
                 Var "f"))
let e2 = If (Var "a", 
             Var "f", 
             If (Var "b", Var "g", Var "h"))

let e3 = If (And (Var "a", Var "b"), Var "a", Var "b")
let e4 = If (And (Var "a", Var "b"), Var "b", Var "a")

let check_sat f = 
  let _ = prerr_endline ("\n" ^ Fmla.to_string f) in 
  let (v, model_opt) = Solver.check_satisfiability [f] in 
  let _ = prerr_endline (Solver.string_of_satisfiability v) in 
    match model_opt with 
    | Some model -> prerr_endline (Model.to_string model); true 
    | None -> false 

let verify_equiv : exp -> exp -> bool 
=fun _ _ -> false 

let run () = 
  let _ = verify_equiv e1 e2 in 
  let _ = verify_equiv e3 e4 in 
    ()