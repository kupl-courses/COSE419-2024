open Sat 

let f1 = 
  Or (
    And (Var "Q1", Var "Q2"), 
    And (Var "R1", Var "R2")
  )

let f2 = False 

let f3 = Not True 

let f4 = 
  And (
    Imply (Var "P", Var "Q"), 
    And (Var "P", Not (Var "Q"))
  )

let f5 = Or (And (Var "P", Var "Q"), Var "R")

let test () = 
  List.iter (fun f ->
    print_endline (Sat.string_of_formula f ^ " : " ^ 
      if Sat.solve f then "SAT" else "UNSAT")  
  ) [f1; f2; f3; f4; f5]

let _ = test ()