open Smt 

exception Not_implemented

type var = string  

type formula = 
  | True 
  | False 
  | Var of var 
  | Not of formula 
  | And of formula * formula
  | Or of formula * formula 
  | Imply of formula * formula
  | Iff of formula * formula

let rec string_of_formula f = 
  match f with 
  | True -> "true"
  | False -> "false"
  | Var x -> x 
  | Not f -> "not " ^ string_of_formula f 
  | And (f1, f2) -> "(" ^ string_of_formula f1 ^ " and " ^ string_of_formula f2 ^ ")"
  | Or (f1, f2) -> "(" ^ string_of_formula f1 ^ " or " ^ string_of_formula f2 ^ ")"
  | Imply (f1, f2) -> "(" ^ string_of_formula f1 ^ " -> " ^ string_of_formula f2 ^ ")"
  | Iff (f1, f2) -> "(" ^ string_of_formula f1 ^ " <-> " ^ string_of_formula f2 ^ ")"

let (*rec*) trans : formula -> Fmla.t 
=fun _ -> raise Not_implemented 

let check_sat : formula -> bool * Model.t option 
=fun f -> 
  let v, model_opt = Solver.check_satisfiability [trans f] in 
    if Solver.is_sat v then           
      match model_opt with 
      | Some model -> (true, Some model)
      | None -> raise (Failure "check_sat")
    else (false, None)

let check_valid : formula -> bool * Model.t option 
=fun f -> 
  let v, model_opt = Solver.check_validity [trans f] in 
    if Solver.is_valid v then (true, None)
    else 
      match model_opt with 
      | Some model -> (false, Some model) 
      | None -> raise (Failure "check_valid")
  
let f1 = 
  Imply (
    And (
      Var "P", 
      Var "Q"
    ), 
    Imply (
      Var "P", 
      Var "Q"
    )
  )

let f2 = 
  Or (
    Imply (Var "P", Var "Q"), 
    And (
      Var "P", 
      Not (Var "Q")
    )
  )

let f3 = 
  Imply (
    Imply (
      Var "P", 
      Imply (
        Var "Q", 
        Var "R"
      )
    ), 
    Imply (
      Var "P",
      Var "Q"
    )
  )

let f4 = 
  Imply (
    Imply (
      Var "P", 
      Or (
        Var "Q", 
        Var "R"
      )  
    ), 
    Imply (
      Var "P", 
      Var "Q"
    )
  )

let f5 = 
  Imply (
    Not (And (Var "P", Var "Q")), 
    Imply ( 
      Var "R", 
      Imply (
        Not (Var "R"),
        Var "Q"
      )
    )
  )

let test f = 
  match check_valid f with 
  | true, None -> print_endline (string_of_formula f ^ " is valid")
  | false, Some m -> print_endline (string_of_formula f ^ " is invalid. Model = " ^ Model.to_string m)
  | _ -> raise (Failure "unknown") 

let run () = 
  List.iter (fun f -> 
    test f; 
    print_endline ""
  ) [f1; f2; f3; f4; f5]
