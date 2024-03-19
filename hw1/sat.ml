open Utils 
(*** Do not open anything else ***)

type var = string  

(*** Propositional Formulas ***)
type formula = 
  | False 
  | True 
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
    | Not f -> "(not " ^ string_of_formula f ^ ")"
    | And (f1, f2) -> "(" ^ string_of_formula f1 ^ " and " ^ string_of_formula f2 ^ ")"
    | Or (f1, f2) -> "(" ^ string_of_formula f1 ^ " or " ^ string_of_formula f2 ^ ")"
    | Imply (f1, f2) -> "(" ^ string_of_formula f1 ^ " -> " ^ string_of_formula f2 ^ ")"
    | Iff (f1, f2) -> "(" ^ string_of_formula f1 ^ " <-> " ^ string_of_formula f2 ^ ")"
  
(*** CNF ***)
type literal = bool * var (* false means negated *)
type clause = literal list 
type cnf = clause list 

let string_of_literal (b, x) = if b then x else "!" ^ x 
let string_of_clause c = string_of_list string_of_literal c ~first:"(" ~last:")" ~sep:"\\/"
let string_of_cnf a = string_of_list string_of_clause a ~first:"(" ~last:")" ~sep:"/\\"
  
(*** DPLL ***)
exception Not_implemented 

(* Problem 1: CNF conversion *)
let convert : formula -> cnf 
=fun _ -> raise Not_implemented (* TODO *)

(* Problem 2: substitution a[v/x] (replacing x by v in a) *)
let subst : cnf -> bool -> var -> cnf 
=fun a v x -> ignore (a,v,x); raise Not_implemented  (* TODO *)

(* Problem 3: boolean constraint propagation *)
let (* rec *) bcp : cnf -> cnf
=fun _ -> raise Not_implemented (* TODO *)

(* Problem 4: pure literal elimination *)
let (* rec *) ple : cnf -> cnf 
=fun _ -> raise Not_implemented (* TODO*)

let choose : cnf -> var 
=fun a -> snd (List.hd (List.hd a))

let rec dpll : cnf -> bool 
=fun a ->  
  let a = ple (bcp a) in 
    if a = [] then true  (* /\ [] = true *)
    else if List.mem [] a then false (* \/ [] = false *)
    else 
      let x = choose a in 
        dpll (subst a false x) || dpll (subst a true x) 

let solve : formula -> bool 
=fun f -> dpll (convert f)