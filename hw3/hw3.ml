(* open Smt *)
open Utils 

type pgm = var list * lib list * var 
and lib = var * var list * var * phi 
and var = string 
and phi = EQ of exp * exp 
and exp = 
  | INT of int 
  | VAR of var 
  | ADD of exp * exp  
  | MUL of exp * exp  

type spec = int list * int 

let string_of_pgm (invars, libs, outvar) = 
  "def f(" ^ string_of_list id invars ~first:"" ~last:"" ^ "): " ^ "\n" ^
  list_fold (fun (name, ins, out, _) s -> 
    s ^ "  " ^ out ^ " := " ^ name ^ string_of_list id ins ^ "\n"
  ) libs "" ^ 
  "  return " ^ outvar ^ "\n"

let verify : pgm -> spec -> bool
=fun _ _ -> false (* TODO *)

let synthesize : lib list -> spec -> pgm option 
=fun _ _ -> None (* TODO *)