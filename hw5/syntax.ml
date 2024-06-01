type id = string (* identifier *)
type var = id 

type aexp = 
| Const of int
| Var of id
| Add of aexp * aexp
| Sub of aexp * aexp
| Mult of aexp * aexp

type bexp = 
| True 
| False
| Equal of aexp * aexp
| Le of aexp * aexp
| Not of bexp
| And of bexp * bexp

type cmd = 
| Assign of id * aexp
| Seq of cmd list
| If of bexp * cmd * cmd
| While of bexp * cmd
| Assert of bexp
| Skip 

type pgm = var list * cmd 

let rec string_of_aexp a = 
  match a with
  | Const n -> string_of_int n
  | Var x -> x
  | Add (a1, a2) -> string_of_aexp a1 ^ " + " ^ string_of_aexp a2
  | Mult (a1, a2) -> string_of_aexp a1 ^ " * " ^ string_of_aexp a2
  | Sub (a1, a2) -> string_of_aexp a1 ^ " - " ^ string_of_aexp a2

and string_of_bexp b = 
  match b with
  | True -> "true" 
  | False -> "false"
  | Equal (a1, a2) -> string_of_aexp a1 ^ " == " ^ string_of_aexp a2
  | Le (a1, a2) -> string_of_aexp a1 ^ " <= " ^ string_of_aexp a2
  | Not b -> "!(" ^ string_of_bexp b ^ ")"
  | And (b1, b2) -> string_of_bexp b1 ^ " && " ^ string_of_bexp b2