(* open Smt  *)
(* open Utils *)

type sets = int * int * (int list list)

let sets1 : sets = (6, 7, 
[
  [1;0;0;1;0;0;1]; 
  [1;0;0;1;0;0;0];
  [0;0;0;1;1;0;1]; 
  [0;0;1;0;1;1;0]; 
  [0;1;1;0;0;1;1];
  [0;1;0;0;0;0;1];
])

type formula = 
  | S of int 
  | T of int * int 
  | Bool of bool 
  | And of formula list 
  | Or of formula list 
  | Not of formula 
  | Imply of formula * formula 
  | Iff of formula * formula 
  | Neq of int * int 

let encode : sets -> formula 
=fun (n, m, t) -> ignore (n, m, t); Bool true (* TODO *)

let solve : formula -> int list 
=fun f -> ignore f; [] (* TODO *)
