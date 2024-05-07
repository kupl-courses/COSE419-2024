(* open Syntax
open Graph
open Utils
open Smt  *)


let verify_partial_correctness : Syntax.pgm -> bool 
=fun pgm -> 
  let cfg = Graph.pgm2cfg pgm in 
  ignore cfg; false 

let verify_termination : Syntax.pgm -> bool 
=fun pgm -> 
  let cfg = Graph.pgm2cfg pgm in 
  ignore cfg; false 

