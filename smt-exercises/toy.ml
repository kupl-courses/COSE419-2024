open Smt 

let check_sat f = 
  let _ = prerr_endline ("\n" ^ Fmla.to_string f) in 
  let (v, model_opt) = Solver.check_satisfiability [f] in 
  let _ = prerr_endline (Solver.string_of_satisfiability v) in 
    match model_opt with 
    | Some model -> prerr_endline (Model.to_string model)
    | None -> ()

(* Propositional Logic *)
let p = Expr.create_var (Expr.sort_of_bool ()) ~name:"p"
let q = Expr.create_var (Expr.sort_of_bool ()) ~name:"q"
let r = Expr.create_var (Expr.sort_of_bool ()) ~name:"r"
let f1 = Fmla.create_and [
  Fmla.create_imply (Fmla.create_exp p) (Fmla.create_exp q); 
  Fmla.create_iff r (Fmla.create_not (Fmla.create_exp q)); 
  Fmla.create_or [Fmla.create_not (Fmla.create_exp p); (Fmla.create_exp r)]
]

(* Integer Arithmetic *)
let x = Expr.create_var (Expr.sort_of_int ()) ~name:"x"
let y = Expr.create_var (Expr.sort_of_int ()) ~name:"y"
let f2 = Fmla.create_and [
  Fmla.create_exp (Expr.create_gt x (Expr.of_int 2)); 
  Fmla.create_exp (Expr.create_lt y (Expr.of_int 10)); 
  Fmla.create_exp (Expr.create_eq (Expr.create_add x 
                                                   (Expr.create_mul (Expr.of_int 2) 
                                                                     y)) 
                                  (Expr.of_int 7))
]

(* Real Arithmetic *)
let x = Expr.create_var (Expr.sort_of_real ()) ~name:"x"
let y = Expr.create_var (Expr.sort_of_real ()) ~name:"y"
let f3 = Fmla.create_and [
  Fmla.create_exp (Expr.create_gt (Expr.create_add (Expr.create_mul x x) 
                                                   (Expr.create_mul y y))
                                  (Expr.of_int 3)); 
  Fmla.create_exp (Expr.create_lt (Expr.create_add (Expr.create_power x (Expr.of_int 3))
                                                   y)
                                  (Expr.of_int 5))  
]

(* BitVectors *)
let x = Expr.create_var (Expr.sort_of_bitvector 32) ~name:"x"
let y = Expr.create_var (Expr.sort_of_bitvector 32) ~name:"y"
let c2 = Expr.create_bv_numeral "2" 32
let c3 = Expr.create_bv_numeral "3" 32
let c24 = Expr.create_bv_numeral "24" 32
let f4 = Expr.create_eq (Expr.create_land x y) y
let f5 = Expr.create_eq (Expr.create_shl x c2) c3
let f6 = Expr.create_eq (Expr.create_shl x c2) c24

let run () = 
  let _ = check_sat f1 in
  let _ = check_sat f2 in  
  let _ = check_sat f3 in  
  let _ = check_sat f4 in 
  let _ = check_sat f5 in 
  let _ = check_sat f6 in 
    ()