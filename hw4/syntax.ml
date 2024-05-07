open Utils 

type id = string

type typ = 
  | T_int 
  | T_bool 
  | T_arr of typ 
  
type exp = 
  | E_int of int 
  | E_bool of bool 
  | E_lv of lv 
  | E_arr_update of id * exp * exp (* updated array *)
  | E_add of exp * exp 
  | E_sub of exp * exp 
  | E_mul of exp * exp 
  | E_div of exp * exp 
  | E_neg of exp 
  | E_len of id 
  | E_not of exp 
  | E_eq of exp * exp 
  | E_neq of exp * exp 
  | E_lt of exp * exp 
  | E_gt of exp * exp 
  | E_le of exp * exp 
  | E_ge of exp * exp 
  
and lv = V_var of id | V_arr of id * exp 

type fmla = 
  | F_exp of exp 
  | F_order of exp list * exp list 
  | F_not of fmla 
  | F_and of fmla list 
  | F_or of fmla list 
  | F_imply of fmla * fmla 
  | F_iff of fmla * fmla 
  | F_forall of id * typ option * fmla 
  | F_exists of id * typ option * fmla 
  | F_sorted of id * exp * exp 
  | F_partitioned of id * exp * exp * exp * exp

type inv = id * fmla 
type rank = exp list 

type stmt = 
  | S_seq of stmt list 
  | S_skip 
  | S_assign of lv * exp 
  | S_if of exp * stmt * stmt 
  | S_while of inv * rank option * exp * stmt 
  | S_call of id * id * exp list 
  | S_return of exp 
  | S_break 

type decl = typ * id

type pgm = {
  pre: inv; (* function precondition annotation *)
  post: inv; (* function postcondition annotation *)
  rank: rank option; (* ranking function annotation *)
  typ: typ; (* return type *)
  id: id; (* function name *)
  args: decl list; (* formal arguments *)
  locals: decl list; (* local variables *)
  stmt: stmt; (* function body *)
}

(**************************************************)

let rec replace_exp : id -> exp -> exp -> exp 
=fun x e exp -> 
  match exp with 
  | E_int n -> E_int n 
  | E_bool b -> E_bool b 
  | E_lv (V_var y) -> if x = y then e else exp 
  | E_lv (V_arr (y, e1)) -> 
    if x = y then 
      match e with 
      | E_lv (V_var z) -> E_lv (V_arr (z, replace_exp x e e1))
      | _ -> raise (Failure "Syntax.replace_exp: cannot replace array name by an expression")
    else E_lv (V_arr (y, replace_exp x e e1))
  | E_arr_update (y, e1, e2) -> 
    if x = y then 
      match e with 
      | E_lv (V_var z) -> E_arr_update (z, replace_exp x e e1, replace_exp x e e2)
      | _ -> raise (Failure "Syntax.replace_exp: cannot replace array name by an expression")
    else E_arr_update (y, replace_exp x e e1, replace_exp x e e2)
  | E_add (e1, e2) -> E_add (replace_exp x e e1, replace_exp x e e2)
  | E_sub (e1, e2) -> E_sub (replace_exp x e e1, replace_exp x e e2)
  | E_mul (e1, e2) -> E_mul (replace_exp x e e1, replace_exp x e e2)
  | E_div (e1, e2) -> E_div (replace_exp x e e1, replace_exp x e e2)
  | E_neg e1 -> E_neg (replace_exp x e e1)
  | E_len y -> 
    if x = y then 
      match e with 
      | E_lv (V_var z) -> E_len z 
      | _ -> raise (Failure "Syntax.replace_exp: cannot replace length name by an expression")
    else E_len y 
  | E_not e1 -> E_not (replace_exp x e e1)
  | E_eq (e1, e2) -> E_eq (replace_exp x e e1, replace_exp x e e2)
  | E_neq (e1, e2) -> E_neq (replace_exp x e e1, replace_exp x e e2)
  | E_lt (e1, e2) -> E_lt (replace_exp x e e1, replace_exp x e e2)
  | E_gt (e1, e2) -> E_gt (replace_exp x e e1, replace_exp x e e2)
  | E_le (e1, e2) -> E_le (replace_exp x e e1, replace_exp x e e2)
  | E_ge (e1, e2) -> E_ge (replace_exp x e e1, replace_exp x e e2)

let rec replace_fmla : id -> exp -> fmla -> fmla 
=fun x e f -> 
  match f with 
  | F_exp exp -> F_exp (replace_exp x e exp)
  | F_order (es1, es2) -> F_order (List.map (replace_exp x e) es1, List.map (replace_exp x e) es2)
  | F_not f -> F_not (replace_fmla x e f)
  | F_and fs -> F_and (List.map (replace_fmla x e) fs)
  | F_or fs -> F_or (List.map (replace_fmla x e) fs)
  | F_imply (f1, f2) -> F_imply (replace_fmla x e f1, replace_fmla x e f2)
  | F_iff (f1, f2) -> F_iff (replace_fmla x e f1, replace_fmla x e f2)
  | F_forall (y, typ, f) -> F_forall (y, typ, replace_fmla x e f)
  | F_exists (y, typ, f) -> F_exists (y, typ, replace_fmla x e f)
  | F_sorted (y, e2, e3) -> 
    if x = y then 
      match e with 
      | E_lv (V_var z) -> F_sorted (z, replace_exp x e e2, replace_exp x e e3) 
      | _ -> raise (Failure "Syntax.replace_fmla: e must be a variable here (F_sorted)")
    else F_sorted (y, replace_exp x e e2, replace_exp x e e3) 
  | F_partitioned (y, e2, e3, e4, e5) -> 
    if x = y then 
      match e with 
      | E_lv (V_var z) -> 
        F_partitioned (z, replace_exp x e e2, 
                          replace_exp x e e3, 
                          replace_exp x e e4, 
                          replace_exp x e e5)
      | _ -> raise (Failure "Syntax.replace_fmla: e must be a variable here (F_partitioned)")
    else 
      F_partitioned (y, replace_exp x e e2, 
                        replace_exp x e e3, 
                        replace_exp x e e4, 
                        replace_exp x e e5)


let create_program body pre post rank typ id args locals = {
  pre = pre; 
  post = post; 
  rank = rank; 
  typ = typ; 
  id = id; 
  args = args; 
  locals = locals; 
  stmt = body
}

let rec string_of_typ : typ -> string 
=fun t ->  
  match t with
  | T_int     -> "Int"
  | T_bool    -> "Bool"
  | T_arr t1  -> (string_of_typ t1) ^ "[]"

and string_of_exp : exp -> string
=fun e -> 
  match e with
  | E_int   n1          -> (string_of_int n1)
  | E_bool  b1          -> (if b1 then "true" else "false")
  | E_lv    v1          -> (string_of_lv v1)
  | E_arr_update   (x1, e2, e3) -> x1 ^ "<" ^ string_of_exp e2 ^ ":" ^ string_of_exp e3 ^ ">"
  | E_add   (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " + " ^ (string_of_exp e2) ^ ")"
  | E_sub   (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " - " ^ (string_of_exp e2) ^ ")"
  | E_mul   (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " * " ^ (string_of_exp e2) ^ ")"
  | E_div   (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " / " ^ (string_of_exp e2) ^ ")"
  | E_neg   e1          -> "-" ^ (string_of_exp e1)
  | E_len   id1         -> "|" ^ id1 ^ "|"
  | E_not   e1          -> "!" ^ (string_of_exp e1)
  | E_eq    (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " == " ^ (string_of_exp e2) ^ ")"
  | E_lt    (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " < "  ^ (string_of_exp e2) ^ ")"
  | E_neq   (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " != " ^ (string_of_exp e2) ^ ")"
  | E_gt    (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " > "  ^ (string_of_exp e2) ^ ")"
  | E_le    (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " <= " ^ (string_of_exp e2) ^ ")"
  | E_ge    (e1, e2)    -> "(" ^ (string_of_exp e1) ^ " >= " ^ (string_of_exp e2) ^ ")"

and string_of_lv : lv -> string
=fun v -> 
  match v with
  | V_var id1       -> id1
  | V_arr (id1, e2)  -> id1 ^ "[" ^ (string_of_exp e2) ^ "]"

let rec string_of_fmla : fmla -> string
=let ts = string_of_fmla in 
  fun f -> 
  match f with
  | F_exp     e1        -> (e1 |> string_of_exp)
  | F_order   (es1, es2) -> "(" ^ string_of_list string_of_exp es1 ^ " < " ^ string_of_list string_of_exp es2 ^ ")"
  | F_not     f1        -> "~(" ^ (f1 |> ts) ^ ")"
  | F_and     fl1       -> "(" ^ (fl1 |> List.map ts |> String.concat " && ") ^ ")"
  | F_or      fl1       -> "(" ^ (fl1 |> List.map ts |> String.concat " || ") ^ ")"
  | F_imply   (f1, f2)  -> "(" ^ (f1 |> ts) ^ " -> "  ^ (f2 |> ts) ^ ")"
  | F_iff     (f1, f2)  -> "(" ^ (f1 |> ts) ^ " <-> " ^ (f2 |> ts) ^ ")"
  | F_forall  (x1, Some t1, f2)  -> "(forall (" ^ (x1) ^ ":" ^ string_of_typ t1 ^ "). " ^ (f2 |> ts) ^ ")"
  | F_exists  (x1, Some t1, f2)  -> "(exists (" ^ (x1) ^ ":" ^ string_of_typ t1 ^ "). " ^ (f2 |> ts) ^ ")"
  | F_forall  (x1, None, f2)  -> "(forall (" ^ (x1) ^ "). " ^ (f2 |> ts) ^ ")"
  | F_exists  (x1, None, f2)  -> "(exists (" ^ (x1) ^ "). " ^ (f2 |> ts) ^ ")"
  | F_sorted (x1, e2, e3) -> "sorted(" ^ x1 ^ ", " ^ string_of_exp e2 ^ ", " ^ string_of_exp e3 ^ ")"
  | F_partitioned (x1, e2, e3, e4, e5) -> "partitioned(" ^ x1 ^ ", " ^ string_of_exp e2 ^ ", " 
                                              ^ string_of_exp e3 ^ ", " ^ string_of_exp e4 ^ ", " ^ string_of_exp e5 ^ ")"

let string_of_inv : inv -> string 
=fun (id, f) -> "@" ^ id ^ ": " ^ string_of_fmla f 

let string_of_rank : rank -> string
=fun r -> "#rank: " ^ string_of_list string_of_exp r 

let string_of_stmt : ?indent:int -> stmt -> string
=fun ?(indent=0) s -> 
  let rec inner_to_string : ?depth:int -> stmt -> string 
  =fun ?(depth=indent+1) s -> begin
    let its = inner_to_string ~depth in 
    let its_deep = inner_to_string ~depth:(depth + 1) in 
    let tb = char_of_int 9 in (* \t *)
    let idt = String.make depth tb in (* \t * depth *)
    match s with
    | S_seq     sl1               -> sl1 |> List.map its |> String.concat "\n"
    | S_skip                      -> idt ^ "skip;"
    | S_assign  (v1, e2)          -> idt ^ (v1 |> string_of_lv) ^ " := " ^ (e2 |> string_of_exp) ^ ";"
    | S_if      (e1, s2, s3)      -> idt ^ "if (" ^ (e1 |> string_of_exp) ^ ") {\n" ^
                                        (s2 |> its_deep) ^ "\n" ^
                                      idt ^ "} {\n" ^
                                        (s3 |> its_deep) ^ "\n" ^
                                      idt ^ "}"
    | S_while   (i1, r2, e3, s4)  -> idt ^ "while\n" ^
                                      idt ^ "\t" ^ (i1 |> string_of_inv) ^ "\n" ^
                                      (if (r2 |> Option.is_some) then (idt ^ "\t" ^ (r2 |> Option.value ~default:[] |> string_of_rank) ^ "\n") else "") ^
                                      idt ^ "\t(" ^ (e3 |> string_of_exp) ^ ")\n" ^
                                      idt ^ "{\n" ^
                                        (s4 |> its_deep) ^ "\n" ^
                                      idt ^ "}"
    | S_call  (ret, id1, el2)  -> idt ^ ret ^ " := " ^ id1 ^ "(" ^ string_of_list string_of_exp el2 ^ ")"
    | S_return  e1                -> idt ^ "return " ^ (e1 |> string_of_exp) ^ ";"
    | S_break                     -> idt ^ "break;"
    (* inner_to_string function end *)
  end in
  inner_to_string s
  (* to_string function end *)  

let string_of_decl (t1, id1) = (t1 |> string_of_typ) ^ " " ^ id1 ^";"

let string_of_pgm : ?indent:int -> pgm -> string
=fun ?(indent=0) { pre; post; rank; typ; id; args; locals; stmt } -> 
  let tb = char_of_int 9 in (* \t *)
  let idt = String.make indent tb in (* \t * depth *)
  idt ^ (pre |> string_of_inv) ^ "\n" ^
  idt ^ (post |> string_of_inv) ^ "\n" ^
  idt ^ (if (rank |> Option.is_some) then ((rank |> Option.value ~default:[] |> string_of_rank) ^ "\n") else "") ^
  idt ^ (typ |> string_of_typ) ^ " " ^ id ^ " (" ^ (args |> List.map string_of_decl |> String.concat ", ") ^ ") {\n" ^
  idt ^ "\t// locals: (" ^ (locals |> List.map string_of_decl |> String.concat ", ") ^ ")\n" ^
  (stmt |> string_of_stmt ~indent:(indent+1)) ^ "\n" ^
  idt ^ "}"