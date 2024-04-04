open Utils 
open Cover 
open Learn 

(* test *)
let _ = print_endline (string_of_list string_of_int (solve (encode sets1)))

(* test *)
let _ = 
  match synthesize spec1 with 
  | None -> print_endline "No solution exists"
  | Some dnf -> 
    print_endline ("solution found with m = " ^ string_of_int (List.length dnf)); 
    print_endline (string_of_dnf dnf)