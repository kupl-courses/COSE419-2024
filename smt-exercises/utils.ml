let (<<<) f g = fun x -> f (g x)
let (>>>) f g = fun x -> g (f x)
let ($>) x f = match x with Some s -> f s | None -> None
let (&>) x f = match x with Some s -> Some (f s) | None -> None
let (@) l1 l2 = List.append l1 l2
let id x = x
let flip f = fun y x -> f x y
let fst (a,_) = a 
let snd (_,b) = b

(* range2 3 5 = [3; 4] *)
let rec range2 b e = if b = e then [] else b::(range2 (b+1) e)
let range n = range2 0 n

let make_list n e = List.map (fun _ -> e) (range n)
let rec zip a b = 
  match a, b with
  | [], [] -> []
  | h::t, h'::t' -> (h, h')::(zip t t')
  | _ -> raise (Failure "zip")

let enumerate lst = zip (range (List.length lst)) lst

let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f list init ->
  List.fold_left (flip f) init list

let list_prefix lst pos = 
  list_fold (fun (idx, elem) res ->
    if idx <= pos then res @ [elem] else res 
  ) (enumerate lst) []  

let replace_list_elem l pos a  = List.mapi (fun i x -> if i = pos then a else x) l

let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first="(") ?(last=")") ?(sep=",") : ('a -> string)
  -> ('a list) -> string
= fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last

let time f x =
  let t = Unix.gettimeofday() in
  let fx = f x in
  prerr_endline ("Execution time: " ^ string_of_float (Unix.gettimeofday() -. t)); 
  fx

let time2 f x y =
  let t = Unix.gettimeofday() in
  let fx = f x y in
  prerr_endline ("Execution time: " ^ string_of_float (Unix.gettimeofday() -. t));
  fx