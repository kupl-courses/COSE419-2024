open Utils
open Hw3

(* tests *)
let p1 : string * lib list * spec = ("f(x, y) = x + y", 
  [
    ("add", ["i11"; "i12"], "o1", EQ (VAR "o1", ADD (VAR "i11", VAR "i12")));
  ], 
  ([2; 3], 5)
)

let p2 : string * lib list * spec = ("f(x) = x^8", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("mul", ["i21"; "i22"], "o2", EQ (VAR "o2", MUL (VAR "i21", VAR "i22")));
    ("mul", ["i31"; "i32"], "o3", EQ (VAR "o3", MUL (VAR "i31", VAR "i32")));
  ], 
  ([2], 256)
)

let p3 : string * lib list * spec = ("f(a,b,c,h) = a * b^h + b * h + c", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("mul", ["i21"; "i22"], "o2", EQ (VAR "o2", MUL (VAR "i21", VAR "i22")));
    ("add", ["i31"; "i32"], "o3", EQ (VAR "o3", ADD (VAR "i31", VAR "i32")));
    ("add", ["i41"; "i42"], "o4", EQ (VAR "o4", ADD (VAR "i41", VAR "i42")));
  ], 
  ([1; 2; 3; 4], 27)
)

let p4 : string * lib list * spec = ("f(x) = x^31", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("mul", ["i21"; "i22"], "o2", EQ (VAR "o2", MUL (VAR "i21", VAR "i22")));
    ("mul", ["i31"; "i32"], "o3", EQ (VAR "o3", MUL (VAR "i31", VAR "i32")));
    ("mul", ["i41"; "i42"], "o4", EQ (VAR "o4", MUL (VAR "i41", VAR "i42")));
    ("mul", ["i51"; "i52"], "o5", EQ (VAR "o5", MUL (VAR "i51", VAR "i52")));
    ("mul", ["i61"; "i62"], "o6", EQ (VAR "o6", MUL (VAR "i61", VAR "i62")));
    ("mul", ["i71"; "i72"], "o7", EQ (VAR "o7", MUL (VAR "i71", VAR "i72")));
  ], 
  ([2], 2147483648)
)

let p5 : string * lib list * spec = ("f(x) = (2*(x-1))^2", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("sub1", ["i2"], "o2", EQ (ADD (VAR "o2", INT 1), VAR "i2"));
    ("mul2", ["i3"], "o3", EQ (VAR "o3", MUL (VAR "i3", INT 2)));
  ], 
  ([5], 64)
) 

let test () = 
  List.iter (fun (idx, (desc, libs, spec)) ->
    print_endline (string_of_int (idx + 1) ^ ": " ^ desc);
    match synthesize libs spec with 
    | Some pgm -> 
      print_endline (string_of_pgm pgm); 
      print_endline ("Verification result: " ^ string_of_bool (verify pgm spec));
      print_endline ("-----\n")
    | None -> 
      print_endline "failed to synthesize"
  ) (enumerate [p1; p2; p3; p4; p5;])

let _ = test ()