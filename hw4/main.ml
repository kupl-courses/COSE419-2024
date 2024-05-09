exception Error of string

let position : Lexing.lexbuf -> string
= fun lexbuf -> begin
  let pos = lexbuf.lex_curr_p in
  pos.pos_fname ^ ":" ^ 
  (pos.pos_lnum |> string_of_int) ^ ":" ^ 
  ((pos.pos_cnum - pos.pos_bol + 1) |> string_of_int)
end

let read_args : unit -> Args.t
=fun () -> 
  let _ = Args.create() in 
  let args = Args.read () in
    if args.printArg then print_endline (Args.to_string ()) else ();
    args 

let read : string -> Syntax.pgm 
=fun filename -> 
  let in_c = Stdlib.open_in filename in
  let lexbuf = Lexing.from_channel in_c in
  try
    let res = Parser.start Lexer.next_token lexbuf in
      close_in in_c; 
      res
  with
  | Lexer.LexingError msg -> (
    close_in in_c;
    Error ("read: " ^ msg ^ "[" ^ (position lexbuf) ^ "]") |> Stdlib.raise)
  | Parser.Error -> (
    close_in in_c;
    Error ("read: syntax error [" ^ (position lexbuf) ^ "]") |> Stdlib.raise)

let main : unit -> unit
=fun () -> 
  let args = read_args () in 
  let pgm = read args.inputFile in
  let cfg = Graph.pgm2cfg pgm in 
  let _ = if args.printAdt 
    then prerr_endline ("Input Program.\n" ^ (pgm |> Syntax.string_of_pgm ~indent:1)) else () in 
  let _ = if args.printCfg then (Graph.Cfg.dot cfg; exit 1) in 
  let res = 
    if not args.termination then begin 
        prerr_endline "Verifying partial correctnesss";
        Verifier.verify_partial_correctness pgm
      end 
    else begin 
        prerr_endline "Verifying termination";
        Verifier.verify_termination pgm 
      end in 
    if res then print_endline "Verification succeeded"
    else print_endline "Verification failed"

let _ = main()
