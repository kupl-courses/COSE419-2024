open Cfg 

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
  let (vars, cmd) = read args.inputFile in
  let cfg = cmd2cfg cmd in 
  let _ = if args.printCfg then (Cfg.dot cfg; exit 1) in 
  let n = Analyzer.analyze (vars, cmd) in 
    List.iter (fun x -> prerr_endline x) vars; 
    prerr_endline ("# of proved assertions: " ^ string_of_int n)
  
let _ = main()
