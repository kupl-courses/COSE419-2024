module Setting = struct
  let inputFile : string Stdlib.ref
  = Stdlib.ref ""

  let printArg : bool Stdlib.ref
  = Stdlib.ref false

  let printCfg : bool Stdlib.ref
  = Stdlib.ref false

  let speclist : (Arg.key * Arg.spec * Arg.doc) list
  = [
    ("--input",         (Arg.String (fun s -> inputFile := s)), 
                        "File path for input simple c program");
    ("--print-arg",     (Arg.Set printArg),
                        "Print out arguments of this process");
    ("--print-cfg",     (Arg.Set printCfg),
                        "Print out CFG of input program in dot");]
    
  let anon_fun : string -> unit
  = fun s -> begin
    (* anon_fun function start *)
    match s with
    | _     -> invalid_arg "invalid option"
    (* anon_fun function end *)
  end
  
  let usage_msg : string
  = "verifier [--input] file [options]"
end

type t = {
  inputFile: string;
  printArg: bool;
  printCfg: bool;
}

let create : unit -> unit
= let open Setting in
  fun () -> begin
  (* create function start *)
  Arg.parse speclist anon_fun usage_msg;
  ()
  (* create function end *)
end

let read : unit -> t
= let open Setting in
  fun () -> begin
  (* read function start *)
  { inputFile=(!inputFile);
    printArg=(!printArg);
    printCfg=(!printCfg)}
  (* read function end *)
end

let to_string : unit -> string
= fun () -> begin
  let args = read () in
  "Current Arguments.\n" ^
  "\t- Input File: " ^ args.inputFile ^ "\n" ^
  "\t- Print Arguments Flag: " ^ (args.printArg |> string_of_bool) ^ "\n" ^
  "\t- Print CFG Flag: " ^ (args.printCfg |> string_of_bool)
end