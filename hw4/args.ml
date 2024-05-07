module Setting = struct
  (* STRING - input file path *)
  let inputFile : string Stdlib.ref
  = Stdlib.ref ""

  (* INT - Z3 timebudget in seconds *)
  let z3Timeout : int Stdlib.ref
  = Stdlib.ref 10

  (* FLAG - verification mode flag *)
  let partial : bool Stdlib.ref
  = Stdlib.ref true
  let termination : bool Stdlib.ref
  = Stdlib.ref false

  (* FLAG - print data *)
  let printArg : bool Stdlib.ref
  = Stdlib.ref false
  let printAdt : bool Stdlib.ref
  = Stdlib.ref false
  let printCfg : bool Stdlib.ref
  = Stdlib.ref false

  let speclist : (Arg.key * Arg.spec * Arg.doc) list
  = [
    ("--input",         (Arg.String (fun s -> inputFile := s)), 
                        "File path for input simple c program");
    ("--z3-timeout",    (Arg.Int (fun d -> z3Timeout := d)), 
                        "Timebudget for Z3 solver - default - 30s");
    ("--partial",       (Arg.Unit (fun () -> termination := false; partial := true)), 
                        "Flag for partial correctness verifier");
    ("--termination",   (Arg.Unit (fun () -> partial := false; termination := true)), 
                        "Flag for termination prover");
    ("--print-arg",     (Arg.Set printArg),
                        "Print out arguments of this process");
    ("--print-adt",     (Arg.Set printAdt),
                        "Print out ADT of input program");
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

  let validate_arg : unit -> unit
  = let file_exists : string -> bool
    = fun name -> begin
      (* file_exists function start *)
      try Unix.access name [Unix.F_OK]; true
      with
      | (Unix.Unix_error (Unix.ENOENT, _, _)) -> false
      | _ -> true
      (* file_exists function end *)
    end in
    fun () -> begin
    (* validate_arg function start *)
    if not (file_exists !inputFile) then  Stdlib.invalid_arg "invalid input" else
    if !partial && !termination then      Stdlib.invalid_arg "invalid flag for partial and total correctness verification" else
    if not (!partial || !termination) then      Stdlib.invalid_arg "invalid flag for partial and total correctness verification" else
    ()
    (* validate_arg function end *)
  end
end

type t = {
  inputFile: string;
  z3Timeout: int;
  partial: bool;
  termination: bool;
  printArg: bool;
  printAdt: bool;
  printCfg: bool;
}

let create : unit -> unit
= let open Setting in
  fun () -> begin
  (* create function start *)
  Arg.parse speclist anon_fun usage_msg;
  validate_arg ();
  ()
  (* create function end *)
end

let read : unit -> t
= let open Setting in
  fun () -> begin
  (* read function start *)
  { inputFile=(!inputFile);
    z3Timeout=(!z3Timeout);
    partial=(!partial);
    termination=(!termination);
    printArg=(!printArg);
    printAdt=(!printAdt); 
    printCfg=(!printCfg)}
  (* read function end *)
end

let to_string : unit -> string
= fun () -> begin
  let args = read () in
  "Current Arguments.\n" ^
  "\t- Input File: " ^ args.inputFile ^ "\n" ^
  "\t- Z3 Timebudget: " ^ (args.z3Timeout |> string_of_int) ^ "\n" ^
  "\t- Partial Flag: " ^ (args.partial |> string_of_bool) ^ "\n" ^
  "\t- Termination Flag: " ^ (args.termination |> string_of_bool) ^ "\n" ^
  "\t- Print Arguments Flag: " ^ (args.printArg |> string_of_bool) ^ "\n" ^
  "\t- Print ADT Flag: " ^ (args.printAdt |> string_of_bool) ^ "\n" ^
  "\t- Print CFG Flag: " ^ (args.printCfg |> string_of_bool)
end