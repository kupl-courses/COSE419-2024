{
  open Parser
    
  exception LexingError of string

  let comment_depth = Stdlib.ref 0

  let kwd_map : (string, Parser.token) Core.Map.Poly.t
  = let module PMap = Core.Map.Poly in
    [
      ("true",        BOOLEAN true);
      ("false",       BOOLEAN false);
      ("pre",         PRE);
      ("post",        POST);
      ("forall",      FORALL);
      ("exists",      EXISTS);
      ("int",         INT);
      ("bool",        BOOL);
      ("if",          IF);
      ("else",        ELSE);
      ("for",         FOR);
      ("while",       WHILE);
      ("do",          DO);
      ("return",      RETURN);
      ("break",       BREAK);
      ("skip",        SKIP);
      ("sorted",      SORTED);
      ("partitioned", PARTITIONED);
      ("call",        CALL); 
      ]
    |> PMap.of_alist
    |> (function |`Ok m -> m |`Duplicate_key _ -> LexingError (": wrong keyword table settings") |> Stdlib.raise)
  
  let id_or_kwd : string -> Parser.token
  = let open Core in
    fun s -> begin
    (* id_or_kwd function start *)
    let token = Map.find kwd_map s in
    match token with
    | Some t  -> t
    | None    -> IDENT s
    (* id_or_kwd function end *)
  end
}

let letter    = ['a'-'z' 'A'-'Z']
let digit     = ['0'-'9']
let number    = digit+
let space     = ' ' | '\t' | '\r'
let blank     = space+
let new_line  = '\n' | "\r\n"
let ident     = letter (letter | digit | '_')*

let comment_line_header   = "//"
let comment_block_header  = "/*"
let comment_block_footer  = "*/"

rule next_token = parse
  | comment_line_header   { comment_line lexbuf }
  | comment_block_header  { comment_depth := 1; comment_block lexbuf }
  | blank                 { next_token lexbuf }
  | new_line              { Lexing.new_line lexbuf; next_token lexbuf }
  | ident as s            { id_or_kwd s }
  | number as n           { NUMBER (int_of_string n) }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | '['                   { LBRACK }
  | ']'                   { RBRACK }
  | '@'                   { ASSERT }
  | '#'                   { HASH }
  | ';'                   { SEMICOLON }
  | ':'                   { COLON }
  | ','                   { COMMA }
  | '.'                   { DOT }
  | '|'                   { MID }
  | '~'                   { FNOT }
  | "->"                  { IMPLY }
  | "<->"                 { IFF }
  | "&&"                  { AND }
  | "||"                  { OR }
  | "<="                  { LE }
  | ">="                  { GE }
  | '<'                   { LT }
  | '>'                   { GT }
  | "=="                  { EQ }
  | "!="                  { NEQ }
  | '!'                   { ENOT }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { STAR }
  | '/'                   { SLASH }
  | ":="                  { ASSIGN }
  | eof                   { EOF }
  | _ as c                { LexingError (": illegal character \'" ^ (c |> String.make 1) ^ "\'") |> Stdlib.raise }

and comment_block = parse
  | comment_block_header  { Stdlib.incr comment_depth; comment_block lexbuf }
  | comment_block_footer  { Stdlib.decr comment_depth; if !comment_depth > 0 then comment_block lexbuf else next_token lexbuf }
  | eof                   { LexingError (": illegal comment") |> Stdlib.raise }
  | _                     { comment_block lexbuf }

and comment_line = parse
  | new_line              { Lexing.new_line lexbuf; next_token lexbuf }
  | eof                   { EOF }
  | _                     { comment_line lexbuf }
