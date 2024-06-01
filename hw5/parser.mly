%{
%}

(* Token - Value *)
%token <string> IDENT
%token <int> NUMBER
%token TRUE FALSE

(* Token - System and Keyboard Symbol *)
%token LPAREN RPAREN LBRACE RBRACE 
%token ASSERT SEMICOLON COMMA
%token EOF

(* Token - Other Symbol *)
%token AND
%token LE EQ ENOT
%token PLUS MINUS STAR
%token ASSIGN

(* Token - Keyword *)
%token VAR 
%token IF ELSE WHILE FOR

(* Priority *)
%right  ENOT
%left   AND
%left   EQ LE 
%left   PLUS MINUS
%left   STAR 

(* Type of Pattern *)
%type   <Syntax.aexp>         aexp
%type   <Syntax.bexp>         bexp
%type   <Syntax.cmd>          cmd

(* Entrancy of Pattern *)
%start  <Syntax.pgm>         start

%%

start:
  | p=pgm EOF                             { p }

aexp:
  | LPAREN e1=aexp RPAREN                  { e1 }
  | n1=NUMBER                             { Syntax.Const n1 }
  | id1=IDENT                             { Syntax.Var id1 }
  | e1=aexp PLUS e2=aexp                    { Syntax.Add (e1, e2) }
  | e1=aexp MINUS e2=aexp                   { Syntax.Sub (e1, e2) }
  | e1=aexp STAR e2=aexp                    { Syntax.Mult (e1, e2) }

bexp:
  | LPAREN e1=bexp RPAREN                  { e1 }
  | TRUE                                  { Syntax.True }
  | FALSE                                 { Syntax.False }
  | e1=aexp EQ e2=aexp                      { Syntax.Equal (e1, e2) }
  | e1=aexp LE e2=aexp                      { Syntax.Le (e1, e2) }
  | ENOT e1=bexp                           { Syntax.Not e1 }
  | e1=bexp AND e2=bexp                     { Syntax.And (e1, e2) }

cmd:
  | LBRACE RBRACE                           { Syntax.Skip }
  | ASSERT e1=bexp SEMICOLON                { Syntax.Assert e1 }
  | LBRACE sl1=nonempty_list (cmd) RBRACE   { Syntax.Seq sl1 }
  | id1=IDENT ASSIGN e2=aexp SEMICOLON      { Syntax.Assign (id1, e2) }
  | IF LPAREN e1=bexp RPAREN s2=cmd         { Syntax.If (e1, s2, Syntax.Skip) }
  | IF LPAREN e1=bexp RPAREN s2=cmd ELSE s3=cmd { Syntax.If (e1, s2, s3) }
  | WHILE LPAREN e1=bexp RPAREN s2=cmd      { Syntax.While (e1, s2) }
  | FOR LPAREN a3=assign SEMICOLON e4=bexp SEMICOLON a5=assign RPAREN s6=cmd
                                          { Syntax.Seq [a3; (Syntax.While (e4, (Syntax.Seq [s6; a5])))] }                                        

assign:
  | id1=IDENT ASSIGN e2=aexp                   { Syntax.Assign (id1, e2) }

decls:
  | VAR ids1=separated_nonempty_list (COMMA, IDENT) SEMICOLON  { ids1 }                    

pgm:
  | d1=decls s2=cmd { (d1, s2) }