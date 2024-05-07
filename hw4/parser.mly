%{
  open Utils
%}

(* Token - Value *)
%token <string> IDENT
%token <int> NUMBER
%token <bool> BOOLEAN

(* Token - System and Keyboard Symbol *)
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token ASSERT HASH SEMICOLON COLON COMMA DOT MID
%token EOF

(* Token - Other Symbol *)
%token FNOT IMPLY IFF AND OR
%token LE GE LT GT EQ NEQ ENOT
%token PLUS MINUS STAR SLASH
%token ASSIGN

(* Token - Keyword *)
%token PRE POST
%token FORALL EXISTS
%token INT BOOL
%token IF ELSE FOR WHILE DO RETURN BREAK SKIP
%token SORTED PARTITIONED
%token CALL

(* Priority *)
%right  DOT
%right  FNOT
%right  IMPLY IFF
%right  ENOT
%left   OR AND
%left   EQ NEQ LT GT LE GE
%left   PLUS MINUS
%left   STAR SLASH

(* Type of Pattern *)
%type   <Syntax.typ>          typ ttyp
%type   <Syntax.exp>          exp
%type   <Syntax.lv>           lv
%type   <Syntax.fmla>         fmla pdc
%type   <Syntax.inv>          inv
%type   <Syntax.rank option>  rank
%type   <Syntax.stmt>         stmt (*astmt*) assign
%type   <Syntax.decl>         decl arg_decl
%type   <Syntax.decl list>    arg
%type   <Syntax.pgm>          pgm

(* Entrancy of Pattern *)
%start  <Syntax.pgm>         start

%%

start:
  | p=pgm EOF                             { p }

typ:
  | t1=ttyp                               { t1 }
  | t1=ttyp LBRACK RBRACK                 { Syntax.T_arr t1 }

ttyp:
  | INT                                   { Syntax.T_int }
  | BOOL                                  { Syntax.T_bool }

exp:
  | LPAREN e1=exp RPAREN                  { e1 }
  | n1=NUMBER                             { Syntax.E_int n1 }
  | b1=BOOLEAN                            { Syntax.E_bool b1 }
  | l1=lv                                 { Syntax.E_lv l1 }
  | e1=exp PLUS e2=exp                    { Syntax.E_add (e1, e2) }
  | e1=exp MINUS e2=exp                   { Syntax.E_sub (e1, e2) }
  | e1=exp STAR e2=exp                    { Syntax.E_mul (e1, e2) }
  | e1=exp SLASH e2=exp                   { Syntax.E_div (e1, e2) }
  | MINUS e1=exp                          { Syntax.E_neg e1 }
  | MID id1=IDENT MID                     { Syntax.E_len id1 }
  | ENOT e1=exp                           { Syntax.E_not e1 }
  | e1=exp EQ e2=exp                      { Syntax.E_eq (e1, e2) }
  | e1=exp NEQ e2=exp                     { Syntax.E_neq (e1, e2) }
  | e1=exp LT e2=exp                      { Syntax.E_lt (e1, e2) }
  | e1=exp GT e2=exp                      { Syntax.E_gt (e1, e2) }
  | e1=exp LE e2=exp                      { Syntax.E_le (e1, e2) }
  | e1=exp GE e2=exp                      { Syntax.E_ge (e1, e2) }

lv:
  | id1=IDENT                             { Syntax.V_var id1 }
  | id1=IDENT LBRACK e2=exp RBRACK         { Syntax.V_arr (id1, e2) }

fmla:
  | LPAREN f1=fmla RPAREN                 { f1 }
  | e1=exp                                { Syntax.F_exp e1 }
  | FNOT f1=fmla                          { Syntax.F_not f1 }
  | f1=fmla AND f2=fmla                   { Syntax.F_and [f1; f2] (* |> Fmla.flatten *) }
  | f1=fmla OR f2=fmla                    { Syntax.F_or [f1; f2]  (* |> Fmla.flatten *) }
  | f1=fmla IMPLY f2=fmla                 { Syntax.F_imply (f1, f2) }
  | f1=fmla IFF f2=fmla                   { Syntax.F_iff (f1, f2) }
  | FORALL x1=IDENT DOT f2=fmla              { Syntax.F_forall (x1, None, f2) }
  | EXISTS x1=IDENT DOT f2=fmla              { Syntax.F_exists (x1, None, f2) }
  | pdc1=pdc                              { pdc1 }

pdc:
  | SORTED LPAREN x1=IDENT COMMA e2=exp COMMA e3=exp RPAREN
                                          { Syntax.F_sorted (x1, e2, e3) }
  | PARTITIONED LPAREN x1=IDENT COMMA e2=exp COMMA e3=exp COMMA e4=exp COMMA e5=exp RPAREN
                                          { Syntax.F_partitioned (x1, e2, e3, e4, e5) }

inv:
  | ASSERT id1=IDENT COLON f2=fmla        { (id1, f2) }
  | ASSERT COLON f1=fmla                  { (" ", f1) }

rank:
  | HASH LPAREN el1=separated_nonempty_list (COMMA, exp) RPAREN  { Some el1 }
  |                                       { None }

fbody:
  | LBRACE RBRACE { ([], Syntax.S_skip) }
  | LBRACE dl1=nonempty_list (decl) sl1=nonempty_list (stmt) RBRACE { (dl1, Syntax.S_seq sl1) }
  | LBRACE sl1=nonempty_list (stmt) RBRACE { ([], Syntax.S_seq sl1) }
  | LBRACE dl1=nonempty_list (decl) RBRACE { (dl1, Syntax.S_seq []) }

stmt:
  | LBRACE RBRACE                         { Syntax.S_skip }
  | LBRACE sl1=nonempty_list (stmt) RBRACE { Syntax.S_seq sl1 }
  | SKIP SEMICOLON                        { Syntax.S_skip }
  | v1=lv ASSIGN e2=exp SEMICOLON         { Syntax.S_assign (v1, e2) }
  | decl SEMICOLON                        { Syntax.S_skip }
  | IF LPAREN e1=exp RPAREN s2=stmt       { Syntax.S_if (e1, s2, Syntax.S_skip) }
  | IF LPAREN e1=exp RPAREN s2=stmt ELSE s3=stmt
                                          { Syntax.S_if (e1, s2, s3) }
  | WHILE i1=inv r2=rank LPAREN e3=exp RPAREN s4=stmt 
  | WHILE r2=rank i1=inv LPAREN e3=exp RPAREN s4=stmt 
                                          { Syntax.S_while (i1, r2, e3, s4) }
  | DO s1=stmt WHILE i2=inv r3=rank LPAREN e4=exp RPAREN
  | DO s1=stmt WHILE r3=rank i2=inv LPAREN e4=exp RPAREN
  | DO s1=stmt WHILE LPAREN e4=exp RPAREN i2=inv r3=rank
  | DO s1=stmt WHILE LPAREN e4=exp RPAREN r3=rank i2=inv
                                          { Syntax.S_seq [s1; (Syntax.S_while (i2, r3, e4, s1))] }
  | FOR i1=inv r2=rank LPAREN a3=assign SEMICOLON e4=exp SEMICOLON a5=assign RPAREN s6=stmt
  | FOR r2=rank i1=inv LPAREN a3=assign SEMICOLON e4=exp SEMICOLON a5=assign RPAREN s6=stmt 
                                          { Syntax.S_seq [a3; (Syntax.S_while (i1, r2, e4, (Syntax.S_seq [s6; a5])))] }                                        
  | CALL ret=IDENT ASSIGN id1=IDENT LPAREN el2=separated_nonempty_list (COMMA, exp) RPAREN SEMICOLON
                                          { Syntax.S_call (ret, id1, el2) }
  | RETURN e1=exp SEMICOLON               { Syntax.S_return e1 }
  | BREAK SEMICOLON                       { Syntax.S_break }

assign:
  | v1=lv ASSIGN e2=exp                   { Syntax.S_assign (v1, e2) }

decl:
  | t1=typ id2=IDENT SEMICOLON                     { (t1, id2) }

arg:
  | al1=separated_nonempty_list (COMMA, arg_decl)
                                          { al1 }

arg_decl:
  | t1=typ id2=IDENT                      { (t1, id2) }

pgm:
  | ASSERT PRE COLON? pre=fmla ASSERT POST COLON? post=fmla r1=rank t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=fbody
  | ASSERT PRE COLON? pre=fmla r1=rank ASSERT POST COLON? post=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=fbody
  | ASSERT POST COLON? post=fmla ASSERT PRE COLON? pre=fmla r1=rank t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=fbody
  | ASSERT POST COLON? post=fmla r1=rank ASSERT PRE COLON? pre=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=fbody
  | r1=rank ASSERT PRE COLON? pre=fmla ASSERT POST COLON? post=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=fbody
  | r1=rank ASSERT POST COLON? post=fmla ASSERT PRE COLON? pre=fmla t2=typ id3=IDENT LPAREN arg4=arg RPAREN s5=fbody
                                          { Syntax.create_program (snd s5)
                                                ("pre", pre)
                                                ("post", post) 
                                                r1 
                                                t2 
                                                id3 
                                                arg4
                                                (fst s5) }