%{
(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token  TRUE FALSE
%token <string> Id
%token LET REC EQ IN FUN ARROW IF THEN ELSE
%token EOF
%token PLUS MINUS MUL DIV LT LE NE AND OR LPAREN RPAREN
%token COLONCOLON LBRAC RBRAC
%token SEMI
%token APP

%start exp 
%type <Nano.expr> exp
%type <Nano.binop> bin
%type <Nano.expr> lists
%type <Nano.expr> lists2

%nonassoc LET IF 
%left OR
%left AND
%left EQ NE LT LE
%right COLONCOLON
%left PLUS MINUS
%left MUL DIV
%left FUN
 
%%

exp: Num                        { Const $1 }
    | TRUE                      { True }
    | FALSE                     { False }
    | Id                        { Var $1 }
    | LET Id EQ exp IN exp      { Let ($2,$4,$6) }
    | LET REC Id EQ exp IN exp  { Letrec ($3, $5, $7) }
    | FUN Id ARROW exp          { Fun ($2,$4) }
    | ARROW                     { NilExpr }
    | IF exp THEN exp ELSE exp  { If ($2,$4,$6) }
    | exp exp %prec APP         { App($1,$2) }
    | LPAREN exp RPAREN         { $2 }
    | exp MUL exp               { Bin ($1,Mul,$3) }
    | exp DIV exp               { Bin ($1,Div,$3) }
    | exp PLUS exp              { Bin ($1,Plus,$3) }
    | exp MINUS exp             { Bin ($1,Minus,$3) }
    | exp LT exp                { Bin ($1,Lt,$3) }
    | exp LE exp                { Bin ($1,Le,$3) }
    | exp NE exp                { Bin ($1,Ne,$3) }
    | exp AND exp               { Bin ($1,And,$3) }
    | exp OR exp                { Bin ($1,Or,$3) }
    | exp EQ exp                { Bin ($1, Eq, $3) }
    | lists                     { $1 }


lists:                 
    | LBRAC RBRAC               { NilExpr }
    | exp COLONCOLON exp        { Bin( $1, Cons, $3) }
    | LBRAC lists2 RBRAC        { $2 }

lists2: 
    | exp SEMI lists2           { Bin( $1, Cons, $3) }
    | exp                       { Bin( $1, Cons, NilExpr) }

bin:  PLUS                      { Plus }
    | MINUS                     { Minus }
    | MUL                       { Mul }
    | DIV                       { Div }
    | LT                        { Lt }
    | LE                        { Le }
    | NE                        { Ne }
    | AND                       { And }   
    | OR                        { Or }
    | COLONCOLON                { Cons }

