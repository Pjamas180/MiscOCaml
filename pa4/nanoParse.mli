type token =
  | Num of (int)
  | TRUE
  | FALSE
  | Id of (string)
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | EOF
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR
  | LPAREN
  | RPAREN
  | COLONCOLON
  | LBRAC
  | RBRAC
  | SEMI
  | APP

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
