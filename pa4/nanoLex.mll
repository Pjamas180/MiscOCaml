{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

let digit = ['0' - '9']
let letter = ['A'-'Z' 'a'-'z']
rule token = parse
    eof                     { EOF }
  | "true"                  { TRUE } 
  | "false"                 { FALSE }
  | ['0'-'9']* as inte      { Num(int_of_string inte) }
  | "let rec"               { REC }
  | "let"                   { LET }
  | "rec"                   { REC }
  | "="                     { EQ }
  | "in"                    { IN }
  | "fun"                   { FUN }
  | "->"                    { ARROW }
  | "if"                    { IF }
  | "then"                  { THEN }
  | "else"                  { ELSE }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
                            { Id(str) }
  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "*"                     { MUL }
  | "/"                     { DIV }
  | "<"                     { LT }
  | "<="                    { LE }
  | "!="                    { NE }
  | "&&"                    { AND }
  | "||"                    { OR }
  | ")"                     { RPAREN }
  | "("                     { LPAREN }
  | "["                     { LBRAC }
  | "]"                     { RBRAC }
  | ";"                     { SEMI }
  | "::"                    { COLONCOLON }
  | ['\n' '\t' ' ' '\r']    { token lexbuf } (* skip spaces *)
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }
