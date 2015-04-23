(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

(* expr type that holds all expressions of types x,y,sine and cosine etc
 * PedroExpr1 subtracts y from x and divides the result by z
 * PedroExpr2 is a tangent
 *)
type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | PedroExpr1 of expr * expr * expr
  | PedroExpr2 of expr

(* exprToString is a recursive function that converts an expression into
 * a readable string
 *)
let rec exprToString e = 
    match e with
    VarX -> "x"
    | VarY -> "y"
    | Sine a -> "sin(pi*" ^ exprToString a ^ ")"
    | Cosine b -> "cos(pi*" ^ exprToString b ^ ")"
    | Average (c, d) -> "((" ^ exprToString c ^ "+" ^ exprToString d 
        ^ ")/2)"
    | Times (f, g) -> exprToString f ^ "*" ^ exprToString g
    | Thresh (h, i, j, k) -> "(" ^ exprToString h ^ "<" ^ 
        exprToString i ^ "?" ^ exprToString j ^ ":" ^ exprToString k
        ^ ")"
    (* Adding 2 new cases *)
    | PedroExpr1 (c1,c2,c3) -> "(" ^ exprToString c1 ^ "*" ^ exprToString c2
      ^  ")" ^ "/" ^ exprToString c3 ^ ")"
    | PedroExpr2 (a1) -> "tan(pi*" ^ exprToString a1 ^ ")"
    ;;

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
(* The following functions were created as (x-y)/z and tangent.*)
let buildPedroExpr1(c1,c2,c3)      = PedroExpr1(c1,c2,c3)
let buildPedroExpr2(a1)            = PedroExpr2(a1)

let pi = 4.0 *. atan 1.0

(* eval is a recursive function that evaluates the expressions of type
 * expr
 *)
let rec eval (e,x,y) = 
    match e with
    VarX -> x
    | VarY -> y
    | Sine a -> sin (pi *. eval (a, x, y))
    | Cosine b -> cos (pi *. eval (b, x, y))
    | Average (c, d) -> ((eval (c, x, y))+.(eval (d, x, y)))/.2.0
    | Times (f, g) -> (eval (f, x, y)) *. (eval (g, x, y))
    | Thresh (h, i, j, k) -> if x < y then eval (j, x, y) 
                             else eval (k, x, y)
    | PedroExpr1(c1,c2,c3) -> ((eval(c1,x,y))*.((eval(c2,x,y))))
                              /.(eval(c3,y,x))
    | PedroExpr2 a1       -> tan (pi *. eval(a1, x, y))
    ;;

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here *****************)

let expr1 =
    (Thresh(VarX,VarY,VarX,(Times(Sine(VarX),Cosine(Average(VarX,VarY))))));;

let string1 = exprToString expr1;;

Printf.printf "%s\n" string1;;

Printf.printf "%d\n" let value1 = eval (sampleExpr, 0.5, 0.2);;

Printf.printf "%d\n" let value2 = eval (sampleExpr2, 0.5, 0.2);;
