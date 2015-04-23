(* Interpreter file for NanoML grammar.
 * Created by Pedro Villaroman
 * CSE 130 PA4
 * Instructor: Sorin Lerner
 *)

exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | NilExpr -> "nil"
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(* lookup: string * env -> value. Finds the most recent binding for a string
 * x. Returns the most recent value associated with the string. Useful for
 * eval.
 *)
let lookup (x,evn) = 
    let a = listAssoc(x,evn) in
    match a with
        None -> Nil
        | Some v -> v 

(* eval: env * expr -> value. Evaluate expr according to the current env
 * of variables and functions by pattern matching. Simple ML language.
 *)
let rec eval (evn,e) = 
    match e with
    Const i -> Int i
    | True -> Bool true
    | False -> Bool false
    | NilExpr -> Nil
    | Var str -> if str = "hd" || str = "tl" then 
        if str = "hd" then Closure([],None,"hd",e)
                    else Closure([],None,"tl",e)
            else (match lookup(str,evn) with 
         Nil -> failwith ("Variable not bound: "^str) 
        | Int i -> Int i
        | Bool j -> Bool j
        | Closure(a,b,c,d) -> Closure(a,b,c,d)
    )
    | Bin (expr1, binop1, expr2) -> let v1 = eval(evn,expr1) in
                                    let v2 = eval(evn,expr2) in
                (match (v1,binop1,v2) with
                (Int i1, Plus, Int i2) -> Int (i1+i2)
                | (Int i1, Minus, Int i2) -> Int (i1-i2)
                | (Int i1, Mul, Int i2) -> Int (i1*i2)
                | (Int i1, Div, Int i2) -> Int (i1/i2)
                | (Int i1, Eq, Int i2) -> if i1 = i2 then Bool true else
                        Bool false
                | (Int i1, Ne, Int i2) -> if i1 = i2 then Bool false else
                        Bool true
                | (Bool i1, Eq, Bool i2) -> if i1=i2 then Bool true else
                        Bool false
                | (Bool i1, Ne, Bool i2) -> if i1 = i2 then Bool false else
                        Bool true
                | (Int i1, Lt, Int i2) -> if (<) i1 i2 then Bool true else
                        Bool false
                | (Int i1, Le, Int i2) -> if (<=) i1 i2 then Bool true else
                        Bool false
                | (Bool i1, And, Bool i2) -> if i1 && i2 then Bool true else
                        Bool false
                | (Bool i1, Or, Bool i2) -> if i1 || i2 then Bool true else
                        Bool false
                | (Int i1, Cons, v2) -> Pair(Int i1,v2)
                | _ -> failwith "Bad Operand")
    | If (expr1, expr2, expr3) -> let v1 = eval(evn,expr1) in
            (match v1 with
            Bool true -> eval(evn,expr2)
            | Bool false -> eval(evn,expr3)
            | _ -> failwith "Expression does not evaluate to a bool")
    | Let (str, expr1, expr2) -> let v1 = eval(evn,expr1) in
        let evn2 = (str,v1)::evn in
        eval(evn2,expr2)
    (* Letrec will modify the environment so it knows about a recursivr
     * function
     *)
    | Letrec (str, expr1, expr2) ->
        let evn1 = (str,Bool true)::evn in
        let v1 = eval(evn1,expr1) in
        let evn2 = (str,v1)::evn1 in
        eval(evn2,expr2)
    (* App will evaluate any give closures. If a "hd" or "tl" is found in
     * the closure, we will evaluate accordingly by returning a specific
     * pair
     *)
    | App (expr1,expr2) -> let v1 = eval(evn,expr1) in
                           let v2 = eval(evn,expr2) in
            let Closure(a,b,c,d) = v1 in
                if c = "hd" || c = "tl" then
                    let Pair(a,b) = eval(evn,expr2) in
                    if c = "hd" then a else b
                    else
                if b = None then
                    let evn2 = (c,v2)::a in eval(evn2,d)
                else
                    let evn2 = (c,v2)::evn in eval(evn2,d)
    | Fun (str,expr1) -> match evn with
        [] -> Closure(evn,None,str,expr1)
        | h::t -> (match h with 
            (str1, Bool true) -> Closure(t,Some str1,str,expr1)
            | (str1,_) -> Closure(evn,None,str,expr1))

(**********************     Testing Code  ******************************)
