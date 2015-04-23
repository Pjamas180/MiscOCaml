(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

(* val assoc : 'a *'b * ('b * 'a) list -> 'a = <fun>
 * assoc is a recursive function which finds the first string in the
 * list that matches with the parameter k and returns the value 
 * associated with the string in the pair. If the value is not found,
 * d is returned. I used the cons operator to break up the list and
 * compare values.
 *)
let rec assoc (d,k,l) =
    match l with
    [] -> d
    (* Using cons operator to break up the string *)
    | h::t ->
            match h with
            | (x,y) -> if x = k then y else assoc (d,k,t);;

(* val removeDuplicates : 'a list -> 'a list = <fun>
 * removeDuplicates is a function which contains a recursive helper
 * function that eliminates any duplicates in a list. List.rev and 
 * List.mem were utilized.
 *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        (* Saving seen' and rest' in order to keep track of values *)      
        let seen' = if List.mem h seen then seen else h::seen in
        let rest' = t in
        helper (seen',rest'); 
  in
  (* Reversing list because current seen list is backwards *)
  List.rev (helper ([],l));;


(* val wwhile : ('a -> 'a * bool) * 'a -> 'a = <fun>
 * wwhile is a recursive function that modifies a value according
 * to a boolean statement from a function that was passed in.
 *)
let rec wwhile (f,b) = 
    let (x, y) = f b in 
    (* Checking value of y if true or false *)
    match y with
    (* If y evaluates to true, then recursively call wwhile *)
    true -> wwhile (f, x)
    (* If y is false, then return the value where it was false *)
    | false -> x;;

(* val fixpoint : ('a -> 'a) * 'a -> 'a = <fun>
 * fixpoint is a function that contains the function wwhile which
 * changes a number until the input number, b, is equal to the 
 * output of f(b).
 *)
let fixpoint (f,b) = wwhile ((let f' x = let xx = f x in (xx, xx != x) in
    f'),b);;



(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
