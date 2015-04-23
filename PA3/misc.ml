(* CSSE 130: Programming Assignment 3
 * misc.ml
 *  *)

 (* For this assignment, you may use the following library functions:
   *
   *    List.map
   *    List.fold_left
   *    List.fold_right
   *    List.split
   *    List.combine
   *    List.length
   *    List.append
   *    List.rev
   *
   *    See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   *    documentation.
   *)



 (* Do not change the skeleton code! The point of this assignment is to figure
  * out how the functions can be written this way (using fold). You may only
  * replace the   failwith "to be implemented"   part. *)



 (*****************************************************************)
 (******************* 1. Warm Up   ********************************)
 (*****************************************************************)

 (* sqsum : int list -> int that takes a list of integers and returns the sum
  * of the squares of the integers using List.fold_left
  *)
 let sqsum xs = 
   let f a x = (x*x) + a in
   let base = 0 in
    List.fold_left f base xs

 (* pipe : ('a -> 'a) list -> ('a -> 'a). The function pipe takes a list of 
  * functions [f1;...;fn] and returns a function f such that for any x, the
  * application f x returns the result fn(...(f2(f1 x)))
  *)
 let pipe fs = 
   (* Need to take in a function that returns a function of the same type *)
   let f a x =  fun y -> x(a(y)) in
   let base = fun x -> x in
     List.fold_left f base fs

 (* sepConcat : string -> string list -> string. The function sepConcat is a 
  * curried function which takes as input a string sep to be used as a 
  * separator, and a list of strings [s1;...;sn].
  *)
 let rec sepConcat sep sl = 
   match sl with 
   | [] -> ""
   | h :: t -> 
       let f a x = a ^ sep ^ x in
       let base = h in
       let l =  t in
         List.fold_left f base l

 (* stringOfList : ('a -> string) -> 'a list -> string. The first input is a
  * function f: 'a -> string which will be called by stringOfList to convert
  * each element of the list to a string.
  *)
 let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]"

 (*****************************************************************)
 (******************* 2. Big Numbers ******************************)
 (*****************************************************************)

 (* clone : 'a -> int -> 'a list. Tail-recursive function which takes as input
  * x and then takes as input an integer n. The result is a list of length n,
  * where each element is x.
  *)
 let rec clone x n = 
   if n < 1 then [] else
     match n with
     _ -> let f x a = x :: a in
          let base = [] in
          let rec cloneHelper f curr nTimes x =
            match nTimes with
            0 -> curr
            | _ -> cloneHelper f (f x curr) (nTimes-1) x in
              cloneHelper f base n x

 (* padZero : int list -> int list -> int list * int list. Curried function 
  * which Takes two lists and adds zeros in front to make the lists equal.
  *)
 let rec padZero l1 l2 = 
   if (List.length l1) > (List.length l2) then 
     (l1, (clone 0 (List.length l1 - List.length l2))@l2) else
     ((clone 0 (List.length l2 - List.length l1))@l1,l2)

 (* removeZero : int list -> int list. A function that takes a list and 
  * removes a prefix of trailing zeros. 
  *)
 let rec removeZero l = 
   match l with
   [] -> []
   | h::t ->
       match h with
       0 -> removeZero t
      | _ -> l

 (* bigAdd : int list -> int list -> int list. A curried function that takes
  * the list representations of two numbers, adds them like a normal number
  * and returns the list representation of the addition.
  *)
 let bigAdd l1 l2 = 
   let add (l1, l2) =
     (* ex. f (1,9) (0,[1;2;3;4]);; *)
     let f a x = let (a1,a2) = a in (* a is base in this case *)
                 let (x1,x2) = x in (* x is the tuple passed in from args *)
                 match a2 with
                  [] -> let x3 = (x1 + x2) in
                  if x3 > 9 then (1,[1]@([x3 mod 10]@a2)) else
                   (2,[0]@([x3 mod 10]@a2))
                 | h::t -> let x3 = (x1 + x2 + h) in
                 if x3 > 9 then (3,[1]@([x3 mod 10]@t)) else
                   (4,[0]@([x3 mod 10]@t))
                 in
     let base  = (0, []) in
     let args  =  (List.rev (List.combine l1 l2)) in
     let (_, res) = List.fold_left f base args in res
   in 
     removeZero (add (padZero l1 l2))

 (* mulByDigit : int -> int list -> int list. A recursive function that 
  * multiplies a list representation of a number by an input value less than
  * 10.
  *)
 let rec mulByDigit i l = match i with
    0 -> l
    | 1 -> l
    | _ -> bigAdd l (mulByDigit (i-1) l)

 (* bigMul : int list -> int list -> int list. A curried function that takes in 
  * the list representations of two numbers, and multiplies them using bigAdd
  * and mulByDigit.
  *)
 let bigMul l1 l2 = 
   let f a x = let (a1,a2) = a in
               let (x1,x2) = x in
               (* a1 is a pair of lists *)
               let (y1,y2) = a1 in
               match a2 with
               (* for the first case *)
               [] -> (((0::y1),y2),(bigAdd ((mulByDigit x1 y2)@y1) a2))
               | h::t -> (((0::y1),y2),(bigAdd ((mulByDigit x1 y2)@y1) a2))
               in  
   let base = (([],l2),[]) in
   let args = (List.rev (List.combine l1 l2)) in
   let (_, res) = List.fold_left f base args in
               res
