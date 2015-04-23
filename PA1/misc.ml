(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
   ***** PUT DOCUMENTATION COMMENTS HERE *****
   sumList is a recursive function which evaluates the sum of a list of
   integers. Pattern matching was utilized in order to parse the head and
   tail of the list. Once they are parsed, I added the head (which is an 
   integer) to a recursively called sumList on t which eventually adds
   the rest of the integers.
*) 

let rec sumList l = 
    match l with
    | [] -> 0
    | h::t -> h + (sumList t);;



(* digitsOfInt : int -> int list 
   ***** PUT DOCUMENTATION COMMENTS HERE *****
   digitsOfInt is a recursive function which lists out the digits of a 
   number. I recursively called digitsOfInt on the parameter and divided
   it by 10 in order to keep getting the next digit. I then appended it
   onto the list where the first digit was already added to the list. I
   obtained the first number by modding it by 10.
 *)

let rec digitsOfInt n =
    match (abs n) with
    | 0 -> []
    | _ -> (digitsOfInt ((abs n) / 10))@[(abs n) mod 10];;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
 * additivePersistence is a recursive function that computes the number of
 * additions required to obtain a single digit from a number n.
 *)
let rec additivePersistence n = 
    match n with
    n when n < 10 -> 0
    | _ -> 1 + additivePersistence ( sumList ( digits n ) ) ;;

(* digitalRoot : int -> int
 * digitalRoot is a recursive function that computes the the digit finally
 * obtained after additivePersistence
 *)
let rec digitalRoot n = 
    match n with
    n when n < 10 -> n 
    | _ -> digitalRoot ( sumList ( digits n ) ) ;;

(* listReverse : int list -> int list
 * listReverse is a recursive function which reverses a list by appending the
 * head to the end of the tail, which we call listReverse on.
 *)
let rec listReverse l = 
    match l with
    [] -> []
    | h::t -> (listReverse t)@[h];;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> string
 * palindrome is a function that checks to see if a word is a palindrome. In 
 * order to do this, I utilized the explode function and compared the result
 * of the explode function on w to the reverse of explode w.
 *)
let palindrome w = 
    match w with
    w when w = "" -> true
    | _ -> (explode w) = listReverse(explode w);;

(************** Add Testing Code Here ***************)
