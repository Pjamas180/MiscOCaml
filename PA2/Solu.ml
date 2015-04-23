(* My Solutions to Fall2013 Midterm 1 *)

let count l x =
    let count_helper a v = if v = x then a+1 else a in
    let base = 0 in
    List.fold_left count_helper base l;;

(*
let count l x =
    let count_helper acc listVal = if x = listVal then acc+1 in
    let base = 0 in
    List.fold_left count_helper base l;;
*)

let make_palyndrome l =
    let palyndrome_helper acc valElem = valElem::acc in
    let base = l in
    List.fold_left palyndrome_helper base l;;

let make_palyndrome2 l =
    let palyndrome_helper acc valElem = [valElem]@acc in
    let base = l in
    List.fold_left palyndrome_helper base l;;

let fold_2 f a l =
    (* Here we are taking advantage of tuples and doing whatever we want
     * with the function we are given
     *)
    let fnc (i,b) elmt = (i+1, f b elmt i) in
    let b = (0,a) in
    let (_,res) = List.fold_left fnc b l in res ;;

let fold_2m f a l =
    let fnc (i,b) elmt = ((i+1), f b elmt i) in
    let b = (0,a) in
    snd(List.fold_left fnc b l in res );;

let ith l i d =
    let fnc acc elmt pos = if pos = i then elmt else acc in
    fold_2 fnc d l ;;

type 'a fun_tree = 
    | Leaf of ('a -> 'a)
    | Node of ('a fun_tree) * ('a fun_tree);;

let rec apply_all t x =
    match t with
    Leaf f -> f x
    | Node(t1, t2) -> apply_all t2 (apply_all t1 x) ;;

let rec update l i n = 
    match l with
    | [] -> l 
    | h::t -> if i = 0 then n::t  else h::update t (i-1) n;;

let rec update2 l i n d =
    match l with
    | [] -> if i = 0 then n::l else d::update2 l (i-1) n d
    | h::t -> if i = 0 then n::t else h::update2 t (i-1) n d;;

let categorize f l = 
    let base = [[]] in
    let fold_fn acc elmt =
        let pos = f elmt in
        let sub_list = ith acc pos [] in
        update2 acc pos (sub_list@[elmt]) [] in
    List.fold_left fold_fn base l;;
