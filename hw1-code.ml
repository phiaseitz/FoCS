(* FoCS Spring 2015

   Homework 1 code


   Name: Sophia Seitz

   Email: sophia.seitz@students.olin.edu

   Comments: 

 *)



(* 
 *  Question 1
 *)

let append (xs,ys) =  match xs,ys with 
  | [],[] -> []
  | a::b,[] -> xs
  | [],c::d -> ys
  | a::b,c::d -> xs @ ys;;
  (*  http://stackoverflow.com/questions/8286001/in-ocaml-what-is-the-canonical-way-of-matching-against-multiple-arguments-of-a
  *)
  
let rec flatten (xs) = match xs with 
  | [] -> [] 
  | h::t -> h @ flatten (t);;

let rec double (xs) = match xs with 
  | [] -> []
  | h::t -> (2*h)::double(t);;

let rec last (xs) = match xs with 
  | [] -> None
  | h::t -> match t with 
    | [] -> Some h
    | x::y -> last(x::y);;



(*
 *  Question 2 
 *)

let rec setIn (elt,set) = match set with 
  | [] -> false 
  | h::t -> (elt == h) || setIn(elt,t);;
  failwith "Not implemented"

let setSub (set1,set2) = 
  failwith "Not implemented"

let setEqual (set1,set2) = 
  failwith "Not implemented"

let setUnion (set1,set2) = 
  failwith "Not implemented"

let setInter (set1,set2) = 
  failwith "Not implemented"

let setDiff (set1,set2) = 
  failwith "Not implemented"

let setSize (set) =
  failwith "Not implemented"


(* 
 *  Question 3
 *)

type rat = {num: int; den: int}

let half = {num=1; den=2}
let third = {num=1; den=3}
let fourth = {num=1; den=4}

let floatR (r) = float(r.num) /. float(r.den)

let simplify (r) = 
  failwith "Not implemented"

let addR (r1,r2) = 
  failwith "Not implemented"

let multR (r1,r2) = 
  failwith "Not implemented"

type number = I of int
            | R of rat
            | F of float

let add (n1,n2) = 
  failwith "Not implemented"



(* 
 *  Optional question
 *)


type bConst = True | False

type bExpr = Constant of bConst
           | Variable of string
           | And of bExpr * bExpr
           | Or of bExpr * bExpr
           | Not of bExpr

let sample1 = And(Not(Variable "a"),Not(Variable "b"))

let sample2 = Or(Not(Variable "a"),And(Variable "b",Constant(True)))

let sample3 = And(Variable "a", Not(Variable "a"))

let vars (bexpr) = 
  failwith "Not implemented"

let subst (bexpr,var,sub) = 
  failwith "Not implemented"

let eval (bexpr) = 
  failwith "Not implemented"
