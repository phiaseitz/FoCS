(* FoCS Spring 2015

   Homework 1 code


   Name:

   Email:

   Comments:

 *)



(* 
 *  Question 1
 *)

let append (xs,ys) =   
  (* Replace with your implementation of append *)
  failwith "Not implemented"


let flatten (xs) = 
  failwith "Not implemented"

let double (xs) = 
  failwith "Not implemented"

let last (xs) = 
  failwith "Not implemented"



(*
 *  Question 2 
 *)

let setIn (elt,set) = 
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
