(* FoCS Spring 2015

   Homework 1 code


   Name: Sophia Seitz

   Email: sophia.seitz@students.olin.edu

   Comments: 

 *)



(* 
 *  Question 1
 *)

let rec append (xs,ys) =  match xs,ys with 
  | [],[] -> []
  | a::b,[] -> xs
  | [],c::d -> ys
  | a::b,c::d -> a::append(b,ys);;
  (*  http://stackoverflow.com/questions/8286001/in-ocaml-what-is-the-canonical-way-of-matching-against-multiple-arguments-of-a
  *)
  
let rec flatten (xs) = match xs with 
  | [] -> [] 
  | h::t -> append(h,flatten (t));;

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


let rec setSub (set1,set2) = match set1, set2 with
  | [],[] -> true
  | [],h::t -> true
  | x::y, [] -> false
  | x::y, h::t -> setIn(x,set2) && setSub(y,set2);;

let setEqual (set1,set2) = setSub(set1,set2) && setSub(set2,set1);;

let rec setUnion (set1,set2) = match set1,set2 with
  | [],[] -> []
  | [], h::t -> if setIn(h,setUnion([],t)) then setUnion([],t) else h::setUnion([],t)
  | x::y, [] -> if setIn(x,setUnion([],y)) then setUnion([],y) else x::setUnion([],y)
  | x::y, h::t -> if setIn(x,setUnion([],set2)) then setUnion(y,set2) else setUnion(y, x::setUnion([],set2));;

let rec setInter (set1,set2) = match set1,set2 with
  | [],_ -> []
  | _, [] -> []
  | x::y, h::t -> if setIn(x,set2) then x::setInter(y,set2) else setInter(y,set2);;

let rec setSize (set) = match set with
  | [] -> 0
  | h::t -> if setIn(h,t) then setSize(t) else 1+setSize(t);;


(* 
 *  Question 3
 *)

type rat = {num: int; den: int}

let half = {num=1; den=2}
let third = {num=1; den=3}
let fourth = {num=1; den=4}

let floatR (r) = float(r.num) /. float(r.den)

let isDivisible (n,d) = if float(n/d) = float(n)/.float(d) then true else false

let absInt (n) = if n >= 0 then n else -1*n

let rec allPossFactors (n) = if n = 1 then [] else absInt(n)::allPossFactors(absInt(n)-1);;

let rec getFactorsWithList (n, possFactors) = match possFactors with 
 | [] -> []
 | h::t -> if isDivisible(n,h) then h::getFactorsWithList(n,t) else getFactorsWithList(n,t);;

let rec getFactors(n) = getFactorsWithList(n,allPossFactors(n));;

let rec recMax (x,l) = match l with
  | [] -> x
  | h::t -> if x>h then recMax(x,t) else recMax(h,t);;

let maxList (l) = match l with 
  | [] -> 0
  | h::t -> recMax(h,t);;

let findGCF (factors1, factors2) = maxList(setInter(factors1,factors2));;

let simplify (r) = 
  if r.den > 0 
    then  {num = (r.num/findGCF(getFactors(r.num),getFactors(r.den))); den = (r.den/findGCF(getFactors(r.num),getFactors(r.den)))}
  else 
    {num = (r.num/(-findGCF(getFactors(r.num),getFactors(r.den)))); den = (r.den/(-findGCF(getFactors(r.num),getFactors(r.den))))};;

let addR (r1,r2) = simplify({num = (r1.num*r2.den + r2.num*r1.den); den = (r1.den*r2.den)});;

let multR (r1,r2) = simplify({num = (r1.num*r2.num); den = (r1.den*r2.den)});;
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
