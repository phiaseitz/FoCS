(*
#use "l2_workout.ml";;
*)

let rec squares l = match l with
	[] -> []
	| h::t -> h*h::squares(t);;

let rec diags l =  match l with
	[] -> []
	| h::t -> (h,h)::diags(t);;

let rec heads l = match l with
	[] -> []
	| h::t -> match h with 
		[] -> None::heads(t)
		| x::y -> (Some x)::heads(t);;

let diag x = (x,x);;

let rec diagsh xs = match xs with
	[] -> []
	| x::xs' -> diag(x)::diagsh(xs');;

let head l = match l with
	[] -> None
	| h::t -> Some h;;

let rec headsh lol = match lol with
	[] -> []
	| l::lol' -> head(l)::headsh(lol');;

let rec map (f,xs) =
   match xs with
 	[] -> []
   	| x :: xs' -> (f x)::(map (f,xs'));;

let triples xs = map((fun x -> (x,x+1,x+2)),xs);;

let third x = match x with (x1,x2,x3) -> x3;;

let thirds xs = map(third,xs)

let pairs (a,xs) = map ((fun x -> (a,x)), xs)