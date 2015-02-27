(*
#use "workout.ml";;
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

let thirds xs = map(third,xs);;

let pairs (a,xs) = map ((fun x -> (a,x)), xs);;

(* TESTS -> IN CLASS ON FEB 23
filter (fun x -> x > 0) [1;-2;3;-4;5];;
removeNone [Some 1; None; Some 2; None; Some 5];;
flatten [[1;2] ; [3;4] ; [5] ; []; [6;7]];;
*)

let rec filter f l = match l with
	[] -> []
	| h::t -> if (f h) then h::filter f t else filter f t;;

let removeNone l = filter (fun x -> match x with
	Some y -> true | None -> false) l;;

let rec map_append f xs  =
   match xs with
 	[] -> []
   	| x :: xs -> (f x)@(map_append f xs);;

let flatten l =  map_append (fun x -> x) l ;;

let rec gen (i,x) = if (i <= 0) then [] else x::(gen(i-1,x))

let expand l = map_append gen l ;;

let new_filter p l = map_append (fun x -> if p(x) then [x] else []) l;;

let cons a b = a::b;;

let app a b = a@b;;

let rec map_gen comb f xs =
   match xs with
 	[] -> []
   	| x :: xs -> comb (f x) (map_append f xs);;
