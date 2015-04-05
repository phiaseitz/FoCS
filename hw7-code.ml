(* FoCS Spring 2015

   Homework 7 code


   Name: Sophia Seitz

   Email: sophia.seitz@students.olin.edu

   Comments:

 *)



(* 
 *   explode_str : string -> string list
 *      returns the list of characters making up a string
 *      (where each character gets returned as a string)
 *
 *)

let explode_str (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.sub str index 1)::result)
  in
    acc(String.length(str)-1, [])




(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type direction = Left | Right

type symbol = string

type 'a tm = { tm_states : 'a list;
               tm_input_alph : symbol list;
	       tm_tape_alph : symbol list;
	       tm_leftmost : symbol;
	       tm_blank : symbol;
	       tm_delta : ('a * symbol * 'a * symbol * direction) list;
	       tm_start : 'a;
	       tm_accept : 'a;
	       tm_reject : 'a }


(*
 * Print a configuration (including newline) to standard output
 *  and return a value
 * 
 *)

let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value




(* QUESTION 1 *)

let starting_config m w = ([],m.tm_start,">"::(explode_str w));;

let accepting_config m c = match c with (u,q,v) -> q = m.tm_accept;;

let rejecting_config m c = match c with (u,q,v) -> q = m.tm_reject;;

let halting_config m c = (accepting_config m c)||(rejecting_config m c);;

let find_delta m q v = match (List.filter(fun delta_fun-> 
	match delta_fun with (bstate, read, _, _,_) -> 
		if (bstate = q) && (read = v) then true else false) 
	m.tm_delta) with 
		h::t -> h;;

let rec chop_last_elem l = match (List.rev l) with h::t -> ((List.rev t),h);;

let step_config m c = match c with (u,q,v) -> match v with
	[] -> (match (find_delta m q "_") with 
		(bstate, read, estate, write, dir) -> (match dir with
			Right -> (u@[write], estate, [])
			| Left -> (match (chop_last_elem u) with (newu, ulast) ->
					(newu, estate, ulast::[write]))))
	| v1::v' -> (match (find_delta m q v1) with 
		(bstate, read, estate, write, dir) -> (match dir with
				Right -> (u@[write], estate, v')
				| Left -> (match (chop_last_elem u) with (newu, ulast) ->
					(newu, estate, ulast::(write::v')))));;

let rec step_until_halt m c = if (print_config m c (halting_config m c)) 
		then c 
	else (step_until_halt m (step_config m c));;

let run m w = accepting_config m (step_until_halt m (starting_config m w));;



(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { tm_states = ["start"; "q1"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "acc", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "_", "acc", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbn = { tm_states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     tm_input_alph = ["a";"b"];
	     tm_tape_alph = ["a";"b";"X";"_";">"];
	     tm_blank = "_";
	     tm_leftmost = ">";
	     tm_start = "start";
	     tm_accept = "acc";
	     tm_reject = "rej";
	     tm_delta = [ ("start", "a", "start", "a", Right);
     	                ("start", "b", "q1", "b", Right);
		        ("start", ">", "start", ">", Right);
		        ("start", "_", "q2", "_", Right);
		        ("start", "X", "rej", "X", Right);
		        ("q1", "b", "q1", "b", Right);
		        ("q1", "_", "q2", "_", Right);
		        ("q1", "a", "rej", "a", Right);
		        ("q1", ">", "rej", ">", Right);
		        ("q1", "X", "rej", "X", Right);
		        ("q2", ">", "q3", ">", Right);
		        ("q2", "a", "q2", "a", Left);
		        ("q2", "b", "q2", "b", Left);
		        ("q2", "X", "q2", "X", Left);
		        ("q2", "_", "q2", "_", Left);
		        ("q3", "X", "q3", "X", Right);
		        ("q3", "_", "acc", "_", Right);
		        ("q3", "a", "q4", "X", Right);
		        ("q3", "b", "rej", "b", Right);
		        ("q3", ">", "rej", ">", Right);
		        ("q4", "a", "q4", "a", Right);
		        ("q4", "X", "q4", "X", Right);
		        ("q4", "b", "q2", "X", Right);
		        ("q4", "a", "rej", "a", Right);
		        ("q4", ">", "rej", ">", Right);
		        ("q4", "_", "rej", "_", Right);
		        ("acc", "a", "acc", "a", Right);
		        ("acc", "b", "acc", "b", Right);
		        ("acc", ">", "acc", ">", Right);
		        ("acc", "X", "acc", "X", Right);
		        ("acc", "_", "acc", "_", Right);
		        ("rej", "a", "rej", "a", Right);
		        ("rej", "b", "rej", "b", Right);
		        ("rej", ">", "rej", ">", Right);
		        ("rej", "X", "rej", "X", Right);
		        ("rej", "_", "rej", "_", Right)] }

let anbncn = { tm_states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       tm_input_alph = ["a";"b";"c"];
	       tm_tape_alph = ["a";"b";"c";"X";"_";">"];
	       tm_blank = "_";
	       tm_leftmost = ">";
	       tm_start = "start";
	       tm_accept = "acc";
	       tm_reject = "rej";
	       tm_delta = [ ("start", "a", "start", "a", Right);
     	          ("start", "b", "q1", "b", Right);
		  ("start", "c", "q6", "c", Right);
		  ("start", ">", "start", ">", Right);
		  ("start", "_", "q2", "_", Right);
		  ("start", "X", "rej", "X", Right);
		  ("q1", "b", "q1", "b", Right);
		  ("q1", "c", "q6", "c", Right);
		  ("q1", "_", "q2", "_", Right);
		  ("q1", "a", "rej", "a", Right);
		  ("q1", ">", "rej", ">", Right);
		  ("q1", "X", "rej", "X", Right);
		  ("q2", ">", "q3", ">", Right);
		  ("q2", "a", "q2", "a", Left);
		  ("q2", "b", "q2", "b", Left);
		  ("q2", "c", "q2", "c", Left);
		  ("q2", "_", "q2", "_", Left);
		  ("q2", "X", "q2", "X", Left);
		  ("q3", "X", "q3", "X", Right);
		  ("q3", "_", "acc", "_", Right);
		  ("q3", "a", "q4", "X", Right);
		  ("q3", "b", "rej", "b", Right);
		  ("q3", "c", "rej", "c", Right);
		  ("q3", ">", "rej", ">", Right);
		  ("q4", "a", "q4", "a", Right);
		  ("q4", "X", "q4", "X", Right);
		  ("q4", "b", "q5", "X", Right);
		  ("q4", "c", "rej", "c", Right);
		  ("q4", "_", "rej", "_", Right);
		  ("q4", ">", "rej", ">", Right);
		  ("q5", "b", "q5", "b", Right);
		  ("q5", "X", "q5", "X", Right);
		  ("q5", "c", "q2", "X", Right);
		  ("q5", "a", "rej", "a", Right);
		  ("q5", "_", "rej", "_", Right);
		  ("q5", ">", "rej", ">", Right);
		  ("q6", "c", "q6", "c", Right);
		  ("q6", "_", "q2", "_", Right);
		  ("q6", "a", "rej", "a", Right);
		  ("q6", "b", "rej", "b", Right);
		  ("q6", ">", "rej", ">", Right);
		  ("q6", "X", "rej", "X", Right);
		  ("acc", "a", "acc", "a", Right);
		  ("acc", "b", "acc", "b", Right);
		  ("acc", "c", "acc", "c", Right);
		  ("acc", ">", "acc", ">", Right);
		  ("acc", "X", "acc", "X", Right);
		  ("acc", "_", "acc", "_", Right);
		  ("rej", "a", "rej", "a", Right);
		  ("rej", "b", "rej", "b", Right);
		  ("rej", "c", "rej", "c", Right);
		  ("rej", ">", "rej", ">", Right);
		  ("rej", "X", "rej", "X", Right);
		  ("rej", "_", "rej", "_", Right)] }




(* QUESTION 2 *)


let question2a = { tm_states = [];
		   tm_input_alph = [];
		   tm_tape_alph = [];
		   tm_leftmost = "";
		   tm_blank = "";
		   tm_delta = [];
		   tm_start = "";
		   tm_accept = "";
		   tm_reject = "" }


let question2b = { tm_states = [];
		   tm_input_alph = [];
		   tm_tape_alph = [];
		   tm_leftmost = "";
		   tm_blank = "";
		   tm_delta = [];
		   tm_start = "";
		   tm_accept = "";
		   tm_reject = "" }



(* QUESTION 3 *)


let binary_sum = { tm_states = [];
		   tm_input_alph = [];
		   tm_tape_alph = [];
		   tm_leftmost = "";
		   tm_blank = "";
		   tm_delta = [];
		   tm_start = "";
		   tm_accept = "";
		   tm_reject = "" }


