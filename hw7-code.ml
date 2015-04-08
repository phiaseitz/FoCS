(* FoCS Spring 2015

   Homework 7 code


   Name: Sophia Seitz

   Email: sophia.seitz@students.olin.edu

   Comments: I worked with Anne Wilkinson on generating the Turing Machines
   on proble 2

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


let question2a = { tm_states = ["start";"a1";"b1";"a2";"b2";"rew";"sx";
				"xa1";"xb1";"xa2";"xb2";"acc";"rej"];
		   tm_input_alph = ["a"; "b"];
		   tm_tape_alph = ["a";"b";"X";">";"_"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [("start", "a", "rej", "a", Right);
		   	("start", "b", "rej", "b", Right);
		   	("start", "X", "rej", "X", Right);
		   	("start", ">", "a1", ">", Right);
		   	("start", "_", "rej", "_", Right);
		   	("a1", "a", "a1", "a", Right);
		   	("a1", "b", "b1", "b", Right);
		   	("a1", "X", "rej", "X", Right);
		   	("a1", ">", "rej", ">", Right);
		   	("a1", "_", "b1", "_", Right);
		   	("b1", "a", "a2", "a", Right);
		   	("b1", "b", "b1", "b", Right);
		   	("b1", "X", "rej", "X", Right);
		   	("b1", ">", "rej", ">", Right);
		   	("b1", "_", "a2", "_", Right);
		   	("a2", "a", "a2", "a", Right);
		   	("a2", "b", "b2", "b", Right);
		   	("a2", "X", "rej", "X", Right);
		   	("a2", ">", "rej", ">", Right);
		   	("a2", "_", "b2", "_", Right);
		   	("b2", "a", "rej", "a", Right);
		   	("b2", "b", "b2", "b", Right);
		   	("b2", "X", "rej", "X", Right);
		   	("b2", ">", "rej", ">", Right);
		   	("b2", "_", "rew", "_", Right);
		   	("rew", "a", "rew", "a", Left);
		   	("rew", "b", "rew", "b", Left);
		   	("rew", "X", "rew", "X", Left);
		   	("rew", ">", "sx", ">", Right);
		   	("rew", "_", "rew", "_", Left);
		   	("sx", "a", "xa1", "X", Right);
		   	("sx", "b", "rej", "b", Right);
		   	("sx", "X", "sx", "X", Right);
		   	("sx", ">", "rej", ">", Right);
		   	("sx", "_", "acc", "_", Right);
		   	("xa1", "a", "xa1", "a", Right);
		   	("xa1", "b", "xb1", "X", Right);
		   	("xa1", "X", "xa1", "X", Right);
		   	("xa1", ">", "rej", ">", Right);
		   	("xa1", "_", "rej", "_", Right);
		   	("xb1", "a", "xa2", "X", Right);
		   	("xb1", "b", "xb1", "b", Right);
		   	("xb1", "X", "xb1", "X", Right);
		   	("xb1", ">", "rej", ">", Right);
		   	("xb1", "_", "rej", "_", Right);
		   	("xa2", "a", "xa2", "a", Right);
		   	("xa2", "b", "xb2", "X", Right);
		   	("xa2", "X", "xa2", "X", Right);
		   	("xa2", ">", "rej", ">", Right);
		   	("xa2", "_", "rej", "_", Right);
		   	("xb2", "a", "rej", "a", Right);
		   	("xb2", "b", "rew", "b", Left);
		   	("xb2", "X", "rej", "X", Right);
		   	("xb2", ">", "rej", ">", Right);
		   	("xb2", "_", "rew", "_", Left);
		   	("acc", "a", "acc", "a", Right);
		   	("acc", "b", "acc", "b", Right);
		   	("acc", "X", "acc", "X", Right);
		   	("acc", ">", "acc", ">", Right);
		   	("acc", "_", "acc", "_", Right);
		   	("rej", "a", "rej", "a", Right);
		   	("rej", "b", "rej", "b", Right);
		   	("rej", "X", "rej", "X", Right);
		   	("rej", ">", "rej", ">", Right);
		   	("rej", "_", "rej", "_", Right)];
		   tm_start = "start";
		   tm_accept = "acc";
		   tm_reject = "rej" }


let question2b = { tm_states = ["start";"ca1";"cb";"ca2";"rew";"q1";"q2";"q3";
			"q4";"q5";"q6";"q7";"q8";"q9";"q10";"q11";"acc";"rej"];
		   tm_input_alph = ["a"; "b"];
		   tm_tape_alph = ["a";"b";"B";"X";">";"_"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [("start", "a", "rej", "a", Right);
		   	("start", "b", "rej", "b", Right);
		   	("start", "B", "rej", "B", Right);
		   	("start", "X", "rej", "X", Right);
		   	("start", ">", "ca1", ">", Right);
		   	("start", "_", "rej", "_", Right);
		   	("ca1", "a", "ca1", "a", Right);
		   	("ca1", "b", "cb", "b", Right);
		   	("ca1", "B", "rej", "B", Right);
		   	("ca1", "X", "rej", "X", Right);
		   	("ca1", ">", "rej", ">", Right);
		   	("ca1", "_", "cb", "_", Right);
		   	("cb", "a", "ca2", "a", Right);
		   	("cb", "b", "cb", "b", Right);
		   	("cb", "B", "rej", "B", Right);
		   	("cb", "X", "rej", "X", Right);
		   	("cb", ">", "rej", ">", Right);
		   	("cb", "_", "ca2", "_", Right);
		   	("ca2", "a", "ca2", "a", Right);
		   	("ca2", "b", "rej", "b", Right);
		   	("ca2", "B", "rej", "B", Right);
		   	("ca2", "X", "rej", "X", Right);
		   	("ca2", ">", "rej", ">", Right);
		   	("ca2", "_", "rew", "_", Left);
		   	("rew", "a", "rew", "a", Left);
		   	("rew", "b", "rew", "b", Left);
		   	("rew", "B", "rew", "B", Left);
		   	("rew", "X", "rew", "X", Left);
		   	("rew", ">", "q1", ">", Right);
		   	("rew", "_", "rew", "_", Left);
		   	("q1", "a", "q2", "X", Right);
		   	("q1", "b", "q11", "b", Right);
		   	("q1", "B", "q11", "B", Right);
		   	("q1", "X", "q1", "X", Right);
		   	("q1", ">", "rej", ">", Right);
		   	("q1", "_", "q11", "_", Right);
		   	("q2", "a", "q2", "a", Right);
		   	("q2", "b", "q3", "B", Right);
		   	("q2", "B", "q7", "b", Right);
		   	("q2", "X", "rej", "X", Right);
		   	("q2", ">", "rej", ">", Right);
		   	("q2", "_", "q11", "_", Right);
		   	("q3", "a", "q4", "X", Left);
		   	("q3", "b", "q3", "b", Right);
		   	("q3", "B", "rej", "B", Right);
		   	("q3", "X", "q3", "X", Right);
		   	("q3", ">", "rej", ">", Right);
		   	("q3", "_", "rej", "_", Right);
		   	("q4", "a", "rej", "a", Right);
		   	("q4", "b", "q4", "b", Left);
		   	("q4", "B", "q5", "B", Right);
		   	("q4", "X", "q4", "X", Left);
		   	("q4", ">", "rej", ">", Right);
		   	("q4", "_", "rej", "_", Right);
		   	("q5", "a", "rej", "a", Right);
		   	("q5", "b", "q6", "B", Right);
		   	("q5", "B", "rej", "B", Right);
		   	("q5", "X", "rew", "X", Left);
		   	("q5", ">", "rej", ">", Right);
		   	("q5", "_", "rej", "_", Right);
		   	("q6", "a", "q4", "X", Left);
		   	("q6", "b", "q6", "b", Right);
		   	("q6", "B", "rej", "B", Right);
		   	("q6", "X", "q6", "X", Right);
		   	("q6", ">", "rej", ">", Right);
		   	("q6", "_", "rej", "_", Right);
		   	("q7", "a", "q8", "X", Left);
		   	("q7", "b", "rej", "b", Right);
		   	("q7", "B", "q7", "B", Right);
		   	("q7", "X", "q7", "X", Right);
		   	("q7", ">", "rej", ">", Right);
		   	("q7", "_", "rej", "_", Right);
		   	("q8", "a", "rej", "a", Right);
		   	("q8", "b", "q9", "b", Right);
		   	("q8", "B", "q8", "B", Left);
		   	("q8", "X", "q8", "X", Left);
		   	("q8", ">", "rej", ">", Right);
		   	("q8", "_", "rej", "_", Right);
		   	("q9", "a", "rej", "a", Right);
		   	("q9", "b", "rej", "b", Right);
		   	("q9", "B", "q10", "b", Right);
		   	("q9", "X", "rew", "X", Left);
		   	("q9", ">", "rej", ">", Right);
		   	("q9", "_", "rej", "_", Right);
		   	("q10", "a", "q8", "X", Left);
		   	("q10", "b", "rej", "b", Right);
		   	("q10", "B", "q10", "B", Right);
		   	("q10", "X", "q10", "X", Right);
		   	("q10", ">", "rej", ">", Right);
		   	("q10", "_", "rej", "_", Right);
		   	("q11", "a", "rej", "a", Right);
		   	("q11", "b", "q11", "b", Right);
		   	("q11", "B", "q11", "B", Right);
		   	("q11", "X", "q11", "X", Right);
		   	("q11", ">", "rej", ">", Right);
		   	("q11", "_", "acc", "_", Right);
		   	("acc", "a", "acc", "a", Right);
		   	("acc", "b", "acc", "b", Right);
		   	("acc", "B", "acc", "B", Right);
		   	("acc", "X", "acc", "X", Right);
		   	("acc", ">", "acc", ">", Right);
		   	("acc", "_", "acc", "_", Right);
		   	("rej", "a", "rej", "a", Right);
		   	("rej", "b", "rej", "b", Right);
		   	("rej", "B", "rej", "B", Right);
		   	("rej", "X", "rej", "X", Right);
		   	("rej", ">", "rej", ">", Right);
		   	("rej", "_", "rej", "_", Right)];
		   tm_start = "start";
		   tm_accept = "acc";
		   tm_reject = "rej" }



(* QUESTION 3 *)


let binary_sum = { tm_states = ["start";"w3";"w3d";"w1";"w1d";"w2";"w2d";"q1";"q2";"q3";
			"q4";"q5";"q6";"q7";"q8";"q9";"q10";"q11";"q12";"q13";"q14";
			"q15";"q16";"q17";"q18";"q19";"q20";"q21";"q22";"q23";"q24";
			"q25";"q26";"q27";"for";"forc";"acc";"rej"];
		   tm_input_alph = ["0";"1";"#"];
		   tm_tape_alph = ["0";"1";"#";">";"_";"X"];
		   tm_leftmost = ">";
		   tm_blank = "_";
		   tm_delta = [("start", "0", "start", "0", Right);
		   	("start", "1", "start", "1", Right);
		   	("start", "#", "start", "#", Right);
		   	("start", ">", "w3", ">", Right);
		   	("start", "_", "start", "_", Right);
		   	("start", "X", "start", "X", Right);
		   	("w3", "0", "w3d", "0", Right);
		   	("w3", "1", "w3d", "1", Right);
		   	("w3", "#", "rej", "#", Right);
		   	("w3", ">", "rej", ">", Right);
		   	("w3", "_", "rej", "_", Right);
		   	("w3", "X", "rej", "X", Right);
		   	("w3d", "0", "w3d", "0", Right);
		   	("w3d", "1", "w3d", "1", Right);
		   	("w3d", "#", "w1", "#", Right);
		   	("w3d", ">", "rej", ">", Right);
		   	("w3d", "_", "rej", "_", Right);
		   	("w3d", "X", "rej", "X", Right);
		   	("w1", "0", "w1d", "0", Right);
		   	("w1", "1", "w1d", "1", Right);
		   	("w1", "#", "rej", "#", Right);
		   	("w1", ">", "rej", ">", Right);
		   	("w1", "_", "rej", "_", Right);
		   	("w1", "X", "rej", "X", Right);
		   	("w1d", "0", "w1d", "0", Right);
		   	("w1d", "1", "w1d", "1", Right);
		   	("w1d", "#", "w2", "#", Right);
		   	("w1d", ">", "rej", ">", Right);
		   	("w1d", "_", "rej", "_", Right);
		   	("w1d", "X", "rej", "X", Right);
		   	("w2", "0", "w2", "0", Right);
		   	("w2", "1", "w2", "1", Right);
		   	("w2", "#", "rej", "#", Right);
		   	("w2", ">", "rej", ">", Right);
		   	("w2", "_", "q1", "_", Left);
		   	("w2", "X", "rej", "X", Right);
		   	("w2d", "0", "w2d", "0", Right);
		   	("w2d", "1", "w2d", "1", Right);
		   	("w2d", "#", "rej", "#", Right);
		   	("w2d", ">", "rej", ">", Right);
		   	("w2d", "_", "rej", "_", Right);
		   	("w2d", "X", "rej", "X", Right);
		   	("q1", "0", "q2", "X", Left);
		   	("q1", "1", "q8", "X", Left);
		   	("q1", "#", "q1", "#", Left);
		   	("q1", ">", "acc", ">", Right);
		   	("q1", "_", "q1", "_", Left);
		   	("q1", "X", "q1", "X", Left);
		   	("q2", "0", "q2", "0", Left);
		   	("q2", "1", "q2", "1", Left);
		   	("q2", "#", "q3", "#", Left);
		   	("q2", ">", "rej", ">", Right);
		   	("q2", "_", "rej", "_", Right);
		   	("q2", "X", "rej", "X", Right);
		   	("q3", "0", "q6", "X", Left);
		   	("q3", "1", "q4", "X", Left);
		   	("q3", "#", "rej", "#", Right);
		   	("q3", ">", "rej", ">", Right);
		   	("q3", "_", "rej", "_", Right);
		   	("q3", "X", "q3", "X", Left);
		   	("q4", "0", "q4", "0", Left);
		   	("q4", "1", "q4", "1", Left);
		   	("q4", "#", "q5", "#", Left);
		   	("q4", ">", "rej", ">", Left);
		   	("q4", "_", "rej", "_", Left);
		   	("q4", "X", "rej", "X", Left);
		   	("q5", "0", "rej", "0", Right);
		   	("q5", "1", "for", "X", Right);
		   	("q5", "#", "rej", "#", Right);
		   	("q5", ">", "rej", ">", Right);
		   	("q5", "_", "rej", "_", Right);
		   	("q5", "X", "q5", "X", Left);
		   	("q6", "0", "q6", "0", Left);
		   	("q6", "1", "q6", "1", Left);
		   	("q6", "#", "q7", "#", Left);
		   	("q6", ">", "rej", ">", Right);
		   	("q6", "_", "rej", "_", Right);
		   	("q6", "X", "rej", "X", Right);
		   	("q7", "0", "for", "X", Right);
		   	("q7", "1", "rej", "1", Right);
		   	("q7", "#", "rej", "#", Right);
		   	("q7", ">", "rej", ">", Right);
		   	("q7", "_", "rej", "_", Right);
		   	("q7", "X", "q7", "X", Left);
		   	("q8", "0", "q8", "0", Left);
		   	("q8", "1", "q8", "1", Left);
		   	("q8", "#", "q9", "#", Left);
		   	("q8", ">", "rej", ">", Right);
		   	("q8", "_", "rej", "_", Right);
		   	("q8", "X", "rej", "X", Right);
		   	("q9", "0", "q10", "X", Left);
		   	("q9", "1", "q12", "X", Left);
		   	("q9", "#", "rej", "#", Left);
		   	("q9", ">", "rej", ">", Left);
		   	("q9", "_", "rej", "_", Left);
		   	("q9", "X", "q9", "X", Left);
		   	("q10", "0", "q10", "0", Left);
		   	("q10", "1", "q10", "1", Left);
		   	("q10", "#", "q11", "#", Left);
		   	("q10", ">", "rej", ">", Right);
		   	("q10", "_", "rej", "_", Right);
		   	("q10", "X", "rej", "X", Right);
		   	("q11", "0", "rej", "0", Right);
		   	("q11", "1", "for", "X", Right);
		   	("q11", "#", "rej", "#", Right);
		   	("q11", ">", "rej", ">", Right);
		   	("q11", "_", "rej", "_", Right);
		   	("q11", "X", "q11", "X", Left);
		   	("q12", "0", "q12", "0", Left);
		   	("q12", "1", "q12", "1", Left);
		   	("q12", "#", "q13", "#", Left);
		   	("q12", ">", "rej", ">", Right);
		   	("q12", "_", "rej", "_", Right);
		   	("q12", "X", "rej", "X", Right);
		   	("q13", "0", "q14", "X", Left);
		   	("q13", "1", "rej", "1", Right);
		   	("q13", "#", "rej", "#", Right);
		   	("q13", ">", "rej", ">", Right);
		   	("q13", "_", "rej", "_", Right);
		   	("q13", "X", "q13", "X", Left);
		   	("q14", "0", "forc", "0", Right);
		   	("q14", "1", "forc", "1", Right);
		   	("q14", "#", "forc", "#", Right);
		   	("q14", ">", "rej", ">", Right);
		   	("q14", "_", "rej", "_", Right);
		   	("q14", "X", "forc", "X", Right);
		   	("q15", "0", "q22", "X", Left);
		   	("q15", "1", "q16", "X", Left);
		   	("q15", "#", "q15", "#", Left);
		   	("q15", ">", "rej", ">", Right);
		   	("q15", "_", "q15", "_", Left);
		   	("q15", "X", "q15", "X", Left);
		   	("q16", "0", "q16", "0", Left);
		   	("q16", "1", "q16", "1", Left);
		   	("q16", "#", "q17", "#", Left);
		   	("q16", ">", "rej", ">", Right);
		   	("q16", "_", "rej", "_", Right);
		   	("q16", "X", "rej", "X", Right);
		   	("q17", "0", "q20", "X", Left);
		   	("q17", "1", "q18", "X", Left);
		   	("q17", "#", "rej", "#", Right);
		   	("q17", ">", "rej", ">", Right);
		   	("q17", "_", "rej", "_", Right);
		   	("q17", "X", "q17", "X", Left);
		   	("q18", "0", "q18", "0", Left);
		   	("q18", "1", "q18", "1", Left);
		   	("q18", "#", "q19", "#", Left);
		   	("q18", ">", "rej", ">", Right);
		   	("q18", "_", "rej", "_", Right);
		   	("q18", "X", "rej", "X", Right);
		   	("q19", "0", "rej", "0", Right);
		   	("q19", "1", "forc", "X", Right);
		   	("q19", "#", "rej", "#", Right);
		   	("q19", ">", "rej", ">", Right);
		   	("q19", "_", "rej", "_", Right);
		   	("q19", "X", "q19", "X", Left);
		   	("q20", "0", "q20", "0", Left);
		   	("q20", "1", "q20", "1", Left);
		   	("q20", "#", "q21", "#", Left);
		   	("q20", ">", "rej", ">", Right);
		   	("q20", "_", "rej", "_", Right);
		   	("q20", "X", "rej", "X", Right);
		   	("q21", "0", "forc", "X", Right);
		   	("q21", "1", "rej", "1", Right);
		   	("q21", "#", "rej", "#", Right);
		   	("q21", ">", "rej", ">", Right);
		   	("q21", "_", "rej", "_", Right);
		   	("q21", "X", "q21", "X", Left);
		   	("q22", "0", "q22", "0", Left);
		   	("q22", "1", "q22", "1", Left);
		   	("q22", "#", "q23", "#", Left);
		   	("q22", ">", "rej", ">", Right);
		   	("q22", "_", "rej", "_", Right);
		   	("q22", "X", "rej", "X", Right);
		   	("q23", "0", "q26", "X", Left);
		   	("q23", "1", "q24", "X", Left);
		   	("q23", "#", "rej", "#", Right);
		   	("q23", ">", "rej", ">", Right);
		   	("q23", "_", "rej", "_", Right);
		   	("q23", "X", "q23", "X", Left);
		   	("q24", "0", "q24", "0", Left);
		   	("q24", "1", "q24", "1", Left);
		   	("q24", "#", "q25", "#", Left);
		   	("q24", ">", "rej", ">", Right);
		   	("q24", "_", "rej", "_", Right);
		   	("q24", "X", "rej", "X", Right);
		   	("q25", "0", "forc", "X", Right);
		   	("q25", "1", "rej", "1", Right);
		   	("q25", "#", "rej", "#", Right);
		   	("q25", ">", "rej", ">", Right);
		   	("q25", "_", "rej", "_", Right);
		   	("q25", "X", "q25", "X", Left);
		   	("q26", "0", "q26", "0", Left);
		   	("q26", "1", "q26", "1", Left);
		   	("q26", "#", "q27", "#", Left);
		   	("q26", ">", "rej", ">", Right);
		   	("q26", "_", "rej", "_", Right);
		   	("q26", "X", "rej", "X", Right);
		   	("q27", "0", "rej", "0", Right);
		   	("q27", "1", "for", "X", Right);
		   	("q27", "#", "rej", "#", Right);
		   	("q27", ">", "rej", ">", Right);
		   	("q27", "_", "rej", "_", Right);
		   	("q27", "X", "q27", "X", Left);
		   	("for", "0", "for", "0", Right);
		   	("for", "1", "for", "1", Right);
		   	("for", "#", "for", "#", Right);
		   	("for", ">", "rej", ">", Right);
		   	("for", "_", "q1", "_", Left);
		   	("for", "X", "for", "X", Right);
		   	("forc", "0", "forc", "0", Right);
		   	("forc", "1", "forc", "1", Right);
		   	("forc", "#", "forc", "#", Right);
		   	("forc", ">", "rej", ">", Right);
		   	("forc", "_", "q15", "_", Left);
		   	("forc", "X", "forc", "X", Right);
		   	("rej", "0", "rej", "0", Right);
		   	("rej", "1", "rej", "1", Right);
		   	("rej", "#", "rej", "#", Right);
		   	("rej", ">", "rej", ">", Right);
		   	("rej", "_", "rej", "_", Right);
		   	("rej", "X", "rej", "X", Right);
		   	("acc", "0", "acc", "0", Right);
		   	("acc", "1", "acc", "1", Right);
		   	("acc", "#", "acc", "#", Right);
		   	("acc", ">", "acc", ">", Right);
		   	("acc", "_", "acc", "_", Right);
		   	("acc", "X", "acc", "X", Right)];
		   tm_start = "start";
		   tm_accept = "acc";
		   tm_reject = "rej" }


