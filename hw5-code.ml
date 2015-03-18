(* FoCS Spring 2015

   Homework 5 code


   Name: Sophia Seitz 

   Email: sophia.seitz@students.olin.edu 

   Comments:

 *)



(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)




(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings (alphabet, n) = 
  let rec mapCons (c, ls) = 
    match ls with
      [] -> []
    | l::ls' -> (c::l)::mapCons(c,ls')  in
  let rec mapConsSet (alphabet, l) = 
    match alphabet with
      [] -> []
    | c::cs -> mapCons(c,l) @ mapConsSet(cs,l)  in
  let rec mapImplode (css) = 
    match css with
      [] -> []
    | (cs::css) -> (implode cs)::mapImplode(css)  in
  let rec strings' (n) = 
    if (n<=0) then
      [[]]
    else let s = strings'(n-1) in
      [] :: mapConsSet(alphabet,s)
  in 
    mapImplode(strings'(n))






(*
 * The type for PDAs
 * 
 *)

type 'a pda = {pda_states : 'a list;
               pda_alphabet : char list;
               pda_delta : ('a * char option * char * 'a * char list) list;
               pda_start : 'a;
               pda_final : 'a list;
               pda_stack_alph : char list;
               pda_bottom : char}




let initial_config p w = (p.pda_start, explode (w), [p.pda_bottom]);;

let is_accepting_state p s = 
  List.fold_right(fun state accept -> state = s || accept) p.pda_final false;;

let accepting_config pda cfg = match cfg with (state,remaining,stack) ->
  match remaining with 
  [] -> is_accepting_state pda state
  | h::t -> false;;


let has_accepting_config pda cfgs = List.fold_right
  (fun cfg accepting -> (accepting_config pda cfg)||accepting) cfgs false;;

let step_config pda cfg = failwith "not implemented"

let step_configs pda cfgs = failwith "not implemented"

let accept pda w = failwith "not implemented"



(*
 * Create list of all strings of length <= n over a given alphabet
 *
 *)

let strings alphabet n = 
  let rec mapCons c = List.map (fun y -> c::y)  in
  let rec mapConsSet alphabet l = 
    List.fold_right (fun c -> List.append (mapCons c l)) alphabet []  in
  let rec strings' n =
    if (n<=0) then [[]]
    else [] :: mapConsSet alphabet (strings' (n-1))
  in List.map implode (strings' n)


(* 
 *  Compute the language of a PDA, restricted to inputs of length <= n
 *   language pda n   returns a list of strings accepted by dfa
 *   printLanguage pda n   prints the strings accepted by dfa
 *
 *)

let language pda n = 
  List.filter (accept pda) (strings pda.pda_alphabet n)

let printLanguage pda n = 
  List.iter (fun s -> Printf.printf "  %s\n" (if (s="") then "<empty>" else s))
              (language pda n)


(*
 * Some sample PDAs
 *
 *)


let anban = { pda_states = ["q1"; "q2"; "q3"];
              pda_alphabet = ['a';'b'];
              pda_delta = [ ("q1", Some 'a', '_', "q1", ['.'; '_']);
			    ("q1", Some 'a', '.', "q1", ['.'; '.']);
			    ("q1", Some 'b', '_', "q2", ['_']);
			    ("q1", Some 'b', '.', "q2", ['.']);
			    ("q2", Some 'a', '.', "q2", []);
			    ("q2", None, '_', "q3", ['_'])];
	      pda_start = "q1";
	      pda_final = ["q3"];
	      pda_stack_alph = ['.'; '_'];
	      pda_bottom = '_' }

let anbn = { pda_states = ["q1"; "q2"; "q3"];
             pda_alphabet = ['a';'b'];
             pda_delta = [ ("q1", Some 'a', '_', "q1", ['.'; '_']);
			   ("q1", Some 'a', '.', "q1", ['.'; '.']);
			   ("q1", None, '_', "q2", ['_']);
			   ("q1", None, '.', "q2", ['.']);
			   ("q2", Some 'b', '.', "q2", []);
			   ("q2", None, '_', "q3", ['_'])];
	     pda_start = "q1";
	     pda_final = ["q3"];
	     pda_stack_alph = ['.'; '_'];
	     pda_bottom = '_' }

let ambn = { pda_states = ["q1"; "q2"; "q3"];
             pda_alphabet = ['a';'b'];
             pda_delta = [ ("q1", Some 'a', 'X', "q1", ['.'; 'X']);
			   ("q1", Some 'a', '.', "q1", ['.'; '.']);
			   ("q1", None, 'X', "q2", ['X']);
			   ("q1", None, '.', "q2", ['.']);
			   ("q2", Some 'b', '.', "q2", [])]; 
	     pda_start = "q1";
	     pda_final = ["q2"];
	     pda_stack_alph = ['.'; 'X'];
	     pda_bottom = 'X' }

