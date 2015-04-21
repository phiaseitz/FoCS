(* FoCS Spring 2015

   Homework 8 code


   Name: Sophia Seitz

   Email: Sophia.Seitz@students.olin.edu

   Comments: I used the following references for this homework:
   http://caml.inria.fr/pub/docs/manual-caml-light/node14.17.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Char.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html

 *)



(* 
 *   explode_str : string -> string list
 *      returns the list of characters making up a string
 *      (where each character gets returned as a string)
 *
 *)
#load "str.cma";;

let explode_str (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.sub str index 1)::result)
  in
    acc(String.length(str)-1, [])




(*
 *   Type for deterministic Turing machines
 *
 *   Parameterized by type for states
 *)

type direction = Left | Right | Stay

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
 *   Run a Turing machine on an input word
 *)

let run m w = 

  let print_config m (u,q,v) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_newline ()  in
    value  in

  let starting_config = 
    ([], m.tm_start, m.tm_leftmost::(explode_str w))  in

  let accepting_config m (u,q,v) = (q=m.tm_accept)  in

  let rejecting_config m (u,q,v) = (q=m.tm_reject)  in

  let halting_config m c = (accepting_config m c) || (rejecting_config m c)  in

  let step_config m (u,q,v) = 

    let rec find_match (q,a) delta = 
      match delta with
        [] -> failwith "No transition defined!"
      | ((q_,sym_,p_,rewrite_,dir_)::_) when q=q_ && a=sym_ -> (p_,rewrite_,dir_)
      | _::delta' -> find_match (q,a) delta'  in

    let rec last u = 
      match u with
        [] -> failwith "Moving Left from leftmost tape position"
      | [a] -> ([],a)
      | x::xs -> let (u',r) = last xs  in (x::u',r)   in

    if (halting_config m (u,q,v)) 
      then (u,q,v)
    else let (a,v') = match v with
                      | [] -> (m.tm_blank,[])
                      | a::v' -> (a,v')  in
         let (q',b,dir) = find_match (q,a) m.tm_delta  in
         match dir with
         | Left -> let (u',c) = last u in (u',q',c::b::v')
         | Right -> (u@[b],q',v')
         | Stay -> (u,q',b::v')  in

  let rec loop c = 
    let _ = print_config m c c in
    if  (accepting_config m c) then true
    else if (rejecting_config m c) then false
    else loop (step_config m c)  in

  loop starting_config



(* 
 *
 * QUESTION 1
 *
 *)


let make_delta states alph f = List.fold_right (fun a delta -> 
  (List.fold_right (fun state stateDelt ->
    match (f (state,a)) with (newState, write, dir) -> 
    (state,a,newState,write,dir)::stateDelt) states [])@delta) alph [];;
  
let transform_states t states = List.rev(List.rev_map(fun state -> (t state)) states);;

(* List.rev(List.) (fun state newStates ->
  (t state)::newStates) states [] *)



(* let transform_delta t delta = List.map (fun oneDelta ->
  match oneDelta with (s1,input,s2,write,dir) -> 
    ((t s1), input, (t s2), write, dir)) delta;; *)

let transform_delta t delta = List.rev(List.rev_map (fun o_delta -> 
  match o_delta with (s1,input,s2,write,dir) ->
   ((t s1), input, (t s2), write, dir)) delta);;


let transform t tm = 
  { tm_states = (transform_states t tm.tm_states);
    tm_input_alph = tm.tm_input_alph;
    tm_tape_alph = tm.tm_tape_alph;
    tm_blank = tm.tm_blank;
    tm_leftmost = tm.tm_leftmost;
    tm_start = List.hd(transform_states t [tm.tm_start]);
    tm_accept = List.hd(transform_states t [tm.tm_accept]);
    tm_reject = List.hd(transform_states t [tm.tm_reject]);
    tm_delta = transform_delta t tm.tm_delta}




(* 
 * Some sample deterministic Turing machines
 *
 * anbn is the context-free language {a^n b^n | n >= 0}
 * anbncn is the non-context-free language {a^n b^n c^n | n >= 0}
 *
 *)


let anbn = 
  let states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"]  in
  let tape_alph = ["a";"b";"X";"_";">"]  in
  { tm_states = states;
    tm_input_alph = ["a";"b"];
    tm_tape_alph = tape_alph;
    tm_blank = "_";
    tm_leftmost = ">";
    tm_start = "start";
    tm_accept = "acc";
    tm_reject = "rej";
    tm_delta = make_delta states tape_alph
      (fun (q,a) -> 
        match (q,a) with
        | ("start", "a") -> ("start", "a", Right)
        | ("start", "b") -> ("q1", "b", Right)
        | ("start", ">") -> ("start", ">", Right)
        | ("start", "_") -> ("q2", "_", Right)
        | ("q1", "b") -> ("q1", "b", Right)
        | ("q1", "_") -> ("q2", "_", Right)
        | ("q2", ">") -> ("q3", ">", Right)
        | ("q2", sym) -> ("q2", sym, Left)
        | ("q3", "X") -> ("q3", "X", Right)
        | ("q3", "_") -> ("acc", "_", Right)
        | ("q3", "a") -> ("q4", "X", Right)
        | ("q4", "a") -> ("q4", "a", Right)
        | ("q4", "X") -> ("q4", "X", Right)
        | ("q4", "b") -> ("q2", "X", Right)
        | ("acc", sym) -> ("acc", sym, Right)
        | (_, sym) -> ("rej", sym, Right)) }


let anbncn = 
  let states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"] in
  let tape_alph = ["a";"b";"c";"X";"_";">"] in
  { tm_states = states;
    tm_input_alph = ["a";"b";"c"];
    tm_tape_alph = tape_alph;
    tm_blank = "_";
    tm_leftmost = ">";
    tm_start = "start";
    tm_accept = "acc";
    tm_reject = "rej";
    tm_delta = make_delta states tape_alph
      (fun (q,a) -> 
        match (q,a) with
        | ("start", "a") -> ("start", "a", Right)
        | ("start", "b") -> ("q1", "b", Right)
        | ("start", "c") -> ("q6", "c", Right)
        | ("start", ">") -> ("start", ">", Right)
        | ("start", "_") -> ("q2", "_", Right)
        | ("q1", "b") -> ("q1", "b", Right)
        | ("q1", "c") -> ("q6", "c", Right)
        | ("q1", "_") -> ("q2", "_", Right)
        | ("q2", ">") -> ("q3", ">", Right)
        | ("q2", sym) -> ("q2", sym, Left)
        | ("q3", "X") -> ("q3", "X", Right)
        | ("q3", "_") -> ("acc", "_", Right)
        | ("q3", "a") -> ("q4", "X", Right)
        | ("q4", "a") -> ("q4", "a", Right)
        | ("q4", "X") -> ("q4", "X", Right)
        | ("q4", "b") -> ("q5", "X", Right)
        | ("q5", "b") -> ("q5", "b", Right)
        | ("q5", "X") -> ("q5", "X", Right)
        | ("q5", "c") -> ("q2", "X", Right)
        | ("q6", "c") -> ("q6", "c", Right)
        | ("q6", "_") -> ("q2", "_", Right)
        | ("acc", sym) -> ("acc", sym, Right)
        | (_, sym) -> ("rej", sym, Right) )}


(* see write up *)

let evenOddSequence = 
  let delta (p,a) = 
    match p,a with
    | "even", ">" -> ("even", ">", Right)
    | "even", "0" -> ("odd", "0", Right)
    | "even", "1" -> ("even", "1", Right)
    | "even", "_" -> ("q1/0", "_", Left)
          
    | "odd", "0" -> ("even", "0", Right)
    | "odd", "1" -> ("odd", "1", Right)
    | "odd", "_" -> ("q1/1", "_", Left)
          
    | "q1/0", ">" -> ("acc", ">", Right)
    | "q1/0", "0" -> ("q2/0", "0", Left)
    | "q1/0", "1" -> ("q1/0", "1", Left)
          
    | "q2/0", ">" -> ("acc", ">", Right)
    | "q2/0", "0" -> ("q3/0", "0", Left)
    | "q2/0", "1" -> ("q1/0", "1", Left)
          
    | "q3/0", ">" -> ("acc", ">", Right)
    | "q3/0", "1" -> ("q1/0", "1", Left)
          
    | "q1/1", ">" -> ("acc", ">", Right)
    | "q1/1", "0" -> ("q1/1", "0", Left)
    | "q1/1", "1" -> ("q2/1", "1", Left)
          
    | "q2/1", ">" -> ("acc", ">", Right)
    | "q2/1", "0" -> ("q1/1", "0", Left)
    | "q2/1", "1" -> ("q3/1", "1", Left)
          
    | "q3/1", ">" -> ("acc", ">", Right)
    | "q3/1", "0" -> ("q1/1", "0", Left)
          
    | "acc", sym -> ("acc", sym, Right)
    | _, sym -> ("rej", sym, Right)  in
  let states = [ "even";"odd";"q1/0";"q2/0";"q3/0";"q1/1";"q2/1";"q3/1";
                 "acc";"rej"]  in
  let alph = ["0";"1";"_";">"]  in
  { tm_states = states ;
    tm_input_alph = ["0";"1"];
    tm_tape_alph = alph;
    tm_leftmost = ">";
    tm_blank = "_";
    tm_delta = make_delta states alph delta;
    tm_start = "even";
    tm_accept = "acc";
    tm_reject = "rej" }



(* A version of the same Turing machine but with
   structured states 
 *)

type even_odd_sequence_state = 
  | Simple of string
  | BitTag of string * string

let evenOddSequence_struct = 
  let delta (p,a) = 
    match p,a with
    | Simple("even"), ">" -> (Simple("even"), ">", Right)
    | Simple("even"), "0" -> (Simple("odd"), "0", Right)
    | Simple("even"), "1" -> (Simple("even"), "1", Right)
    | Simple("even"), "_" -> (BitTag("q1", "0"), "_", Left)
          
    | Simple("odd"), "0" -> (Simple("even"), "0", Right)
    | Simple("odd"), "1" -> (Simple("odd"), "1", Right)
    | Simple("odd"), "_" -> (BitTag("q1", "1"), "_", Left)
          
    | BitTag("q1", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q1", t), sym when t = sym -> (BitTag("q2", t), sym, Left)
    | BitTag("q1", t), sym -> (BitTag("q1", t), sym, Left)
          
    | BitTag("q2", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q2", t), sym when t = sym -> (BitTag("q3", t), sym, Left)
    | BitTag("q2", t), sym -> (BitTag("q1", t), sym, Left)
          
    | BitTag("q3", t), ">" -> (Simple("acc"), ">", Right)
    | BitTag("q3", t), sym when t <> sym -> (BitTag("q1", t), sym, Left)
          
    | Simple("acc"), sym -> (Simple("acc"), sym, Right)
    | _, sym -> (Simple("rej"), sym, Right)  in
  let states = [ Simple("even"); Simple("odd"); Simple("acc"); Simple("rej");
                 BitTag("q1", "0"); BitTag("q2", "0"); BitTag("q3", "0"); 
                 BitTag("q1", "1"); BitTag("q2", "1"); BitTag("q3", "1") ]  in
  let alph = ["0"; "1"; ">"; "_"]  in
  let string_of st = 
    match st with
    | Simple(s) -> s
    | BitTag(s,b) -> s^"|"^b  in
  transform string_of
    { tm_states = states;
      tm_input_alph = ["0"; "1"];
      tm_tape_alph = alph;
      tm_leftmost = ">";
      tm_blank = "_";
      tm_delta = make_delta states alph delta;
      tm_start = Simple("even");
      tm_accept = Simple("acc");
      tm_reject = Simple("rej") }




(* 
 *
 * QUESTION 2
 *
 *)


(* some helper definitions *)

let digits = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let isDigit c = List.mem c digits

let triple =
  let delta (p,a) = 
    match p,a with 
      Simple("start"), ">" -> (Simple("d1"), ">", Right)

      | Simple("d1"), d when isDigit d -> (Simple("u1"), d, Right)

      | Simple("u1"), "#" -> (Simple("d2"), "#", Right)
      | Simple("u1"), d when isDigit d -> (Simple("u1"), d, Right)

      | Simple("d2"), d when isDigit d -> (Simple("u2"), d, Right)

      | Simple("u2"), "#" -> (Simple("d3"), "#", Right)
      | Simple("u2"), d when isDigit d -> (Simple("u2"), d, Right)

      | Simple("d3"), d when isDigit d -> (Simple("u3"), d, Right)
 
      | Simple("u3"), "_" -> (Simple("rew"), "_", Left)
      | Simple("u3"), d when isDigit d -> (Simple("u3"), d, Right)

      | Simple("rew"), ">" -> (Simple("x1"), ">", Right)
      | Simple("rew"), sym -> (Simple("rew"), sym, Left)

      | Simple("x1"), "#" -> (Simple("checkacc"), "#", Right)
      | Simple("x1"), "X" -> (Simple("x1"), "X", Right)
      | Simple("x1"), d when isDigit d -> (BitTag("tou2",d), "X", Right)

      | BitTag("tou2", t), "#" -> (BitTag("x2", t), "#", Right)
      | BitTag("tou2", t), d when isDigit d -> (BitTag("tou2", t), d, Right)

      | BitTag("x2", t), "X" -> (BitTag("x2", t), "X", Right)
      | BitTag("x2", t), di when di = t -> (BitTag("tou3", t), "X", Right)

      | BitTag("tou3", t), "#" -> (BitTag("x3", t), "#", Right)
      | BitTag("tou3", t), d when isDigit d -> (BitTag("tou3", t), d, Right)

      | BitTag("x3", t), "X" -> (BitTag("x3", t), "X", Right)
      | BitTag("x3", t), di when di = t -> (Simple("rew"), "X", Left)

      | Simple("checkacc"), "#" -> (Simple("checkacc"), "#", Right)
      | Simple("checkacc"), "X" -> (Simple("checkacc"), "X", Right)
      | Simple("checkacc"), "_" -> (Simple("acc"), "_", Right)

      | Simple("acc"), sym -> (Simple("acc"), sym, Right)

      | _, sym -> (Simple("rej"), sym, Right) in

    let states = [Simple("start"); Simple("d1"); Simple("u1"); Simple("d2"); 
      Simple("u2"); Simple("d3"); Simple("u3"); Simple("rew"); Simple("x1"); 
      BitTag("tou2", "0"); BitTag("tou2", "1"); BitTag("tou2", "2"); 
      BitTag("tou2", "3"); BitTag("tou2", "4"); BitTag("tou2", "5"); 
      BitTag("tou2", "6"); BitTag("tou2", "7"); BitTag("tou2", "8"); 
      BitTag("tou2", "9"); BitTag("x2", "0"); BitTag("x2", "1"); 
      BitTag("x2", "2"); BitTag("x2", "3"); BitTag("x2", "4"); BitTag("x2", "5"); 
      BitTag("x2", "6"); BitTag("x2", "7"); BitTag("x2", "8"); BitTag("x2", "9"); 
      BitTag("tou3", "0"); BitTag("tou3", "1"); BitTag("tou3", "2"); 
      BitTag("tou3", "3"); BitTag("tou3", "4"); BitTag("tou3", "5"); 
      BitTag("tou3", "6"); BitTag("tou3", "7"); BitTag("tou3", "8"); 
      BitTag("tou3", "9"); BitTag("x3", "0"); BitTag("x3", "1"); 
      BitTag("x3", "2"); BitTag("x3", "3"); BitTag("x3", "4"); BitTag("x3", "5");
      BitTag("x3", "6"); BitTag("x3", "7"); BitTag("x3", "8"); BitTag("x3", "9");
      Simple("checkacc"); Simple("acc"); Simple("rej")] in

    let alph = [">";"_";"#";"X"] @ digits in

    let string_of st = 
    match st with
    | Simple(s) -> s
    | BitTag(s,b) -> s^"|"^b  in
    transform string_of
     { tm_states = states;
       tm_input_alph = "#"::digits;
       tm_tape_alph = alph;
       tm_leftmost = ">";
       tm_blank = "_";
       tm_delta = make_delta states alph delta;
       tm_start = Simple("start");
       tm_accept = Simple("acc");
       tm_reject = Simple("rej") };;
   



(* 
 *
 * QUESTION 3
 *
 *)



(*
 * Two-tape Turing machines
 *
 *)

type 'a tm2 = { tm2_states : 'a list;
                tm2_input_alph : symbol list;
                tm2_tape_alph : symbol list;
                tm2_leftmost : symbol;
                tm2_blank : symbol;
                tm2_delta : ('a * symbol * symbol * 'a * symbol * symbol * direction * direction) list;
                tm2_start : 'a;
                tm2_accept : 'a;
                tm2_reject : 'a }


(* 
 *     A sample two-tape TM that decides {u#u | u a string}
 *)

let pair = 
  let foreach l f = List.flatten (List.map f l)  in
  let states = [ "1";"2";"3";"4";"5";"6";"acc";"rej" ]  in
  let alph = ["0";"1";"#";"_";">"]  in
  let default = foreach states 
                  (fun q -> foreach alph 
                              (fun sym1 -> foreach alph
                                             (fun sym2 -> [(q,sym1,sym2,"rej",sym1,sym2,Stay,Stay)])))  in
  { tm2_states = states;
    tm2_input_alph = [ "0";"1";"#" ];
    tm2_tape_alph = alph;
    tm2_leftmost = ">";
    tm2_blank = "_";
    tm2_delta = [ ("1",">",">","2",">",">",Right,Right);
                  ("2","0","_","2","0","_",Right,Stay);
                  ("2","1","_","2","1","_",Right,Stay);
                  ("2","#","_","3","_","_",Right,Stay);
                  ("3","0","_","3","_","0",Right,Right);
                  ("3","1","_","3","_","1",Right,Right);
                  ("3","_","_","4","_","_",Left,Stay);
                  ("4","_","_","4","_","_",Left,Stay);
                  ("4","0","_","4","0","_",Left,Stay);
                  ("4","1","_","4","1","_",Left,Stay);
                  ("4",">","_","5",">","_",Stay,Stay);
                  ("5",">","_","5",">","_",Stay,Left);
                  ("5",">","0","5",">","0",Stay,Left);
                  ("5",">","1","5",">","1",Stay,Left);
                  ("5",">",">","6",">",">",Right,Right);
                  ("6","0","0","6","0","0",Right,Right);
                  ("6","1","1","6","1","1",Right,Right);
                  ("6","_","_","acc","_","_",Stay,Stay)
                ] @ default;
    tm2_start = "1";
    tm2_accept = "acc";
    tm2_reject = "rej" }



(* 
 *   Some code to run a two-tape TM on an input word
 *)

let run2 m w = 

  let print_config m (u,q,v,u2,v2) value = 
    let print_syms = List.iter (Printf.printf "%s ")  in
    let _ = print_string "  "  in
    let _ = print_syms u  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v  in
    let _ = print_string " | "  in
    let _ = print_syms u2  in
    let _ = Printf.printf "(%s) " q  in
    let _ = print_syms v2  in
    let _ = print_newline ()  in
    value   in

  let accepting_config m (u,q,v,_,_) = (q=m.tm2_accept)  in

  let rejecting_config m (u,q,v,_,_) = (q=m.tm2_reject)  in

  let halting_config m c = (accepting_config m c) || (rejecting_config m c)  in

  let step_config m (u,q,v,u2,v2) = 

    let rec find_match (q,a,b) delta = 
      match delta with
        [] -> failwith "No transition defined!"
      | ((q_,sym1_,sym2_,p_,rewrite1_,rewrite2_,dir1_,dir2_)::_) 
              when q=q_ && a=sym1_ && b=sym2_ -> (p_,rewrite1_,rewrite2_,dir1_,dir2_)
      | _::delta' -> find_match (q,a,b) delta'  in

    let rec last u = 
      match u with
        [] -> failwith "Moving Left from leftmost tape position"
      | [a] -> ([],a)
      | x::xs -> let (u',r) = last xs  in (x::u',r)   in

    if (halting_config m (u,q,v,u2,v2)) 
      then (u,q,v,u2,v2)
    else let (a,v') = match v with
                      | [] -> (m.tm2_blank,[])
                      | a::v' -> (a,v')  in
         let (a2,v2') = match v2 with
                      | [] -> (m.tm2_blank,[])
                      | a2::v2' -> (a2,v2')  in
         let (q',b,b2,dir,dir2) = find_match (q,a,a2) m.tm2_delta  in
         match dir,dir2 with
         | Left,Left -> let (u',c) = last u in 
                        let (u2',c2) = last u2 in (u',q',c::b::v',u2',c2::b2::v2')
         | Left,Right -> let (u',c) = last u in (u',q',c::b::v',u2@[b2],v2')
         | Left,Stay -> let (u',c) = last u in (u',q',c::b::v',u2,b2::v2')
         | Right,Left -> let (u2',c2) = last u2 in (u@[b],q',v',u2',c2::b2::v2')
         | Right,Right -> (u@[b],q',v',u2@[b2],v2')
         | Right,Stay -> (u@[b],q',v',u2,b2::v2')
         | Stay,Left -> let (u2',c2) = last u2 in (u,q',b::v',u2',c2::b2::v2')
         | Stay,Right -> (u,q',b::v',u2@[b2],v2')
         | Stay,Stay -> (u,q',b::v',u2,b2::v2')   in

  let starting_config = 
    ([], m.tm2_start, m.tm2_leftmost::(explode_str w),[],[m.tm2_leftmost])  in

  let rec loop c = 
    let _ = print_config m c c in
    if  (accepting_config m c) then true
    else if (rejecting_config m c) then false
    else loop (step_config m c)  in

  loop starting_config


type sim_state =
  NoTag of string
  | TagState of string * string
  | TagStateH1 of string * string * string
  | TagStateH12 of string * string * string * string
  | TagStatePush12 of string * string *string * string * string * string
  | TagStatePush2 of string * string * string * string * string

let simulate2 tm2 = 
  let dir_to_string dir = match dir with
    Left -> "l"
    | Right -> "r"
    | Stay -> "s" in

  let string_to_dir s = match s with
    "l" -> Left 
    | "r" -> Right in 

  let get_new_tag (q, a, b) = match (List.find(fun delta2 -> match delta2 with 
    (qin, ain, bin, _,_, _, _, _) -> if ((q = qin) && (a = ain) && (b = bin)) 
    then true else false) tm2.tm2_delta) with 

      (_,_,_,q',apush, bpush, adir, bdir) -> TagStatePush12("rew", q', apush, bpush, (dir_to_string adir), (dir_to_string bdir))
    in 

  let delta (p,a) = match p,a with
    NoTag("start"), ">" -> 
      (TagState("stacktapes","^>|^>"), ">", Right)

    | TagState("stacktapes", tag),"*_|*_" -> 
      (TagState("rew", pair.tm2_start), tag, Left) 
    | TagState("stacktapes", tag),sym -> 
      (TagState("stacktapes","*"^sym^"|*_"), tag, Right)

    | TagState("rew", tm2state), sym when tm2state = pair.tm2_accept -> 
      (NoTag("acc"), sym, Right)
    | TagStateH1("rew", tm2state, h1sym), sym when tm2state = pair.tm2_accept -> 
      (NoTag("acc"), sym, Right)
    | TagStatePush12("rew", tm2state, h1sym, h2sym, dir1, dir2), sym when tm2state = pair.tm2_accept -> 
      (NoTag("acc"), sym, Right) 
    | TagStatePush2("rew", tm2state, h1sym, h2sym, dir2), sym when tm2state = pair.tm2_accept -> 
      (NoTag("acc"), sym, Right)      
    | TagState("rew", tm2state), sym when tm2state = pair.tm2_reject -> 
      (NoTag("rej"), sym, Right)
    | TagStateH1("rew", tm2state, h1sym), sym when tm2state = pair.tm2_reject -> 
      (NoTag("rej"), sym, Right)
    | TagStatePush12("rew", tm2state, h1sym, h2sym, dir1, dir2), sym when tm2state = pair.tm2_reject -> 
      (NoTag("rej"), sym, Right) 
    | TagStatePush2("rew", tm2state, h1sym, h2sym, dir2), sym when tm2state = pair.tm2_reject -> 
      (NoTag("rej"), sym, Right)
    | TagState("rew", tm2state), ">" -> 
      (TagState("scan1", tm2state), ">", Right)
    | TagStateH1("rew", tm2state, h1sym), ">" -> 
      (TagStateH1("scan2", tm2state, h1sym), ">", Right)
    | TagStatePush12 ("rew", tm2state, h1sym, h2sym, dir1, dir2), ">" -> 
      (TagStatePush12("scan1",tm2state, h1sym, h2sym, dir1, dir2), ">", Right) 
    | TagStatePush2 ("rew", tm2state, h1sym, h2sym, dir2), ">" -> 
      (TagStatePush2("scan2", tm2state, h1sym, h2sym, dir2), ">", Right) 
    | TagState("rew", tm2state), sym ->  
      (TagState("rew", tm2state), sym, Left) 
    | TagStateH1("rew", tm2state, h1sym), sym -> 
      (TagStateH1("rew", tm2state, h1sym), sym, Left)
    | TagStatePush12 ("rew", tm2state, h1sym, h2sym, dir1, dir2), sym -> 
      (TagStatePush12 ("rew", tm2state, h1sym, h2sym, dir1, dir2), sym, Left)
    | TagStatePush2 ("rew", tm2state, h1sym, h2sym, dir2), sym -> 
      (TagStatePush2 ("rew", tm2state, h1sym, h2sym, dir2), sym, Left)

    | TagState("scan1", tm2state), symstack when ((String.length symstack) < 5) -> 
      (NoTag("rej"), symstack, Right) 
    | TagStatePush12("scan1", tm2state, h1sym, h2sym, dir1, dir2), symstack when ((String.length symstack) < 5) -> 
      (NoTag("rej"), symstack, Right)
    | TagState("scan1",tm2state), symstack when (symstack.[0] = '^') -> 
      (TagStateH1("rew", tm2state, (Char.escaped symstack.[1])), symstack, Left)
    | TagState("scan1",tm2state), symstack when (symstack.[0] = '*') -> 
      (TagState("scan1", tm2state), symstack, Right)
    | TagStatePush12("scan1", tm2state, h1sym, h2sym, dir1, dir2), symstack when ((symstack.[0] = '^') && (dir1 = "s")) -> 
      (TagStatePush2("rew", tm2state, h1sym, h2sym, dir2), ((Str.string_before symstack 1)^h1sym^(Str.string_after symstack 2)), Left)
    | TagStatePush12("scan1", tm2state, h1sym, h2sym, dir1, dir2), symstack when (symstack.[0] = '^') -> 
      (TagStatePush2("sethead1", tm2state, h1sym, h2sym, dir2), ("*"^h1sym^(Str.string_after symstack 2)), (string_to_dir dir1))
    | TagStatePush12("scan1", tm2state, h1sym, h2sym, dir1, dir2), symstack when (symstack.[0] = '*')  -> 
      (TagStatePush12("scan1", tm2state, h1sym, h2sym, dir1, dir2), symstack, Right) 

    | TagStateH1("scan2", tm2state, h1sym), symstack when ((String.length symstack) < 5) -> 
      (NoTag("rej"), symstack, Right)
    | TagStatePush2("scan2", tm2state, h1sym, h2sym, dir2), symstack when ((String.length symstack) < 5) -> 
      (NoTag("rej"), symstack, Right)
    | TagStateH1("scan2", tm2state, h1sym), symstack when (symstack.[3] = '^') -> 
      (TagStateH12("transition",tm2state, h1sym, (Char.escaped symstack.[4])), symstack, Right)
    | TagStateH1("scan2", tm2state, h1sym), symstack when (symstack.[3] = '*') -> 
      (TagStateH1("transition",tm2state, h1sym), symstack, Right)
    | TagStatePush2("scan2", tm2state, h1sym, h2sym, dir2), symstack when ((symstack.[3] = '^') && (dir2 = "s"))-> 
      (TagStateH12("transition", tm2state, h1sym, h2sym), ((Str.string_before symstack 4)^h2sym), Left)
    | TagStatePush2("scan2", tm2state, h1sym, h2sym, dir2), symstack when (symstack.[3] = '^') -> (TagStatePush2("sethead2", tm2state, h1sym, h2sym, dir2), 
      ((Str.string_before symstack 3)^"*"^h2sym), (string_to_dir dir2))
    | TagStatePush2("scan2", tm2state, h1sym, h2sym, dir2), symstack when (symstack.[3] = '*') -> (TagStatePush2("scan2", tm2state, h1sym, h2sym, dir2), 
      symstack, Right) 

    | TagStateH12("transition", tm2state, h1sym, h2sym), symstack when ((String.length symstack) = 5) -> ((get_new_tag (tm2state,h1sym,h2sym)) , symstack, Left)

    | TagStatePush2("sethead1", tm2state, h1sym, h2sym, dir2), symstack when ((String.length symstack) = 5) -> 
      (TagStatePush2("rew",tm2state, (Char.escaped symstack.[1]), h2sym, dir2), ("^"^(Str.string_after symstack 1)), Left)
    
    | TagStatePush2("sethead2", tm2state, h1sym, h2sym, dir2), symstack when ((String.length symstack) = 5) -> 
      (TagStateH12("transition",tm2state, h1sym, (Char.escaped symstack.[4])), ((Str.string_before symstack 3)^"^"^(Str.string_after symstack 4)), Right)

    | NoTag("acc"), sym -> (NoTag("acc"), sym, Right)
    | _, sym -> (NoTag("rej"), sym, Right) in
  let make_tiles_alph tape_alph = 
    List.fold_right(fun tape1 acc -> 
      (List.fold_right(fun tape1pre acc' -> 
        (List.fold_right (fun tape2 acc'' -> 
          (List.fold_right (fun tape2pre tapealph -> (tape1pre^tape1^"|"^tape2pre^tape2)::tapealph) 
          ["^"; "*"] [])@acc'')
        tape_alph [])@acc') 
      ["^"; "*"] [])@acc)
    tape_alph [] in 

  let string_of st = match st with 
    NoTag(s) -> s
    | TagState(s,t1) -> s^"-"^t1
    | TagStateH1(s,t1,t2) -> s^"-"^t1^"-"^t2 
    | TagStateH12(s,t1,t2,t3) -> s^"-"^t1^"-"^t2^"-"^t3
    | TagStatePush2(s,t1,p1,p2,d2) -> s^"-"^t1^"-"^p1^"-"^p2^"-"^d2
    | TagStatePush12(s,t1,p1,d1,p2,d2) -> s^"-"^t1^"-"^p1^"-"^d1^"-"^p2^"-"^d2 in 

  let make_no_tag_states base_states = List.map(fun state -> NoTag(state)) base_states in

  let make_tag_states base_states tm2states = 
    List.fold_right (fun bstat acc -> 
      (List.fold_right (fun tm2s tags -> TagState(bstat,tm2s)::tags) 
      tm2states []) @ acc)
    base_states [] in

  let make_tagH12_states base_states tm2states tm2alph = 
    List.fold_right(fun base acc -> 
      (List.fold_right (fun tm2s acc' -> 
        (List.fold_right(fun h1 acc'' ->
          (List.fold_right (fun h2 statesl -> TagStateH12(base, tm2s, h1, h2)::statesl) 
          tm2alph [])@acc'')
        tm2alph [])@acc')
      tm2states [])@ acc)
    base_states [] in

  let make_tagH1_states base_states tm2states tm2alph = 
    List.fold_right(fun base acc -> 
      (List.fold_right (fun tm2s acc' -> 
        (List.fold_right(fun h1 statesl -> TagStateH1(base, tm2s, h1)::statesl) 
        tm2alph [])@acc')
      tm2states [])@ acc)
    base_states [] in

  let make_push12_states base_states tm2states tm2alph =
    List.fold_right(fun base acc -> 
      (List.fold_right (fun tm2s acc' -> 
        (List.fold_right(fun h1 acc'' ->
          (List.fold_right (fun h2 acc''' -> 
            (List.fold_right (fun d1 acc'''' -> (
              List.fold_right (fun d2 statesl -> (TagStatePush12(base, tm2s, h1, h2, d1, d2))::statesl)
              ["l"; "r"; "s"] [])@acc'''')
            ["l"; "r"; "s"] [])@acc''')
          tm2alph [])@acc'')
        tm2alph [])@acc')
      tm2states [])@ acc)
    base_states [] in

  let make_push2_states base_states tm2states tm2alph = 
    List.fold_right(fun base acc -> 
      (List.fold_right (fun tm2s acc' -> 
        (List.fold_right(fun h1 acc'' ->
          (List.fold_right (fun h2 acc''' -> 
            (List.fold_right (fun d2 statesl -> (TagStatePush2(base, tm2s, h1, h2, d2))::statesl)
            ["l"; "r"; "s"] [])@acc''')
          tm2alph [])@acc'')
        tm2alph [])@acc')
      tm2states [])@ acc)
    base_states [] in

  let make_stack_tapes_states tm2alph = 
    List.map(fun alph-> 
      if alph = ">" then TagState("stacktapes", "^"^alph^"|^>")
      else TagState("stacktapes", "*"^alph^"|*_")) ("*_|*_"::tm2alph) in

  let make_states tm2states tm2alph = 
    (make_no_tag_states ["start"; "acc"; "rej"])
    @ (make_tag_states ["rew"; "scan1"] tm2alph) 
    @ (make_tagH1_states ["rew";"scan2"] tm2states tm2alph) 
    @ (make_tagH12_states ["transition"] tm2states tm2alph) 
    @ (make_push12_states ["rew"; "scan1"] tm2states tm2alph) 
    @ (make_push2_states ["rew"; "scan2"; "sethead2";"sethead1"] tm2states tm2alph) 
    @ (make_stack_tapes_states tm2alph) in

  let states = make_states tm2.tm2_states tm2.tm2_tape_alph in

  let alph = (make_tiles_alph tm2.tm2_tape_alph)@tm2.tm2_tape_alph in

  transform string_of
     { tm_states = states;
       tm_input_alph = tm2.tm2_input_alph;
       tm_tape_alph = alph;
       tm_leftmost = ">";
       tm_blank = "*_|*_";
       tm_delta = (make_delta states alph delta);
       tm_start = NoTag("start");
       tm_accept = NoTag("acc");
       tm_reject = NoTag("rej") };;

  




