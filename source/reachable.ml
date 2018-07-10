(*

SAHA-Tool: a program for computing approximated solutions to the
reachability problem in semi-algebraic automata.

Copyright (C) 2008  Dario Campagna

This file is part of SAHA-Tool.

SAHA-Tool is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SAHA-Tool is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SAHA-Tool.  If not, see <http://www.gnu.org/licenses/>.

To contact the author: dario.campagna@dipmat.unipg.it

*)


open Hybrid_automata;;
open Formulae;;
open Quantifier_elimination;;


(* Ordered pair of discrete_location and cont_var  *)
module OrderedDiscLocContVarPair =
 struct
  type t = discrete_location * cont_var
  let compare t1 t2 = Pervasives.compare t1 t2
 end;;

(* Association table over OrderedDiscLocContVarPair *)
module AssocDiscLocContVarPair = Map.Make (OrderedDiscLocContVarPair);;

(* Record type with mutable field *)
type map_state_var = {m : formula AssocDiscLocContVarPair.t}

(* Exception which could be rasied during automaton check *)
exception T_Not_Adm;;
exception Inv_Not_Adm;;
exception Inv_Var_Not_Adm;;
exception Act_Not_Adm;;
exception Act_Var_Not_Adm;;
exception Res_Not_Adm;;
exception Res_Var_Not_Adm;;
exception Dyn_Not_Adm;;
exception Eq_Not_Adm;;
exception Diff_Eq_Not_Adm;;
exception Eq_Num;;


(* Functions for separation of Dyn formulae and checks on dynamics *)
(* pos_in_list: return the position of x in list l *)
let rec pos_in_list x l n =
 match l with
    y::ls -> if x = y then n else pos_in_list x ls (n + 1)
  | [] -> -1;;
(* add_n_fun: increment the value in n_equat corresponding to the variable x \in Z' *)
let add_n_equat n_equat x v_list =
 let i = pos_in_list x v_list 0 in
  n_equat.(i) <- n_equat.(i) + 1;
  n_equat;;
(* n_equat_ok: check that all values in n_equat are equal to 1 *)
let n_equat_ok n_equat =
 let n = Array.length n_equat in
 let i = ref 0 in
 let ok = ref true in
  while !ok && !i < n  do 
   if n_equat.(!i) <> 1 then (ok := false;i := !i + 1) else i := !i + 1
  done;
  !ok;;
(* sep_f: function for separation of Dyn(v) of form Z' = f(Z,T) *)
let rec sep_f (dyn_map,n_fun) z_list zp_list state dyn_formula =
 match dyn_formula with
    And (t1,t2) -> sep_f (sep_f (dyn_map,n_fun) z_list zp_list state t1) z_list zp_list state t2
  | Atomic (Eq (Var x,f)) as t when (List.mem x zp_list) && (var_in_term f (z_list@["t";"T"])) -> (AssocDiscLocContVarPair.add (state,x) t dyn_map,add_n_equat n_fun x zp_list)
  | Atomic (Eq (f,Var x)) as t when (List.mem x zp_list) && (var_in_term f (z_list@["t";"T"]))-> (AssocDiscLocContVarPair.add (state,x) t dyn_map,add_n_equat n_fun x zp_list)
  | _ -> raise Eq_Not_Adm
(* sep_dyn_f: function for parsing of Dyn(v) of form Z' = f(Z,T) *) 
let sep_dyn_f dyn z_list zp_list state dyn_map =
  let n = List.length zp_list in
  let n_fun = Array.make n 0 in
  let lexbuf = Lexing.from_string (dyn state) in
  try
   try
    let dyn_formula = Parser_Dyn_Fun.main1 Lexer_Dyn_Fun.token lexbuf in
    let (d_m,n_f) = sep_f (dyn_map,n_fun) z_list zp_list state dyn_formula in
     if (n_equat_ok n_f) then
      d_m
     else
      raise Eq_Num;
   with Failure("lexing: empty token") -> raise Dyn_Not_Adm;
  with Parsing.Parse_error -> raise Eq_Not_Adm;;
(* separate_dyns_functions: function for separation of Dyn formula of form Z' = f(Z,T) of each state, returns a (formula AssocDiscLocContVarPair.t) *)
let separate_dyns_functions st_list (StateFormula dyn) z_list zp_list = List.fold_right (sep_dyn_f dyn z_list zp_list) st_list AssocDiscLocContVarPair.empty;;

(* sep_de: function for separation of Dyn(v) of form Z' = f(Z,T) *)
let xp_x x z_list zp_list =
 let i = pos_in_list x z_list 0 in
  List.nth zp_list i;;
let f_approx x f z_list zp_list =
  Atomic (Eq (Var (xp_x x z_list zp_list),Add (Var x,Prod (TParen f,Var "t"))));;
let rec sep_de (dyn_map,n_fun) z_list zp_list state dyn_formula =
 match dyn_formula with
    And (t1,t2) -> sep_de (sep_de (dyn_map,n_fun) z_list zp_list state t1) z_list zp_list state t2
  | Atomic (Eq (DVar x,f)) when (List.mem x z_list) && (var_in_term f (z_list)) -> (AssocDiscLocContVarPair.add (state,(xp_x x z_list zp_list)) (f_approx x f z_list zp_list) dyn_map,add_n_equat n_fun x z_list)
  | _ -> raise Diff_Eq_Not_Adm
(* sep_dyn_de: function for parsing of Dyn(v) of form Z_d = f(Z) *) 
let sep_dyn_de dyn z_list zp_list state dyn_map =
  let n = List.length zp_list in
  let n_fun = Array.make n 0 in
  let lexbuf = Lexing.from_string (dyn state) in
  try
   try
    let dyn_formula = Parser_Dyn_Diff_Eqs.main1 Lexer_Dyn_Diff_Eqs.token lexbuf in
    let (d_m,n_f) = sep_de (dyn_map,n_fun) z_list zp_list state dyn_formula in
     if (n_equat_ok n_f) then
      d_m
     else
      raise Eq_Num;
   with Failure("lexing: empty token") -> raise Dyn_Not_Adm;
  with Parsing.Parse_error -> raise Diff_Eq_Not_Adm;;
(* separate_dyns_diff_eqs: function for separation of Dyn formula of form Z_d = f(Z) of each state, returns a (formula AssocDiscLocContVarPair.t) *)
let separate_dyns_diff_eqs st_list (StateFormula dyn) z_list zp_list = List.fold_right (sep_dyn_de dyn z_list zp_list) st_list AssocDiscLocContVarPair.empty;;


(* Functions for computing min-max formula conjunction *)
(* and_of_min_max_list: function for computing conjunction of min_max_list elements *)
let amml and_mm mm = 
 if and_mm = True then
  if mm = True then True else mm
 else if mm = True then and_mm else And (and_mm,mm);;
let and_of_min_max_list min_max_list =
 List.fold_left amml (List.hd min_max_list) (List.tl min_max_list);;
(* compute_min_max: function for computing min-max of zp_i *)
let compute_min_max z_list dyn_map state delta start_set_formula plus_n zp_i min_max_list =
 let dyn = AssocDiscLocContVarPair.find (state,zp_i) dyn_map in
 let string_dyn = string_of_formula (formula_subs_time dyn delta) in
 let min_max_zp_i = qepcad_min_max zp_i z_list string_dyn start_set_formula plus_n in
  min_max_zp_i :: min_max_list;;
(* compute_and_min_max: function returning the conjunction of min-max formula *)
let compute_and_min_max zp_list z_list dyn_map state delta star_set_formula plus_n =
 let min_max_list = List.fold_right (compute_min_max z_list dyn_map state delta star_set_formula plus_n) zp_list [] in
  if List.mem False min_max_list then
   False
  else
   and_of_min_max_list min_max_list;;
 
(* Function for computing formula representing reached points set *)
let compute_reached z_list zp_list and_min_max inv plus_n =
 let string_and_min_max = string_of_formula (formula_subs_var and_min_max zp_list (Array.of_list z_list)) in
  if string_and_min_max = "TRUE" then
   qepcad_reached z_list "" inv plus_n
  else
   qepcad_reached z_list string_and_min_max inv plus_n;;
  
(* Function for determining if we can jump from v to u *)
let compute_discrete_jump z_list zp_list reached_v act_e res_e inv_u plus_n =
 let string_reached_v = ref (string_of_formula reached_v) in
 if !string_reached_v = "TRUE" then string_reached_v := "";
 let qe_act_e = qepcad_act z_list !string_reached_v act_e plus_n in
  if qe_act_e = False then False
  else
   let string_qe_act_e = ref (string_of_formula qe_act_e) in
   if !string_qe_act_e = "TRUE" then string_qe_act_e := "";
   let qe_res_e = qepcad_res zp_list z_list !string_qe_act_e res_e plus_n in
   let string_qe_res_e = ref (string_of_formula (formula_subs_var qe_res_e zp_list (Array.of_list z_list))) in
    if !string_qe_res_e = "TRUE" then string_qe_res_e := ""; 
    qepcad_reached z_list !string_qe_res_e inv_u plus_n;;
    
(* float_delta: function for computing float number represented by delta *)
let float_delta delta =
 match delta with
       (Int n) -> float_of_string n
     | (Rational (n,d)) -> (float_of_string n)/.(float_of_string d)
     | _ -> 0.;;

(* add_delta: function for computing d = d + delta *)
let add_delta sd1 sd2 =
 match (sd1,sd2) with
       (Int "0",_) -> sd2	
     | (Int n1, Int n2) when n1 <> "0" -> Int (string_of_int (int_of_string n1 + int_of_string n2))
     | (Rational (n1,d1),Rational (n2,d2)) when d1 = d2 -> Rational (string_of_int (int_of_string n1 + int_of_string n2),d1)
     | _ -> sd1;;

(* reachable_functions: function for determining if target_state is reachable from start_state within time time_bound in automaton with dynamics Z' = F(Z,T) *)
let reachable_functions ha dyn_map start_state start_formula target_state target_formula delta (time_bound:float) plus_n =
 let inv = take_inv ha.inv in
 let act = take_edge_formula ha.act and res = take_edge_formula ha.res in
 let target_reached = ref false and reached_set = ref "FALSE" in
 let init_formula = ref (string_of_formula (qepcad_reached ha.z start_formula (inv start_state) plus_n)) in
 if !init_formula = "TRUE" then init_formula := "";
 if !init_formula = "FALSE" then
  (
   print_string ("Empty starting set.\n");
   (!target_reached,!reached_set)
  )
 else
  (
   (*let f_delta = float_delta delta in*)
   let start_adj_states = adj_states start_state ha.ve in
   let tb = ref 0. and out_of_inv = ref false in
   let comp_queue = Queue.create () in
    Queue.add (start_state,time_bound,!init_formula,start_adj_states) comp_queue;
    while ((Queue.is_empty comp_queue = false) && (!target_reached = false)) do
     let (curr_state,t_bound,start_set_formula,curr_adj_states) = Queue.take comp_queue in
     let d = ref (Int "0") in
      tb := t_bound;
      out_of_inv := false;
      while ((!tb >= (float_delta !d)(*f_delta*)) && (!target_reached = false) && (!out_of_inv = false)) do
       let and_min_max = compute_and_min_max ha.zp ha.z dyn_map.m curr_state !d start_set_formula plus_n in
        if and_min_max <> False then
         (
          let reached = compute_reached ha.z ha.zp and_min_max (inv curr_state) plus_n in
          if reached = False then
            ((*if !d <> Int "0" then tb := !tb -. f_delta;*)
             tb := !tb -. (float_delta !d);
             out_of_inv := true)
          else
           (
            if curr_state = target_state then 
             (
              let string_reached = ref (string_of_formula reached) in
              if !string_reached = "TRUE" then string_reached := "";
              let points_reached = string_of_formula (qepcad_reached ha.z !string_reached target_formula plus_n) in
              if points_reached <> "FALSE" then (target_reached := true;reached_set := points_reached;tb := !tb -. (float_delta !d));
             );
            let n = Array.length curr_adj_states in
            let i = ref 0 in 
            while ((!i < n) && (!target_reached = false)) do
             let jump = compute_discrete_jump ha.z ha.zp reached (act curr_state curr_adj_states.(!i)) (res curr_state curr_adj_states.(!i)) (inv curr_adj_states.(!i)) plus_n in
              if jump <> False then
               (
                let string_jump = ref (string_of_formula jump) in
                if !string_jump = "TRUE" then string_jump := "";
                let points_reached = string_of_formula (qepcad_reached ha.z !string_jump target_formula plus_n) in
                if (curr_adj_states.(!i) = target_state && points_reached <> "FALSE") then
                 (target_reached := true;reached_set := points_reached;tb := !tb -. (float_delta !d))
                else
                 (
                  let i_adj_states = adj_states curr_adj_states.(!i) ha.ve in
                  if ((Array.length i_adj_states > 0) || (curr_adj_states.(!i) = target_state)) then
                   (
                    let string_jump = ref (string_of_formula jump) in
                    if !string_jump = "TRUE" then string_jump := "";
                    if !d <> Int "0" then 
                     (Queue.add (curr_adj_states.(!i),!tb -. (float_delta !d)(*f_delta*),!string_jump,i_adj_states) comp_queue)
                    else
                     (Queue.add (curr_adj_states.(!i),!tb,!string_jump,i_adj_states) comp_queue);
                    i := !i + 1
                   )
                  else
                   i := !i + 1
                 )
               )
              else
               i := !i + 1;    
            done;
            (*if !d <> Int "0" then tb := !tb -. f_delta;*)
            d := add_delta !d delta
           )
         )
        else
         ((*if !d <> Int "0" then tb := !tb -. f_delta;*)
          d := add_delta !d delta);
      done;
    done;
    print_newline ();
    if !tb > 0. then
     print_string ("Remaining time: " ^ (string_of_float !tb) ^ "\n")
    else 
     print_string ("No remaining time.\n");
    if !out_of_inv = true && !target_reached = false then print_string ("Exited from the invariant of the last examined location.\n");
    (!target_reached,!reached_set));;
    
(* reachable_diff_eqs: function for determining if target_state is reachable from start_state within time time_bound in automaton with dynamics Z_d = F(Z) *)
let reachable_diff_eqs ha dyn_map start_state start_formula target_state target_formula delta (time_bound:float) plus_n =
 let inv = take_inv ha.inv in
 let act = take_edge_formula ha.act and res = take_edge_formula ha.res in
 let target_reached = ref false and reached_set = ref "FALSE" in
 let init_formula = ref (string_of_formula (qepcad_reached ha.z start_formula (inv start_state) plus_n)) in
 if !init_formula = "TRUE" then init_formula := "";
 if !init_formula = "FALSE" then
  (
   print_string ("Empty starting set.\n");
   (!target_reached,!reached_set)
  )
 else
  (
   let f_delta = float_delta delta in
   let start_adj_states = adj_states start_state ha.ve in
   let tb = ref 0. and out_of_inv = ref false in
   let current_start_set = ref "FALSE" in
   let comp_queue = Queue.create () in
    Queue.add (start_state,time_bound,!init_formula,start_adj_states) comp_queue;
    while ((Queue.is_empty comp_queue = false) && (!target_reached = false)) do
     let (curr_state,t_bound,start_set_formula,curr_adj_states) = Queue.take comp_queue in
     let d = ref (Int "0") in
      current_start_set := start_set_formula;
      tb := t_bound;
      out_of_inv := false;
      while ((!tb >= f_delta) && (!target_reached = false) && (!out_of_inv = false)) do
       let and_min_max = compute_and_min_max ha.zp ha.z dyn_map.m curr_state !d !current_start_set plus_n in
        if and_min_max <> False then
         (
          let reached = compute_reached ha.z ha.zp and_min_max (inv curr_state) plus_n in
          if reached = False then
            (if !d <> Int "0" then tb := !tb -. f_delta;
             out_of_inv := true)
          else
           (
            current_start_set := string_of_formula reached;
            if curr_state = target_state then 
             (
              let string_reached = ref (string_of_formula reached) in
              if !string_reached = "TRUE" then string_reached := "";
              let points_reached = string_of_formula (qepcad_reached ha.z !string_reached target_formula plus_n) in
              if points_reached <> "FALSE" then (target_reached := true;reached_set := points_reached);
             ); 
            let n = Array.length curr_adj_states in
            let i = ref 0 in 
            while ((!i < n) && (!target_reached = false)) do
             let jump = compute_discrete_jump ha.z ha.zp reached (act curr_state curr_adj_states.(!i)) (res curr_state curr_adj_states.(!i)) (inv curr_adj_states.(!i)) plus_n in
              if jump <> False then
               (
                let string_jump = ref (string_of_formula jump) in
                if !string_jump = "TRUE" then string_jump := "";
                let points_reached = string_of_formula (qepcad_reached ha.z !string_jump target_formula plus_n) in
                if (curr_adj_states.(!i) = target_state && points_reached <> "FALSE") then
                 (target_reached := true;reached_set := points_reached)
                else
                 (
                  let i_adj_states = adj_states curr_adj_states.(!i) ha.ve in
                  if ((Array.length i_adj_states > 0) || (curr_adj_states.(!i) = target_state)) then
                   (
                    let string_jump = ref (string_of_formula jump) in
                    if !string_jump = "TRUE" then string_jump := "";
                    if !d <> Int "0" then 
                     (Queue.add (curr_adj_states.(!i),!tb -. f_delta,!string_jump,i_adj_states) comp_queue)
                    else
                     (Queue.add (curr_adj_states.(!i),!tb,!string_jump,i_adj_states) comp_queue);
                    i := !i + 1
                   )
                  else
                   i := !i + 1
                 )
               )
              else
               i := !i + 1;    
            done;
            if !d <> Int "0" then
             tb := !tb -. f_delta
            else
             d := delta;
           )
         )
        else
         (if !d <> Int "0" then tb := !tb -. f_delta else d := delta);
      done;
    done;
    print_newline ();
    if !tb > 0. then
     print_string ("Remaining time: " ^ (string_of_float !tb) ^ "\n")
    else 
     print_string ("No remaining time.\n");
    if !out_of_inv = true && !target_reached = false then print_string ("Exited from the invariant of the last examined location.\n");
    (!target_reached,!reached_set));;

(* check_formula: function that check syntactic correctness of invariants, activations and resets *)
let check_formulae ha states =
 let inv = take_inv ha.inv and act = take_edge_formula ha.act and res = take_edge_formula ha.res in
 let n = Array.length states and i = ref 0 in
  while !i < n do
   if (inv states.(!i) <> "") then 
    (
     let lexbuf = Lexing.from_string (inv states.(!i)) in
     try
     try
      let inv_formula = Parser_Inv_Act_Res.main1 Lexer_Inv_Act_Res.token lexbuf in
       if var_in_formula inv_formula ha.z then
        i := !i + 1
       else
        raise Inv_Var_Not_Adm;
     with Failure("lexing: empty token") -> raise Inv_Not_Adm;
     with Parsing.Parse_error -> raise Inv_Not_Adm
    )
  done;
  i := 0;
  while !i < n do
   let adj_s = adj_states states.(!i) ha.ve in
   let m = Array.length adj_s and j = ref 0 in
    while !j < m do
     if (act states.(!i) adj_s.(!j) <> "") then 
     (
      let lexbuf = Lexing.from_string (act states.(!i) adj_s.(!j)) in
      try
      try
       let act_formula = Parser_Inv_Act_Res.main1 Lexer_Inv_Act_Res.token lexbuf in
        if var_in_formula act_formula ha.z then
         j := !j + 1
        else
         raise Act_Var_Not_Adm; 
      with Failure("lexing: empty token") -> raise Act_Not_Adm;
      with Parsing.Parse_error -> raise Act_Not_Adm
     )
    done;
    j := 0;
    while !j < m do
     if (res states.(!i) adj_s.(!j) <> "") then 
     (
      let lexbuf = Lexing.from_string (res states.(!i) adj_s.(!j)) in
      try
      try
       let res_formula = Parser_Inv_Act_Res.main1 Lexer_Inv_Act_Res.token lexbuf in
        if var_in_formula res_formula (ha.z @ ha.zp) then
         j := !j + 1
        else
         raise Res_Var_Not_Adm; 
      with Failure("lexing: empty token") -> raise Res_Not_Adm;
      with Parsing.Parse_error -> raise Res_Not_Adm
     )
    done;
    i := ! i + 1
  done;;

(* Functions for acquiring parameters for reachable_functions and reachable_diff_eqs *)
(* read_formula: function for reading a start or target formula *)
let read_formula z_list =
 let formula = read_line () in
  if formula <> "" then
   (
    let lexbuf = Lexing.from_string formula in
     try
     try
      let f = Parser_Inv_Act_Res.main1 Lexer_Inv_Act_Res.token lexbuf in
       if var_in_formula f z_list then
        (formula,false)
       else
        (print_string ("Variables not in Z occurring in the inserted formula.\n");("",true));
     with Failure("lexing: empty token") -> ("",true);
     with Parsing.Parse_error -> ("",true);
   )
  else
   (formula,false);;
(* read_delta: function for reading delta *)
let read_delta () =
 let delta_s = read_line () in
 let lexbuf = Lexing.from_string delta_s in
  try
    let delta = Parser_Delta.main1 Lexer_Delta.token lexbuf in
     (delta,false);
  with
   | Failure("lexing: empty token") -> (Int "0",true)
   | Parsing.Parse_error -> (Int "0",true);;
(* read_time_bound: function for reading time bound *)
let read_time_bound () =
 try
  let time = float_of_string (read_line ()) in
   (time,false);
 with Failure("float_of_string") -> (0.,true);; 
(* reachable_parameters: function for acquiring parameters for reachable_functions and reachable_diff_eqs *)
let reachable_parameters ha states =
 let start_state = ref "" and start_formula = ref "" in
 let target_state = ref "" and target_formula = ref "" in
 let delta = ref (Int "0") and time_bound = ref 0. and qe_plus_n = ref "" in 
 let ok = ref false in
 print_string ("Definition of the reachability problem:\n");
 print_string ("\nStarting region:\n");
 while !ok <> true do
  print_string ("Discrete location: ");
  start_state := read_line ();
  if List.mem !start_state states then
   ok := true
  else
   print_string ("Discrete location not in the automaton.\n");
 done;
 ok := false;
 while !ok <> true do
  print_string ("Formula defining the set of starting points:\n");
  let (s_f,error) = read_formula ha.z in
   if error then
    print_string ("Not admitted formula.\n")
   else
    (ok := true;start_formula := s_f);
 done;
 ok := false;
 print_string ("\nEnding region:\n");
 while !ok <> true do
  print_string ("Discrete location: ");
  target_state := read_line ();
  if List.mem !target_state states then
   ok := true
  else
   print_string ("Discrete location not in the automaton.\n");
 done;
 ok := false;
 while !ok <> true do
  print_string ("Formula defining the set of ending points:\n");
  let (t_f,error) = read_formula ha.z in
   if error then
    print_string ("Not admitted formula.\n")
   else
    (ok := true;target_formula := t_f);  
 done;
 ok := false;
 print_string ("\nTime discretization:\n");
 while !ok <> true do
  print_string ("delta: ");
  let (d,error) = read_delta () in
   if error then
    print_string ("Not admitted value.\n")
   else
    (ok := true;delta := d);
 done;
 ok := false;
 while !ok <> true do 
  print_string ("Time bound: ");
  let (t_b,error) = read_time_bound () in
   if error then
    print_string ("Not admitted value.\n")
   else
    (ok := true;time_bound := t_b);   
 done;
 print_string ("\nValue for QEPCAD B +N option: ");
 qe_plus_n := read_line ();
 print_newline ();
 (!start_state,!start_formula,!target_state,!target_formula,!delta,!time_bound,!qe_plus_n);;

(* reachable: main function for solving reachability *)
let reachable ha automaton_type =
 try
  if automaton_type = 0 then
   (
    let states = automaton_states ha.ve in
     if (List.mem "t" ha.z || List.mem "T" ha.z || List.mem "t" ha.zp || List.mem "T" ha.zp) then raise T_Not_Adm;
     check_formulae ha (Array.of_list states);
     let dyn_map = {m = separate_dyns_functions states ha.dyn ha.z ha.zp} in
     let (start_state,start_formula,target_state,target_formula,delta,time_bound,qe_plusn) = reachable_parameters ha states in
     let (reached,reached_set) = reachable_functions ha dyn_map start_state start_formula target_state target_formula delta time_bound qe_plusn in
      if reached = true then
       (print_string ("Reachable ending points:\n"); print_string reached_set; print_newline ())
      else
       print_string ("Ending states are not reachable.\n");
   )
  else
   (
    let states = automaton_states ha.ve in
     if (List.mem "t" ha.z || List.mem "T" ha.z || List.mem "t" ha.zp || List.mem "T" ha.zp) then raise T_Not_Adm;
     check_formulae ha (Array.of_list states);
     let dyn_map = {m = separate_dyns_diff_eqs states ha.dyn ha.z ha.zp} in
     let (start_state,start_formula,target_state,target_formula,delta,time_bound,qe_plusn) = reachable_parameters ha states in
     let (reached,reached_set) = reachable_diff_eqs ha dyn_map start_state start_formula target_state target_formula delta time_bound qe_plusn in
      if reached = true then
       (print_string ("Reachable ending points:\n"); print_string reached_set; print_newline ())
      else
       print_string ("Ending states are not reachable.\n");
   ); 
 with
  | T_Not_Adm -> print_string ("Names t and T are reserved.\n")
  | Inv_Not_Adm -> print_string ("Automaton with not admitted invariants.\n")
  | Inv_Var_Not_Adm -> print_string ("Variables not in Z occurring in invariants.\n")
  | Act_Not_Adm -> print_string ("Automaton with not admitted activations.\n")
  | Act_Var_Not_Adm -> print_string ("Variables not in Z occurring in activations.\n")
  | Res_Not_Adm -> print_string ("Automaton with not admitted resets.\n")
  | Res_Var_Not_Adm -> print_string ("Variables not in Z U Z' occurring in resets.\n")
  | Dyn_Not_Adm -> print_string ("Automaton with not admitted dynamics.\n")
  | Eq_Not_Adm -> print_string ("Dynamics with not admitted equations or variabiles not in Z and Z' occurring in dynamics.\n")
  | Diff_Eq_Not_Adm -> print_string ("Dynamics with not admitted or not autonomous differential equations.\n")
  | Eq_Num -> print_string ("In the dynamics, for each variable must be defined only one equation.\n")
  | QE_Seg_Fault -> print_string ("QEPCAD B: segmentation fault. Set a value for +N option.\n")
  | QE_Error -> print_string ("QEPCAD B: error.\n");;
