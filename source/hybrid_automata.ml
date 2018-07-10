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


(* Types for representing hybrid automata *)

(* Continuous variable *)
type cont_var = string;;

(* Discrete location *)
type discrete_location = string;;
 
(* Formula as string of characters *)
type string_formula = string;;

(* Graph adjacency list *)
type graph_adj_list = (discrete_location * discrete_location list) list;;

(* Function from state (v) to formula (Inv(v) or Dyn(v)) *)
type state_formula = StateFormula of (discrete_location -> string_formula);;

(* Function from edge (<v,u>) to formula (Act(<v,u>) or Res(<v,u>)) *)
type edge_formula = EdgeFormula of (discrete_location -> discrete_location -> string_formula);;

(* Hybrid automaton *)
type hybrid_automaton =
 {
  z   : cont_var list;          (* Z *)
  zp  : cont_var list;          (* Z' *)
  ve  : graph_adj_list;         (* graph (V,E) *)
  inv : state_formula;          (* Inv *)
  dyn : state_formula;          (* Dyn *) 
  act : edge_formula;           (* Act *)  
  res : edge_formula            (* Res *)
 };;
 
 
(* automaton_states: function for extracting states from a graph_adj_list *)
let st (state,_) st_list = state::st_list;;
let automaton_states ve = List.fold_right st ve [];;

(* take_inv: function for extracting the invariant function of an automaton *)
let take_inv (StateFormula inv) = inv;;

(* take_edge_formula: function for extracting activation or reset function of an automaton *)
let take_edge_formula (EdgeFormula ef) = ef;;

(* adj_states: function for extracting adjacent states from a graph_adj_list *)
let rec adj_states state ve =
 match ve with
       ((s,al)::rve) -> if state = s then Array.of_list al else adj_states state rve
     | [] -> [||];;

(* Functions for printing an automaton *)
let string_of_list l =
 let a = Array.of_list l in
 let n = Array.length a in
 if n > 0 then 
  ( 
   let s = ref "[" in
    for i = 0 to n - 2 do
     s := !s ^ a.(i) ^ "; "
    done;
    s := !s ^ a.(n - 1) ^ "]";
    !s
  )
 else
  "[]";;
  
let print_graph_adj_list ve =
 let ve_a = Array.of_list ve in
 let n = Array.length ve_a in
  print_string ("[");
  for i = 0 to n - 2 do
   let (s,l) = ve_a.(i) in
    print_string ("(" ^ s ^ ", " ^ (string_of_list l) ^ "); ")
  done;
  let (s,l) = ve_a.(n-1) in
   print_string ("(" ^ s ^ ", " ^ (string_of_list l) ^ ")]");;
   
let print_state_formula (StateFormula sf) ve =
 let states = Array.of_list (automaton_states ve) in
 let n = Array.length states in
  for i = 0 to n - 1 do
   print_string (states.(i) ^ " : " ^ (sf states.(i)) ^ "\n")
  done;;
  
let print_edge_formula (EdgeFormula ef) ve =
 let states = Array.of_list (automaton_states ve) in
 let n = Array.length states in
  for i = 0 to n - 1 do
   let adj_s = adj_states states.(i) ve in
   let m = Array.length adj_s in
   if m > 0 then
    for j = 0 to m - 1 do
     print_string (states.(i) ^ " -> " ^ adj_s.(j) ^ " : " ^ (ef states.(i) adj_s.(j)) ^ "\n")
    done;
  done;;

let print_automaton ha =
 print_string ("Z = ");
 print_string (string_of_list ha.z);
 print_newline ();
 print_string ("Z' = ");
 print_string (string_of_list ha.zp);
 print_newline ();
 print_string ("Graph:\n");
 print_graph_adj_list ha.ve;
 print_newline ();
 print_string ("Inv:\n");
 print_state_formula ha.inv ha.ve;
 print_string ("Dyn:\n");
 print_state_formula ha.dyn ha.ve;
 print_string ("Act:\n");
 print_edge_formula ha.act ha.ve;
 print_string ("Res:\n");
 print_edge_formula ha.res ha.ve;;
