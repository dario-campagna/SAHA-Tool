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


(* Module [Hybrid_automata]: types for representing hybrid automata *)

type cont_var = string;;

type discrete_location = string;;

type string_formula = string;;

type graph_adj_list = (discrete_location * discrete_location list) list;;

type state_formula = StateFormula of (discrete_location -> string_formula);;

type edge_formula = EdgeFormula of (discrete_location -> discrete_location -> string_formula);;

type hybrid_automaton =
 {
  z   : cont_var list;     
  zp  : cont_var list;     
  ve  : graph_adj_list;  
  inv : state_formula;          
  dyn : state_formula;      
  act : edge_formula;             
  res : edge_formula
 };;
 

val automaton_states : graph_adj_list -> discrete_location list;;

val take_inv : state_formula -> (discrete_location -> string_formula);;

val take_edge_formula : edge_formula -> (discrete_location -> discrete_location -> string_formula);;

val adj_states : discrete_location -> graph_adj_list -> discrete_location array;;

val print_automaton : hybrid_automaton -> unit;;
