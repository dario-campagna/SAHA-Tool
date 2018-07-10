/*

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

*/


%{
   open Hybrid_automata;;
   
   let empty_state_formula _ = "FALSE";;
   let csf (s,f) old_s_f_formula = function x -> if x = s then f else (old_s_f_formula x);;
   let create_state_formula s_f_list = List.fold_right csf s_f_list empty_state_formula;; 
   
   let empty_edge_formula _ _ = "FALSE";;
   let esf (s,t,f) old_edge_formula = function x -> function y -> if x = s && y = t then f else (old_edge_formula x y);;
   let create_edge_formula e_f_list = List.fold_right esf e_f_list empty_edge_formula;;
%}
%token VARIABLES GRAPH INV DYN ACT RES NONE
%token LLPAREN LRPAREN LSEP
%token <string> NAME
%token CLPAREN CRPAREN CSEP FSEP
%token <string> FORMULA
%token EDGE
%token EOF
%left LSEP CSEP FSEP EDGE
%start main1
%type <Hybrid_automata.hybrid_automaton> main1
%%
main1:
    VARIABLES expr1 expr1
    GRAPH expr2
    INV expr3
    DYN expr3
    ACT expr4
    RES expr4
    EOF                   {{ z = $2;
                             zp = $3;
                             ve = $5;
                             inv = Hybrid_automata.StateFormula (create_state_formula $7);
                             dyn = Hybrid_automata.StateFormula (create_state_formula $9);
                             act = Hybrid_automata.EdgeFormula (create_edge_formula $11);
                             res = Hybrid_automata.EdgeFormula (create_edge_formula $13);
                          }}
;
expr1:
    LLPAREN expr1_1 LRPAREN    { $2 }
;
expr1_1:
    NAME                    { $1::[] }
  | expr1_1 LSEP expr1_1    { $1 @ $3 }    
;
expr2:
    LLPAREN expr2_1 LRPAREN    { $2 }
;
expr2_1:
    CLPAREN NAME CSEP expr2_2 CRPAREN    { ($2,$4)::[] }
  | expr2_1 LSEP expr2_1                 { $1 @ $3 }
;
expr2_2:
    LLPAREN LRPAREN            { [] } 
  | LLPAREN expr1_1 LRPAREN    { $2 }
;
expr3:
    NAME FSEP FORMULA          {($1,$3)::[]}
  | expr3 LSEP expr3           {$1 @ $3}
;
expr4:
    NONE       { [] }   
  | expr4_1    { $1 }
expr4_1:
    NAME EDGE NAME FSEP FORMULA        { ($1,$3,$5)::[] }
  | expr4_1 LSEP expr4_1               { $1 @ $3 }

