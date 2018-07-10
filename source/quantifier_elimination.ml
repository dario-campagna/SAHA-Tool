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


open Formulae;;
open Hybrid_automata;;

exception QE_Seg_Fault;;
exception QE_Error;;

(* vars_string: function for creating the variable string (x,y,...,z) *)
let vs v_string var = v_string ^ "," ^ var;;
let vars_string v_list =
 let v_string = "(" ^ List.hd v_list in
  (List.fold_left vs v_string (List.tl v_list)) ^ ")";;

(* exists_string: function for creating the string (Ex)(Ey)...(Ez) *)
let es e_string var = e_string ^ "(E" ^ var ^ ")";;
let exists_string q_list =
 let n = List.length q_list in
  if n > 0 then
   List.fold_left es "" q_list
  else
   "";;

(* write_formula: function for writing a qepcad input formula in file filename *)
let write_formula free_list quant_list qf_formula filename =
 let vars = (vars_string (free_list @ quant_list)) ^ "\n" in
 let free = (string_of_int (List.length free_list)) ^ "\n" in
 let exists = (exists_string quant_list) ^ "\n" in
 let p_formula = "[" ^ qf_formula ^ "].\n" in
 let file = Unix.openfile filename [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o644 in
  let _ = Unix.write file "[]\n" 0 3 in
  let _ = Unix.write file vars 0 (String.length vars) in
  let _ = Unix.write file free 0 (String.length free) in
  let _ = Unix.write file exists 0 (String.length exists) in
  let _ = Unix.write file p_formula 0 (String.length p_formula) in
  let _ = Unix.write file "finish\n" 0 7 in
  Unix.close file;;

(* exec_qepcad: function for executing qepcad on input input_filename, return a quantifier-free formula *)
let exec_qepcad input_filename free_var plus_n =
 let qe_out = "qepcad_output" in
 let grep_out1 = "grep_output1" in
 let grep_out2 = "grep_output2" in
 let search = "'An equivalent quantifier-free formula:'" in
 let string_plus_n = ref "" in
  if plus_n <> "" then string_plus_n := " +N" ^ plus_n
  else string_plus_n := " ";
 let qe_exit = Unix.system ("qepcad" ^ !string_plus_n ^ " < " ^ input_filename ^ " > " ^ qe_out) in
 if qe_exit = Unix.WEXITED 139 then
  raise QE_Seg_Fault
 else if qe_exit <> Unix.WEXITED 0 then
  raise QE_Error
 else
  let _ = Unix.system ("grep -A 2 " ^ search ^ " " ^ qe_out ^ " > " ^ grep_out1) in
  let _ = Unix.system ("grep -E '" ^ free_var ^ "|FALSE|TRUE' " ^ grep_out1 ^ " > " ^ grep_out2) in
  let input = Unix.openfile grep_out2 [Unix.O_RDONLY] 0o644 in
  let input_ch = Unix.in_channel_of_descr input in
   set_binary_mode_in input_ch false;
   let lexbuf = Lexing.from_channel input_ch in
   let result = Parser_QEPCAD_res.main1 Lexer_QEPCAD_res.token lexbuf in
    Unix.close input;
    result;;

(* qepcad_min_max: function for computing min-max zp_i *)
let qepcad_min_max zp z_list string_dyn start_set_formula plus_n =
 let formula = ref "" in
  if start_set_formula <> "" then
   formula := "[ " ^ start_set_formula ^ " ] /\\ [ " ^ string_dyn ^ " ]"
  else
   formula := string_dyn;
  let _ = write_formula [zp] z_list !formula "min_max_formula" in
   exec_qepcad "min_max_formula" zp plus_n;;
  
(* qepcad_reached: function for computing formula representing reached points set *)
let qepcad_reached z_list comp_formula inv plus_n =
 let formula = ref "" in
  if comp_formula = "" then
   if inv = "" then
    formula := ""
   else
    formula := inv
  else
   if inv = "" then
    formula := comp_formula
   else
    formula := "[ " ^ comp_formula ^ " ] /\\ [ " ^ inv ^ " ]";
  if !formula <> "" then
   let _ = write_formula z_list [] !formula "reached_formula" in
    exec_qepcad "reached_formula" (List.hd z_list) plus_n
  else
   True;;
  
(* qepcad_act: function for determining if edge activation is satisfied *)
let qepcad_act z_list reached_v act_e plus_n =
 let formula = ref "" in
  if reached_v = "" then
   if act_e = "" then
    formula := ""
   else
    formula := act_e
  else
   if act_e = "" then
    formula := reached_v
   else
    formula := "[ " ^ reached_v ^ " ] /\\ [ " ^ act_e ^ " ]";
  if !formula <> "" then
   let _ = write_formula z_list [] !formula "act_formula" in
    exec_qepcad "act_formula" (List.hd z_list) plus_n
  else
   True;;
  
(* qepcad_res: function for computing formula representing point reached after a discrete jump *)
let qepcad_res zp_list z_list comp_formula res_e plus_n =
 let formula = ref "" in
  if comp_formula = "" then
   if res_e = "" then
    formula := ""
   else
    formula := res_e
  else
   if res_e = "" then
    formula := comp_formula
   else
    formula := "[ " ^ comp_formula ^ " ] /\\ [ " ^ res_e ^  " ]";
  if !formula <> "" then
   let _ = write_formula zp_list z_list !formula "reset_formula" in
    exec_qepcad "reset_formula" (List.hd zp_list) plus_n
  else
   True;;
  
