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


(* Types for representing QEPCAD B first-order formulae with no quantifiers *)

type long_int = string;;

(* Terms *)
type term = Var of string
          | DVar of string
          | Int of long_int
          | Rational of long_int * long_int
          | Neg of term
          | Add of term * term
          | Sub of term * term
          | Prod of term * term
          | Pow of term * long_int
          | TParen of term;;
          
let rec string_of_term t = 
 match t with
       Var s -> s
     | DVar s -> s ^ "_d"
     | Int n -> n
     | Rational (n,d) -> n ^ "/" ^ d
     | Neg t1 -> "-" ^ string_of_term t1
     | Add (t1,t2) -> string_of_term t1 ^ " + " ^ string_of_term t2
     | Sub (t1,t2) -> string_of_term t1 ^ " - " ^ string_of_term t2
     | Prod (t1,t2) -> string_of_term t1 ^ " " ^ string_of_term t2
     | Pow (t1,n) -> string_of_term t1 ^ "^" ^ n
     | TParen t1 -> "(" ^ string_of_term t1 ^ ")";;

let rec term_subs_time t delta =
 match t with
       Var "t" -> delta
     | Var "T" -> delta
     | Var x -> Var x
     | DVar x -> DVar x
     | Int n -> Int n
     | Rational (n,d) -> Rational (n,d)
     | Neg t1 -> Neg (term_subs_time t1 delta)
     | Add (t1,t2) -> Add ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Sub (t1,t2) -> Sub ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Prod (t1,t2) -> Prod ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Pow (t1,n) -> Pow ((term_subs_time t1 delta),n)
     | TParen t1 -> TParen (term_subs_time t1 delta);;

let rec pos_in_list x ls i =
 if x = (List.hd ls) then i else pos_in_list x (List.tl ls) (i + 1);;  

let rec term_subs_var t old_vars new_vars =
 match t with
       Var x -> if List.mem x old_vars then Var (new_vars.(pos_in_list x old_vars 0)) else Var x
     | DVar x -> DVar x
     | Int n -> Int n
     | Rational (n,d) -> Rational (n,d)
     | Neg t1 -> Neg (term_subs_var t1 old_vars new_vars)
     | Add (t1,t2) -> Add ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Sub (t1,t2) -> Sub ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Prod (t1,t2) -> Prod ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Pow (t1,n) -> Pow ((term_subs_var t1 old_vars new_vars),n)
     | TParen t1 -> TParen (term_subs_var t1 old_vars new_vars);;
     
let rec var_in_term t var_list =
 match t with
       Var x -> List.mem x var_list
     | DVar x -> List.mem x var_list
     | Int n -> true
     | Rational (n,d) -> true
     | Neg t1 -> var_in_term t1 var_list
     | Add (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Sub (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Prod (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Pow (t1,n) -> var_in_term t1 var_list
     | TParen t1 -> var_in_term t1 var_list;;

(* Predicates *)
type pred = Lt of term * term
          | Leq of term * term
          | Eq of term * term
          | Geq of term * term
          | Gt of term * term
          | Neq of term * term;;
          
let string_of_pred p =
 match p with
       Lt (t1,t2) -> string_of_term t1 ^ " < " ^ string_of_term t2
     | Leq (t1,t2) -> string_of_term t1 ^ " <= " ^ string_of_term t2
     | Eq (t1,t2) -> string_of_term t1 ^ " = " ^ string_of_term t2
     | Geq (t1,t2) -> string_of_term t1 ^ " >= " ^ string_of_term t2
     | Gt (t1,t2) -> string_of_term t1 ^ " > " ^ string_of_term t2
     | Neq (t1,t2) -> string_of_term t1 ^ " /= " ^ string_of_term t2;;

let pred_subs_time p delta =
 match p with
       Lt (t1,t2) -> Lt ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Leq (t1,t2) -> Leq ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Eq (t1,t2) -> Eq ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Geq (t1,t2) -> Geq ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Gt (t1,t2) -> Gt ((term_subs_time t1 delta),(term_subs_time t2 delta))
     | Neq (t1,t2) -> Neq ((term_subs_time t1 delta),(term_subs_time t2 delta));;
     
let pred_subs_var p old_vars new_vars =
 match p with
       Lt (t1,t2) -> Lt ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Leq (t1,t2) -> Leq ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Eq (t1,t2) -> Eq ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Geq (t1,t2) -> Geq ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Gt (t1,t2) -> Gt ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars))
     | Neq (t1,t2) -> Neq ((term_subs_var t1 old_vars new_vars),(term_subs_var t2 old_vars new_vars));;

let var_in_pred p var_list =
 match p with
       Lt (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Leq (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Eq (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Geq (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Gt (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     | Neq (t1,t2) -> (var_in_term t1 var_list) && (var_in_term t2 var_list)
     
(* Formulae *)
type formula = True | False
             | Atomic of pred
             | Not of formula
             | And of formula * formula
             | Or of formula * formula
             | Impl of formula * formula
             | Iff of formula * formula
             | FParen of formula;;
              
let rec string_of_formula f =
 match f with
       True -> "TRUE"
     | False -> "FALSE"
     | Atomic p -> string_of_pred p
     | Not f1 -> "~ " ^ string_of_formula f1
     | And (f1,f2) -> string_of_formula f1 ^ " /\\ " ^ string_of_formula f2
     | Or (f1,f2) -> string_of_formula f1 ^ " \\/ " ^ string_of_formula f2
     | Impl (f1,f2) -> string_of_formula f1 ^ " ==> " ^ string_of_formula f2
     | Iff (f1,f2) -> string_of_formula f1 ^ " <==> " ^ string_of_formula f2
     | FParen f1 -> "[" ^ string_of_formula f1 ^ "]";;
     
let rec formula_subs_time f delta =
 match f with
       True -> True
     | False -> False
     | Atomic p -> Atomic (pred_subs_time p delta)
     | Not f1 -> Not (formula_subs_time f1 delta)
     | And (f1,f2) -> And ((formula_subs_time f1 delta),(formula_subs_time f2 delta)) 
     | Or (f1,f2) -> Or ((formula_subs_time f1 delta),(formula_subs_time f2 delta))
     | Impl (f1,f2) -> Impl ((formula_subs_time f1 delta),(formula_subs_time f2 delta))
     | Iff (f1,f2) -> Iff ((formula_subs_time f1 delta),(formula_subs_time f2 delta))
     | FParen f1 -> FParen (formula_subs_time f1 delta);;

let rec formula_subs_var f old_vars new_vars =
 match f with
       True -> True
     | False -> False
     | Atomic p -> Atomic (pred_subs_var p old_vars new_vars)
     | Not f1 -> Not (formula_subs_var f1 old_vars new_vars)
     | And (f1,f2) -> And ((formula_subs_var f1 old_vars new_vars),(formula_subs_var f2 old_vars new_vars)) 
     | Or (f1,f2) -> Or ((formula_subs_var f1 old_vars new_vars),(formula_subs_var f2 old_vars new_vars))
     | Impl (f1,f2) -> Impl ((formula_subs_var f1 old_vars new_vars),(formula_subs_var f2 old_vars new_vars))
     | Iff (f1,f2) -> Iff ((formula_subs_var f1 old_vars new_vars),(formula_subs_var f2 old_vars new_vars))
     | FParen f1 -> FParen (formula_subs_var f1 old_vars new_vars);;
     
let rec var_in_formula f var_list =
 match f with
       True -> true
     | False -> true
     | Atomic p -> var_in_pred p var_list
     | Not f1 -> var_in_formula f1 var_list
     | And (f1,f2) -> (var_in_formula f1 var_list) && (var_in_formula f2 var_list)
     | Or (f1,f2) -> (var_in_formula f1 var_list) && (var_in_formula f2 var_list)
     | Impl (f1,f2) -> (var_in_formula f1 var_list) && (var_in_formula f2 var_list)
     | Iff (f1,f2) -> (var_in_formula f1 var_list) && (var_in_formula f2 var_list)
     | FParen f1 -> var_in_formula f1 var_list;;
