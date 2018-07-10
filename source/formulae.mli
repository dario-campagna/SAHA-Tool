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


(* Module [Formulae]: types for representing QEPCAD B first-order formulae with no quantifiers *)

type long_int = string;;

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

type pred = Lt of term * term
          | Leq of term * term
          | Eq of term * term
          | Geq of term * term
          | Gt of term * term
          | Neq of term * term;;

type formula = True | False
             | Atomic of pred
             | Not of formula
             | And of formula * formula
             | Or of formula * formula
             | Impl of formula * formula
             | Iff of formula * formula
             | FParen of formula;;

val string_of_formula : formula -> string;;

val formula_subs_time : formula -> term -> formula;;

val formula_subs_var : formula -> string list -> string array -> formula;;

val var_in_term : term -> string list -> bool

val var_in_formula : formula -> string list -> bool



