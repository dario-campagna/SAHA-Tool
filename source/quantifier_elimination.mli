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

exception QE_Seg_Fault;;
exception QE_Error;;

val qepcad_min_max : cont_var -> (cont_var list) -> string_formula -> string_formula -> string -> formula;;

val qepcad_reached : (cont_var list) -> string_formula -> string_formula -> string -> formula;;

val qepcad_act : (cont_var list) -> string_formula -> string_formula -> string -> formula;;

val qepcad_res: (cont_var list) -> (cont_var list) -> string_formula -> string_formula -> string -> formula;;
