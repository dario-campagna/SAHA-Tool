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
   open Formulae;;
%}
%token <string> INT
%token RATIONAL
%token EOL
%nonassoc RATIONAL
%start main1
%type <Formulae.term> main1
%%
main1:
    expr1 EOL             { $1 }
;
expr1:
    INT                     { Int $1 }
  | INT RATIONAL INT        { Rational ($1,$3) } 
;
