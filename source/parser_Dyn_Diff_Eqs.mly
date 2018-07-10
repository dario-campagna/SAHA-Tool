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
%token AND
%token FRPAREN FLPAREN
%token EQ
%token <string> INT
%token <string> VAR
%token <string> DVAR
%token RATIONAL NEG
%token ADD SUB PROD POW
%token TLPAREN TRPAREN
%token EOL
%left ADD SUB
%left PROD
%nonassoc POW RATIONAL NEG
%nonassoc EQ
%nonassoc AND
%start main1
%type <Formulae.formula> main1
%%
main1:
    expr1 EOL             { $1 }
;
expr1:
    expr1 AND expr1         { And ($1,$3) }
  | expr2                   { Atomic $1}
  | FLPAREN expr1 FRPAREN   { FParen $2 }
;
expr2:
     DVAR EQ expr3         { Eq (DVar $1,$3) }
;
expr3:
    INT                     { Int $1 }
  | VAR                     { Var $1 }
  | INT RATIONAL INT        { Rational ($1,$3) } 
  | expr3 ADD expr3         { Add ($1,$3) }
  | expr3 SUB expr3         { Sub ($1,$3) }
  | expr3 PROD expr3        { Prod ($1,$3) }
  | expr3 POW INT           { Pow ($1,$3) }
  | NEG expr3               { Neg $2 }
  | TLPAREN expr3 TRPAREN   { TParen $2 }
;
