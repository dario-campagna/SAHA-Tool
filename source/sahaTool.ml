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


open Reachable;;
open Hybrid_automata;;

let menu () =
 print_string ("\n1) Work on automaton with dynamics defined by functions of the form f(Z,T);\n");
 print_string ("2) Work on automaton with dynamics defined by differential equations;\n");
 print_string ("3) Help.\n");
 let choice = ref "" in
 let choice_ok = ref false in
  while !choice_ok = false do
   print_string ("Make your choice: ");
    choice := read_line ();
    if !choice <> "1" && !choice <> "2" && !choice <> "3" then
     print_string ("Insert 1, 2 or 3.\n")
    else
     choice_ok := true;
  done;
  !choice;;
  
let start kind =
 try
  print_string ("\nInsert the path of the file with the definition of the automaton: ");
  let automaton_path = read_line () in
  let automaton_input = Unix.openfile automaton_path [Unix.O_RDONLY] 0o644 in
  let input_ch = Unix.in_channel_of_descr automaton_input in
   set_binary_mode_in input_ch false;
   let lexbuf = Lexing.from_channel input_ch in
   try
    let automaton = Parser_Automata.main1 Lexer_Automata.token lexbuf in
     print_string ("\nLoaded automaton.\n");
     print_automaton automaton;
     print_newline();
     reachable automaton kind;
   with Parsing.Parse_error -> print_string ("Errors in the definition of the automaton.\n");
 with Unix.Unix_error (Unix.ENOENT,"open",automaton_path) -> print_string ("File not found.\n");;
 
let help () =
 print_string ("\nDescription of the hybrid automaton:\n");
 print_string ("  VARIABLES\n");
 print_string ("  [ name_of_#1_var_in_Z; ... ; name_of_#n_var_in_Z ]\n");
 print_string ("  [ name_of_#1_var_in_Z'; ... ; name_of_#n_var_in_Z' ]\n");
 print_string ("  GRAPH\n");
 print_string ("  [ (discrete_location_name, [ adjacent_discrete_location_name; ...]); ...]\n");
 print_string ("  INV\n");
 print_string ("  discrete_location_name : \" formula \";\n");
 print_string ("  discrete_location_name : \" formula \";\n");
 print_string ("  ...\n");
 print_string ("  discrete_location_name : \" formula \"\n");
 print_string ("  DYN\n");
 print_string ("  discrete_location_name : \" dynamic \";\n");
 print_string ("  discrete_location_name : \" dynamic \";\n");
 print_string ("  ...\n");
 print_string ("  discrete_location_name : \" dynamic \"\n");
 print_string ("  ACT\n");
 print_string ("  NONE if there are no edges, otherwise\n");
 print_string ("  discrete_location_name -> discrete_location_name : \" formula \";\n");
 print_string ("  discrete_location_name -> discrete_location_name : \" formula \";\n");
 print_string ("  ...\n");
 print_string ("  discrete_location_name -> discrete_location_name : \" formula \"\n");
 print_string ("  RES\n");
 print_string ("  NONE if there are no edges, otherwise\n");
 print_string ("  discrete_location_name -> discrete_location_name : \" formula \";\n");
 print_string ("  discrete_location_name -> discrete_location_name : \" formula \";\n");
 print_string ("  ...\n");
 print_string ("  discrete_location_name -> discrete_location_name : \" formula \"\n");
 print_string ("\nNames for variables in Z and Z', names for discrete locations:\n");
 print_string ("  Names for variables and discrete locations can be strings beginning with\n");
 print_string ("  a letter (upper or lower case), followed by letters and numbers.\n");
 print_string ("  Names \"t\" and \"T\" can not be used for variables in Z and Z'.\n");
 print_string ("\nFormulae:\n");
 print_string ("  First order formulae without quantifiers.\n");
 print_string ("  Boolean operators: /\\, \\/, ~ (negation), ==>, <==, <==>.\n");
 print_string ("  Relational operators: =, /=, <, >, <=, >=.\n");
 print_string ("  Arithmetic operators: +, -, ' ' (product), ^ (power).\n");
 print_string ("  Precedence for Boolean operators is not understood, to remove ambiguity\n");
 print_string ("  brackets [ and ] must be used.\n");
 print_string ("  Atomic formulae are equalities or inequalities of polynomials with variables\n");
 print_string ("  in Z (for invariants and activations) or in Z U Z' (for resets), with\n");
 print_string ("  rational coefficients and positive integer exponents.\n");
 print_string ("  Within polynomials ( and ) can be used.\n");
 print_string ("  The empty string can be used to represent the formula TRUE.\n");
 print_string ("\nDynamics:\n");
 print_string ("  Two types of dynamics are admitted.\n");
 print_string ("  Dynamics defined by functions of the form f(Z,T):\n");
 print_string ("   name_of_#1_var_in_Z' = f_1(Z,T) /\\ ... /\\ name_of_#n_var_in_Z' = f_n(Z,T)\n");
 print_string ("   f_i(Z,T) is a polynomial with variables in Z, in which the variabile\n");
 print_string ("   representing time (\"t\" or \"T\") occurs, defined using the arithmetic\n");
 print_string ("   operators admitted in formulae, rational coefficients and positive\n");
 print_string ("   integer exponents.\n");
 print_string ("   f_i(Z,T) must be such that f_i(Z,0) = Z_i.\n");
 print_string ("   For each variable in Z' must be defined only one equation.\n");
 print_string ("  Dynamics defined by systems of autonomous differential equations:\n");
 print_string ("   A system is represented by a formula of the following form\n");
 print_string ("   [name_of_#1_var_in_Z]_d = f_1(Z) /\ ... /\\ [name_of_#n_var_in_Z]_d = f_n(Z)\n");
 print_string ("   f_i(Z) is a polynomial with variables in Z, defined using the arithmetic\n");
 print_string ("   operators admitted in formulae, rational coefficients and positive\n");
 print_string ("   integer exponents. For each variable in Z must be defined only one\n");
 print_string ("   differential equations.\n");
 print_string ("\nTime discretization:\n");
 print_string ("  Delta: a positive integer number or a positive rational number (i.e. 1/10).\n");
 print_string ("  Time bound: a postive integer number.\n");
 print_string ("\nQEPCAD B +N option:\n");
 print_string ("  The +N option of QEPCAD B permits to set the amount of memory to be used\n");
 print_string ("  for garbage collection. The default value is 2000000.\n");
 ();;

let main () =
 print_string ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
 print_string ("%                                                                             %\n");
 print_string ("%                                 SAHA Tool                                   %\n");
 print_string ("%                                                                             %\n");
 print_string ("%        Computing approximated solutions to the reachability problem         %\n");
 print_string ("%                     in semi-algebraic hybrid automata                       %\n");
 print_string ("%                                                                             %\n");
 print_string ("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n");
 print_string ("SAHA-Tool Copyright (C) 2008  Dario Campagna\n");
 print_string ("Released under the terms of GNU GENERAL PUBLIC LICENSE Version 3\n\n");
 print_string ("This program comes with ABSOLUTELY NO WARRANTY; for details type 'show w'.\n");
 print_string ("This is free software, and you are welcome to redistribute it\n");   
 print_string ("under certain conditions; type 'show c' for details.\n\n");
 print_string ("Type 'start' to begin working.\n\n");
 
 let to_do = ref "" in
 let to_do_ok = ref false in
  while !to_do_ok = false do
   print_string (">> ");
    to_do := read_line ();
    if !to_do <> "show w" && !to_do <> "show c" && !to_do <> "start" then
     print_string ("\nType show w, show c or start.\n")
    else
     to_do_ok := true;
  done;
 if !to_do = "start" then
 (  
  let choice = menu () in
   if choice = "1" then
    start 0
   else if choice = "2" then
    start 1
   else
    help ();
 )
 else if !to_do = "show w" then
 (
  print_string ("\nDisclaimer of Warranty. (GNU GENERAL PUBLIC LICENSE Version 3, section 15)\n");
  print_string ("THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\n");
  print_string ("APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\n");
  print_string ("HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\n");
  print_string ("OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\n");
  print_string ("THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n");
  print_string ("PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\n");
  print_string ("IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\n");
  print_string ("ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n");
 )
 else
 (
  print_string ("\nConveying Verbatim Copies. (GNU GENERAL PUBLIC LICENSE Version 3, section 4)\n");
  print_string ("You may convey verbatim copies of the Program's source code as you\n");
  print_string ("receive it, in any medium, provided that you conspicuously and\n");
  print_string ("appropriately publish on each copy an appropriate copyright notice;\n");
  print_string ("keep intact all notices stating that this License and any\n");
  print_string ("non-permissive terms added in accord with section 7 apply to the code;\n");
  print_string ("keep intact all notices of the absence of any warranty; and give all\n");
  print_string ("recipients a copy of this License along with the Program.\n");
  print_string ("You may charge any price or no price for each copy that you convey,\n");
  print_string ("and you may offer support or warranty protection for a fee.\n");
  print_string ("\nConveying Modified Source Versions. (GNU GENERAL PUBLIC LICENSE Version 3, section 5)\n");
  print_string ("You may convey a work based on the Program, or the modifications to\n");
  print_string ("produce it from the Program, in the form of source code under the\n");
  print_string ("terms of section 4, provided that you also meet all of these conditions:\n");
  print_string (" a) The work must carry prominent notices stating that you modified\n");
  print_string ("    it, and giving a relevant date.\n");
  print_string (" b) The work must carry prominent notices stating that it is\n");
  print_string ("    released under this License and any conditions added under section\n");
  print_string ("    7.  This requirement modifies the requirement in section 4 to\n");
  print_string ("    \"keep intact all notices\".\n");
  print_string (" c) You must license the entire work, as a whole, under this\n");
  print_string ("    License to anyone who comes into possession of a copy.  This\n");
  print_string ("    License will therefore apply, along with any applicable section 7\n");
  print_string ("    additional terms, to the whole of the work, and all its parts,\n");
  print_string ("    regardless of how they are packaged.  This License gives no\n");
  print_string ("    permission to license the work in any other way, but it does not\n");
  print_string ("    invalidate such permission if you have separately received it.\n");
  print_string (" d) If the work has interactive user interfaces, each must display\n");
  print_string ("    Appropriate Legal Notices; however, if the Program has interactive\n");
  print_string ("    interfaces that do not display Appropriate Legal Notices, your\n");
  print_string ("    work need not make them do so.\n");
  print_string ("A compilation of a covered work with other separate and independent\n");
  print_string ("works, which are not by their nature extensions of the covered work,\n");
  print_string ("and which are not combined with it such as to form a larger program,\n");
  print_string ("in or on a volume of a storage or distribution medium, is called an\n");
  print_string ("\"aggregate\" if the compilation and its resulting copyright are not\n");
  print_string ("used to limit the access or legal rights of the compilation's users\n");
  print_string ("beyond what the individual works permit.  Inclusion of a covered work\n");
  print_string ("in an aggregate does not cause this License to apply to the other\n");
  print_string ("parts of the aggregate.\n");
  print_string ("\nConveying Non-Source Forms. (GNU GENERAL PUBLIC LICENSE Version 3, section 6)\n");
  print_string ("You may convey a covered work in object code form under the terms\n");
  print_string ("of sections 4 and 5, provided that you also convey the\n");
  print_string ("machine-readable Corresponding Source under the terms of this License,\n");
  print_string ("in one of these ways:\n");
  print_string (" a) Convey the object code in, or embodied in, a physical product\n");
  print_string ("    (including a physical distribution medium), accompanied by the\n");
  print_string ("    Corresponding Source fixed on a durable physical medium\n");
  print_string ("    customarily used for software interchange.\n");
  print_string (" b) Convey the object code in, or embodied in, a physical product\n");
  print_string ("    (including a physical distribution medium), accompanied by a\n");
  print_string ("    written offer, valid for at least three years and valid for as\n");
  print_string ("    long as you offer spare parts or customer support for that product\n");
  print_string ("    model, to give anyone who possesses the object code either (1) a\n");
  print_string ("    copy of the Corresponding Source for all the software in the\n");
  print_string ("    product that is covered by this License, on a durable physical\n");
  print_string ("    medium customarily used for software interchange, for a price no\n");
  print_string ("    more than your reasonable cost of physically performing this\n");
  print_string ("    conveying of source, or (2) access to copy the\n");
  print_string ("    Corresponding Source from a network server at no charge.\n");
  print_string (" c) Convey individual copies of the object code with a copy of the\n");
  print_string ("    written offer to provide the Corresponding Source.  This\n");
  print_string ("    alternative is allowed only occasionally and noncommercially, and\n");
  print_string ("    only if you received the object code with such an offer, in accord\n");
  print_string ("    with subsection 6b.\n");
  print_string (" d) Convey the object code by offering access from a designated\n");
  print_string ("    place (gratis or for a charge), and offer equivalent access to the\n");
  print_string ("    Corresponding Source in the same way through the same place at no\n");
  print_string ("    further charge.  You need not require recipients to copy the\n");
  print_string ("    Corresponding Source along with the object code.  If the place to\n");
  print_string ("    copy the object code is a network server, the Corresponding Source\n");
  print_string ("    may be on a different server (operated by you or a third party)\n");
  print_string ("    that supports equivalent copying facilities, provided you maintain\n");
  print_string ("    clear directions next to the object code saying where to find the\n");
  print_string ("    Corresponding Source.  Regardless of what server hosts the\n");
  print_string ("    Corresponding Source, you remain obligated to ensure that it is\n");
  print_string ("    available for as long as needed to satisfy these requirements.\n");
  print_string (" e) Convey the object code using peer-to-peer transmission, provided\n");
  print_string ("    you inform other peers where the object code and Corresponding\n");
  print_string ("    Source of the work are being offered to the general public at no\n");
  print_string ("  charge under subsection 6d.\n");
  print_string ("A separable portion of the object code, whose source code is excluded\n");
  print_string ("from the Corresponding Source as a System Library, need not be\n");
  print_string ("included in conveying the object code work.\n");
  print_string ("A \"User Product\" is either (1) a \"consumer product\", which means any\n");
  print_string ("tangible personal property which is normally used for personal, family,\n");
  print_string ("or household purposes, or (2) anything designed or sold for incorporation\n");
  print_string ("into a dwelling.  In determining whether a product is a consumer product,\n");
  print_string ("doubtful cases shall be resolved in favor of coverage.  For a particular\n");
  print_string ("product received by a particular user, \"normally used\" refers to a\n");
  print_string ("typical or common use of that class of product, regardless of the status\n");
  print_string ("of the particular user or of the way in which the particular user\n");
  print_string ("actually uses, or expects or is expected to use, the product.  A product\n");
  print_string ("is a consumer product regardless of whether the product has substantial\n");
  print_string ("commercial, industrial or non-consumer uses, unless such uses represent\n");
  print_string ("the only significant mode of use of the product.\n");
  print_string ("\"Installation Information\" for a User Product means any methods,\n");
  print_string ("procedures, authorization keys, or other information required to install\n");
  print_string ("and execute modified versions of a covered work in that User Product from\n");
  print_string ("a modified version of its Corresponding Source.  The information must\n");
  print_string ("suffice to ensure that the continued functioning of the modified object\n");
  print_string ("code is in no case prevented or interfered with solely because\n");
  print_string ("modification has been made.\n");
  print_string ("If you convey an object code work under this section in, or with, or\n");
  print_string ("specifically for use in, a User Product, and the conveying occurs as\n");
  print_string ("part of a transaction in which the right of possession and use of the\n");
  print_string ("User Product is transferred to the recipient in perpetuity or for a\n");
  print_string ("fixed term (regardless of how the transaction is characterized), the\n");
  print_string ("Corresponding Source conveyed under this section must be accompanied\n");
  print_string ("by the Installation Information.  But this requirement does not apply\n");
  print_string ("if neither you nor any third party retains the ability to install\n");
  print_string ("modified object code on the User Product (for example, the work has\n");
  print_string ("been installed in ROM).\n");
  print_string ("The requirement to provide Installation Information does not include a\n");
  print_string ("requirement to continue to provide support service, warranty, or updates\n");
  print_string ("for a work that has been modified or installed by the recipient, or for\n");
  print_string ("the User Product in which it has been modified or installed.  Access to a\n");
  print_string ("network may be denied when the modification itself materially and\n");
  print_string ("adversely affects the operation of the network or violates the rules and\n");
  print_string ("protocols for communication across the network.\n");
  print_string ("Corresponding Source conveyed, and Installation Information provided,\n");
  print_string ("in accord with this section must be in a format that is publicly\n");
  print_string ("documented (and with an implementation available to the public in\n");
  print_string ("source code form), and must require no special password or key for\n");
  print_string ("unpacking, reading or copying.\n");
 );;
main ();;
