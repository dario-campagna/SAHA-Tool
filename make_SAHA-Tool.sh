#!/bin/sh
echo "Making lexers"
ocamllex ./source/lexer_Automata.mll
ocamllex ./source/lexer_Delta.mll
ocamllex ./source/lexer_Dyn_Diff_Eqs.mll
ocamllex ./source/lexer_Dyn_Fun.mll
ocamllex ./source/lexer_Inv_Act_Res.mll
ocamllex ./source/lexer_QEPCAD_res.mll
echo "Making parsers"
ocamlyacc ./source/parser_Automata.mly
ocamlyacc ./source/parser_Delta.mly
ocamlyacc ./source/parser_Dyn_Fun.mly
ocamlyacc ./source/parser_Dyn_Diff_Eqs.mly
ocamlyacc ./source/parser_Inv_Act_Res.mly
ocamlyacc ./source/parser_QEPCAD_res.mly
echo "Making SAHA-Tool"
cd source
ocamlopt -o sahaTool unix.cmxa formulae.mli formulae.ml hybrid_automata.mli hybrid_automata.ml parser_Automata.mli lexer_Automata.ml parser_Automata.ml parser_Delta.mli lexer_Delta.ml parser_Delta.ml parser_Dyn_Diff_Eqs.mli lexer_Dyn_Diff_Eqs.ml parser_Dyn_Diff_Eqs.ml parser_Dyn_Fun.mli lexer_Dyn_Fun.ml parser_Dyn_Fun.ml parser_Inv_Act_Res.mli lexer_Inv_Act_Res.ml parser_Inv_Act_Res.ml parser_QEPCAD_res.mli lexer_QEPCAD_res.ml parser_QEPCAD_res.ml quantifier_elimination.mli quantifier_elimination.ml reachable.mli reachable.ml sahaTool.ml
cd ..
mv ./source/sahaTool ./bin
