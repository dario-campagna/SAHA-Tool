# SAHA-Tool

*SAHA-Tool* is a software implemented in Objective Caml. It allows one to compute approximated solutions to the reachability problem in Semi-Algebraic Hybrid Automata, exploiting the *miM-ra* method.

*SAHA-Tool* is released under the terms of the GNU GENERAL PUBLIC LICENSE Version 3.0.

## miM-ra method

The *miM-ra* method has been described in "[Dario Campagna, Carla Piazza.
Hybrid automata, reachability, and Systems Biology.
Theoretical Computer Science,
Volume 411, Issue 20,
2010](https://www.sciencedirect.com/science/article/pii/S0304397509008512)".

## Making SAHA-Tool

1. Install QEPCAD B, and add the full path to qesource/bin to your path.
   Please refer to http://www.cs.usna.edu/~qepcad/B/QEPCAD.html for this step.
2. Install the latest Objective Caml release.
   Please refer to http://caml.inria.fr/ocaml/release.en.html for this step.
3. Clone this repository, move to the `SAHA-Tool` directory and type 

    `./make_SAHA-Tool.sh`

    To run SAHA-Tool use the command

    `./bin/sahaTool`