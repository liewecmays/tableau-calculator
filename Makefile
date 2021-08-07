SOURCES = syntax.ml lexer.mll parser.mly util.ml solver.ml main.ml
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile
