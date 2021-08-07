open Syntax
open Util
open Solver

let rec read_solve_print () =
	try
		print_string "# ";
		flush stdout;
		let (premises, conclusion) = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
		let v = solve (premises, conclusion) in
			if v = [] then print_endline "provable." else
				(print_endline "not provable.";
				print_string "counter-model: ";
				print_endline (string_of_valuation v));
		read_solve_print ()
	with
	| Failure s -> print_endline s; read_solve_print ()
	| Parsing.Parse_error -> print_endline "parse error."; read_solve_print ()

let _ = read_solve_print ()
