open Syntax
open Util
open Solver

let rec read_solve_print () =
	try
		print_string "# ";
		flush stdout;
		let (premises, conclusion) = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
		(if is_modal_list (conclusion :: premises) then
			let counter_model = solve (premises, conclusion) in
				if counter_model = [] then print_endline "modal logic: \x1b[1mprovable\x1b[0m" else
					(print_endline "modal logic: \x1b[1m\x1b[31mnot provable\x1b[0m";
					print_string "counter-model: ";
					print_endline (string_of_valuation counter_model Modal));
		else
			let counter_model = solve (premises, conclusion) in
				if counter_model = [] then print_endline "classical logic: \x1b[1mprovable\x1b[0m" else
					(print_endline "classical logic: \x1b[1m\x1b[31mnot provable\x1b[0m";
					print_string "counter-model: ";
					print_endline (string_of_valuation counter_model Classical)));
		read_solve_print ()
	with
	| Failure s -> print_endline s; read_solve_print ()
	| Parsing.Parse_error -> print_endline "parse error."; read_solve_print ()

let _ = read_solve_print ()
