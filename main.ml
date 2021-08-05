open Syntax
open Solver

let rec read_solve_print () =
	try
		print_string "# ";
		flush stdout;
		let inf = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
		let res = solve inf in
			(if List.concat res = [] then print_endline "provable." else
				let rec print_res res =
					match res with
					| [] -> ()
					| v :: rest -> 
						if v = [] then print_res rest else
							print_endline (string_of_valuation v); (* 反例モデルを1つprint *)
				in print_res res);
		read_solve_print ()
	with
	| Failure s -> print_endline s; read_solve_print ()
	| Parsing.Parse_error -> print_endline "parse error."; read_solve_print ()

let _ = read_solve_print ()
