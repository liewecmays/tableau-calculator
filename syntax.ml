type formula =
	| FVar of string
	| FNot of formula
	| FAnd of formula * formula
	| FOr of formula * formula
	| FIf of formula * formula

type inference = Inf of formula list * formula (* premises and conclusion *)

(*
- 結合の強さ: not > and > or > if
- and, or, if -> 右結合
- and, orは結合則を満たすので括弧は不要
*)
let rec string_of_formula fml =
	match fml with
	| FVar p -> p
	| FNot fml' ->
		(match fml' with
		| FNot _ | FVar _ -> "¬" ^ string_of_formula fml'
		| _ -> "¬(" ^ string_of_formula fml' ^ ")")
	| FAnd (fml1, fml2) ->
		(match fml1 with
		| FOr (_, _) | FIf (_, _) -> "(" ^ string_of_formula fml1 ^ ")"
		| _ -> string_of_formula fml1)
		^ " ⋀ " ^
		(match fml2 with
		| FOr (_, _) | FIf (_, _) -> "(" ^ string_of_formula fml2 ^ ")"
		| _ -> string_of_formula fml2)
	| FOr (fml1, fml2) ->
		(match fml1 with
		| FIf (_, _) -> "(" ^ string_of_formula fml1 ^ ")"
		| _ -> string_of_formula fml1)
		^ " ⋁ " ^
		(match fml2 with
		| FIf (_, _) -> "(" ^ string_of_formula fml2 ^ ")"
		| _ -> string_of_formula fml2)
	| FIf (fml1, fml2) ->
		(match fml1 with
		| FIf (_, _) -> "(" ^ string_of_formula fml1 ^ ")"
		| _ -> string_of_formula fml1)
		^ " → " ^ string_of_formula fml2

let string_of_formula_list fmls =
	let rec string_of_formula_list_inner fmls flag =
		match fmls with
		| [] -> "]"
		| fml :: rest ->
			if flag then
				string_of_formula fml ^ string_of_formula_list_inner rest false
			else
				", " ^ string_of_formula fml ^ string_of_formula_list_inner rest false
	in "[" ^ string_of_formula_list_inner fmls true

let string_of_inference inf =
	match inf with
	| Inf (pr, cn) -> "primeses: " ^ string_of_formula_list pr ^ ", " ^ "conclusion: " ^ string_of_formula cn

exception StringifyErr
let string_of_valuation v =
	let sort_fun fml1 fml2 =
		match (fml1, fml2) with
		| (FVar x, FVar y) | (FVar x, FNot (FVar y)) | (FNot (FVar x), FVar y) | (FNot (FVar x), FNot (FVar y)) -> String.compare x y
		| _ -> raise StringifyErr
	in let sorted_v = List.sort sort_fun v in
	let rec string_of_valuation_inner v flag =
		match v with
		| [] -> "}"
		| fml :: rest ->
			match fml with
			| FVar x -> if flag then x ^ ":true" ^ string_of_valuation_inner rest false else ", " ^ x ^ ":true" ^ string_of_valuation_inner rest false
			| FNot (FVar x) -> if flag then x ^ ":false" ^ string_of_valuation_inner rest false else ", " ^ x ^ ":false" ^ string_of_valuation_inner rest false
			| _ -> raise StringifyErr
	in "{" ^ string_of_valuation_inner sorted_v true


(* tree-display (for debug) *)
let rec n_space n =
	if n = 0 then "" else " " ^ n_space (n-1)

let print_string_in_tree s level = print_string (n_space level ^ s)

let print_endline_in_tree s level = print_endline (n_space level ^ s)

let string_of_formula_in_tree fml level = n_space level ^ string_of_formula fml
