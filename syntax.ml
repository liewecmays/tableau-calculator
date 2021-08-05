type formula =
	| FVar of string
	| FNot of formula
	| FAnd of formula * formula
	| FOr of formula * formula
	| FIf of formula * formula

type inference = Inf of formula list * formula (* premisses and conclusion *)

let rec string_of_formula fml =
	match fml with
	| FVar p -> p
	| FNot fml' -> "¬" ^ string_of_formula fml'
	| FAnd (fml1, fml2) -> "(" ^ string_of_formula fml1 ^ " ⋀ " ^ string_of_formula fml2 ^ ")"
	| FOr (fml1, fml2) -> "(" ^ string_of_formula fml1 ^ " ⋁ " ^ string_of_formula fml2 ^ ")"
	| FIf (fml1, fml2) -> "(" ^ string_of_formula fml1 ^ " → " ^ string_of_formula fml2 ^ ")"

let string_of_formula_list fmls =
	let rec string_of_formula_list_inner fmls flag =
		match fmls with
		| [] -> ""
		| fml :: rest ->
			if flag then
				string_of_formula fml ^ string_of_formula_list_inner rest false
			else
				", " ^ string_of_formula fml ^ string_of_formula_list_inner rest false
	in string_of_formula_list_inner fmls true

let string_of_inference inf =
	match inf with
	| Inf (pr, cn) -> string_of_formula_list pr ^ " |- " ^ string_of_formula cn

exception StringifyErr
let string_of_valuation v =
	let rec string_of_valuation_inner v flag =
		match v with
		| [] -> ""
		| fml :: rest ->
			match fml with
			| FVar x -> if flag then x ^ ":true" ^ string_of_valuation_inner rest false else ", " ^ x ^ ":true" ^ string_of_valuation_inner rest false
			| FNot (FVar x) -> if flag then x ^ ":false" ^ string_of_valuation_inner rest false else ", " ^ x ^ ":false" ^ string_of_valuation_inner rest false
			| _ -> raise StringifyErr
	in string_of_valuation_inner v true