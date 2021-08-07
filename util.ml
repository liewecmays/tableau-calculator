open Syntax

(* コマンドラインで"-d"が指定された場合、デバッグモード(タブローの探索の様子などを示す) *)
let is_debug_mode =
	try (Sys.argv.(1) = "-d") with
	| Invalid_argument _ -> false


(* string_of_系 *)
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

let string_of_valuation v =
	let rec string_of_valuation_inner v flag =
		match v with
		| [] -> "}"
		| (x, b) :: rest ->
			if flag then
				x ^ ":" ^ string_of_bool b ^ string_of_valuation_inner rest false
			else
				", " ^ x ^ ":" ^ string_of_bool b ^ string_of_valuation_inner rest false
	in "{" ^ string_of_valuation_inner (List.sort (fun (x, _) (y, _) -> String.compare x y) v) true


(* タブローをツリー状に表示する際に使う(デバッグ用) *)
let rec n_space n =
	if n = 0 then "" else " " ^ n_space (n-1)

let print_string_in_tree s level = print_string (n_space level ^ s)

let print_endline_in_tree s level = print_endline (n_space level ^ s)

let string_of_formula_in_tree fml level = n_space level ^ string_of_formula fml


(* 含まれる命題変数をリストにして返す *)
let rec get_vars fml =
	match fml with
	| FVar x -> [x]
	| FNot fml' -> get_vars fml'
	| FAnd (fml1, fml2) | FOr (fml1, fml2) | FIf (fml1, fml2) -> get_vars fml1 @ get_vars fml2

let rec get_vars_list fml_list =
	match fml_list with
	| [] -> []
	| fml :: rest -> get_vars fml @ get_vars_list rest


(* 付値で定まっていない付値に対してfalseを割り当てる *)
let rec fill_false v vars =
	match vars with
	| [] -> v
	| x :: rest -> if List.mem_assoc x v then fill_false v rest else (x, false) :: fill_false v rest


(* リストの重複する要素を除いて返す *)
let remove_dup list =
	let rec remove_dup_inner list acc =
		match list with
		| [] -> acc
		| e :: rest ->
			if List.mem e acc then
				remove_dup_inner rest acc
			else
				remove_dup_inner rest (e :: acc)
	in remove_dup_inner list []
