open Syntax

(* コマンドラインで"-d"が指定された場合、デバッグモード(タブローの探索の様子などを示す) *)
let is_debug_mode =
	try (Sys.argv.(1) = "-d") with
	| Invalid_argument _ -> false


(* 様相演算子が含まれるか否かを判定 *)
let rec is_modal fml =
	match fml with
	| FVar _ -> false
	| FBox _ | FDia _ -> true
	| FNot fml' -> is_modal fml'
	| FAnd (fml1, fml2) | FOr (fml1, fml2) | FIf (fml1, fml2) -> is_modal fml1 || is_modal fml2

let rec is_modal_list fml_list =
	match fml_list with
	| [] -> false
	| fml :: rest -> is_modal fml || is_modal_list rest


(* 付値を世界ごとに分割 *)
let divide_valuation v w =
	let rec filter_valuation w v =
		match v with
		| [] -> []
		| ((w', x), b) :: rest -> if w = w' then (x, b) :: filter_valuation w rest else filter_valuation w rest
	in let rec divide_valuation_inner v worlds =
		match worlds with
		| [] -> []
		| w :: rest -> (w, filter_valuation w v) :: divide_valuation_inner v rest
	in divide_valuation_inner v w


(* string_of_系 *)
(*
- 結合の強さ: not > and > or > if
- and, or, if -> 右結合
- and, orは結合則を満たすので括弧は不要
*)
let rec n_space n =
	if n = 0 then "" else " " ^ n_space (n-1)

let rec string_of_formula fml =
	match fml with
	| FVar p -> p
	| FNot fml' ->
		(match fml' with
		| FVar _ | FNot _ | FBox _ | FDia _ -> "¬" ^ string_of_formula fml'
		| _ -> "¬(" ^ string_of_formula fml' ^ ")")
	| FBox fml' ->
		(match fml' with
		| FVar _ | FNot _ | FBox _ | FDia _ -> "□" ^ string_of_formula fml'
		| _ -> "□(" ^ string_of_formula fml' ^ ")")
	| FDia fml' ->
		(match fml' with
		| FVar _ | FNot _ | FBox _ | FDia _ -> "◊" ^ string_of_formula fml'
		| _ -> "◊(" ^ string_of_formula fml' ^ ")")
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
			(if flag then "" else ", ") ^ string_of_formula fml ^ string_of_formula_list_inner rest false
	in "[" ^ string_of_formula_list_inner fmls true

let string_of_valuation_debug v =
	let rec string_of_valuation_debug_inner v flag =
		match v with
		| [] -> "}"
		| (x, b) :: rest ->
			(if flag then "" else ", ") ^ x ^ ":" ^ string_of_bool b ^ string_of_valuation_debug_inner rest false
	in "{" ^ string_of_valuation_debug_inner (List.sort (fun (x, _) (y, _) -> String.compare x y) v) true

let string_of_valuation v vars =
	let rec string_of_valuation_inner v vars flag =
		match vars with
		| [] -> "}"
		| var :: rest ->
			(if flag then "" else ", ") ^ var ^ ":" ^
			(try string_of_bool (List.assoc var v) with Not_found -> "false") (* 指定されていない変数にはfalseを割り当てる *)
			^ string_of_valuation_inner v rest false
	in "{" ^ string_of_valuation_inner v vars true

let rec string_of_valuation_in_table v vars =
	match vars with
	| [] -> ""
	| var :: rest ->
		(let n = String.length var in
		try 
			(if List.assoc var v then " \x1b[1mt\x1b[0m"  else " f") ^ n_space n
		with Not_found -> " f" ^ n_space n) (* 指定されていない変数にはfalseを割り当てる *)
		^ string_of_valuation_in_table v rest

let string_of_worlds ws =
	let rec string_of_worlds_inner ws flag =
		match ws with
		| [] -> "}"
		| w :: rest ->
			(if flag then "" else ", ") ^ "w" ^ string_of_int w ^ string_of_worlds_inner rest false
	in "{" ^ string_of_worlds_inner ws true

let rec string_of_relation r =
	let rec string_of_relation_inner ws flag =
		match ws with
		| [] -> "}"
		| (i, j) :: rest ->
			(if flag then "" else ", ") ^ "w" ^ string_of_int i ^ " → w" ^ string_of_int j ^ string_of_relation_inner rest false
	in "{" ^ string_of_relation_inner r true

let rec string_of_variables_space vars =
	match vars with
	| [] -> ""
	| var :: rest -> " " ^ var ^ " " ^ string_of_variables_space rest

exception StringifyErr
let string_of_model (w, r, v, vars) mode =
	let vs = divide_valuation v w in
	match mode with
	| Classical ->
		(match vs with
		| [(_, v)] -> string_of_valuation v vars
		| _ -> raise StringifyErr)
	| Modal ->
		"- worlds: " ^ string_of_worlds w ^ "\n" ^
		"- relation: " ^ string_of_relation r ^ "\n" ^
		"- valuation: \n" ^
		"  " ^ "\x1b[4m   " ^
		let n = int_of_float (floor (log10 (float_of_int (List.length w - 1)))) in n_space n
		^ "|" ^ string_of_variables_space vars ^ " \x1b[0m\n" ^
		let rec string_of_model_modal vs flag =
			match vs with
			| [] -> ""
			| (w, v) :: rest ->
				(if flag then "" else "\n") ^ "  w" ^ string_of_int w ^ " |" ^ string_of_valuation_in_table v vars ^ string_of_model_modal rest false
		in string_of_model_modal vs true
	| Debug ->
		let rec string_of_model_debug vs flag =
			match vs with
			| [] -> ""
			| (w, v) :: rest ->
				(if flag then "" else ", ") ^ "w" ^ string_of_int w ^ ":" ^ string_of_valuation_debug v ^ string_of_model_debug rest false
		in string_of_model_debug vs true


(* タブローをツリー状に表示する際に使う(デバッグ用) *)
let print_string_in_tree s level = print_string (n_space level ^ s)

let print_endline_in_tree s level = print_endline (n_space level ^ s)

let string_of_formula_in_tree fml level = n_space level ^ string_of_formula fml


(* 含まれる命題変数をリストにして返す *)
let rec get_vars fml =
	match fml with
	| FVar x -> [x]
	| FNot fml' | FBox fml' | FDia fml' -> get_vars fml'
	| FAnd (fml1, fml2) | FOr (fml1, fml2) | FIf (fml1, fml2) -> get_vars fml1 @ get_vars fml2

let rec get_vars_list fml_list =
	match fml_list with
	| [] -> []
	| fml :: rest -> get_vars fml @ get_vars_list rest


(* 付値で定まっていない付値に対してfalseを割り当てる *)
let rec fill_false v vars =
	if v = [] then [] else
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
