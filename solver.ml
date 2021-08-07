open Syntax

exception SolveErr

let is_debug_mode =
	try (Sys.argv.(1) = "-d") with
	| Invalid_argument _ -> false

(*
原子論理式(あるいはその否定)のリストを受け取り、以下を返す
- そのリストが矛盾していれば空リスト
- そうでなければ、そのリストに整合する原子論理式への付値
 *)
let find_valuation branch =
	let rec find_valuation_inner branch acc =
		match branch with
		| [] -> acc
		| fml :: rest ->
			match fml with
			| FVar x ->
				if List.mem (FNot (FVar x)) acc then
					[] (* 矛盾したら終わり *)
				else
					if List.mem fml acc then
						find_valuation_inner rest acc (* 既に入っている場合は加えない *)
					else
						find_valuation_inner rest (fml :: acc)
			| FNot (FVar x) ->
				if List.mem (FVar x) acc then
					[]
				else
					if List.mem fml acc then
						find_valuation_inner rest acc
					else
						find_valuation_inner rest (fml :: acc)
			| _ -> raise SolveErr
	in find_valuation_inner branch []

(*
- fml_list: 探索の際に使う論理式の集合, 
- branch: 探索の中で出てきた原子論理式(あるいはその否定)のリスト(tableauが分岐する場合はこれも分岐する)
*)
let level = ref 0
let after_fork = ref false
let rec search_tableau fml_list branch = 
	match fml_list with
	| [] ->
		let v = find_valuation branch in
			(if is_debug_mode then
				if v = [] then
					print_endline_in_tree "→ contradiction" !level
				else
					print_endline_in_tree ("→ valuation found: " ^ string_of_valuation v) !level
			else ());
			[v]
	| fml :: rest ->
		(if is_debug_mode then
			if !after_fork then
				(after_fork := false; print_endline (string_of_formula fml (* ^ ", " ^ string_of_formula_list rest *)))
			else
				print_endline (string_of_formula_in_tree fml !level (* ^ ", " ^ string_of_formula_list rest*))
		else ());
		match fml with
		| FVar _ -> search_tableau rest (fml :: branch) (* 原子論理式の場合はbranchに追加 *)
		| FIf (fml1, fml2) ->
			(if is_debug_mode then
				(print_string_in_tree "- " !level;
				level := !level + 2;
				after_fork := true)
			else ());
			let v1 = search_tableau (FNot fml1 :: rest) branch in
			(if is_debug_mode then
				(level := !level - 2;
				print_string_in_tree "- " !level;
				level := !level + 2;
				after_fork := true)
			else ());
			let v2 = search_tableau (fml2 :: rest) branch in
				(if is_debug_mode then level := !level - 2 else ());
				v1 @ v2
		| FAnd (fml1, fml2) -> search_tableau (fml1 :: fml2 :: rest) branch
		| FOr (fml1, fml2) ->
			(if is_debug_mode then
				(print_string_in_tree "- " !level;
				level := !level + 2;
				after_fork := true)
			else ());
			let v1 = search_tableau (fml1 :: rest) branch in
			(if is_debug_mode then
				(level := !level - 2;
				print_string_in_tree "- " !level;
				level := !level + 2;
				after_fork := true)
			else ());
			let v2 = search_tableau (fml2 :: rest) branch in
				(if is_debug_mode then level := !level - 2 else ());
				v1 @ v2
		| FNot fml' ->
			match fml' with
			| FVar _ -> search_tableau rest (fml :: branch) (* 原子論理式の否定の場合はbranchに追加 *)
			| FIf (fml1, fml2) -> search_tableau (fml1 :: FNot fml2 :: rest) branch
			| FAnd (fml1, fml2) ->
				(if is_debug_mode then
					(print_string_in_tree "- " !level;
					level := !level + 2;
					after_fork := true)
				else ());
				let v1 = search_tableau (FNot fml1 :: rest) branch in
				(if is_debug_mode then
					(level := !level - 2;
					print_string_in_tree "- " !level;
					level := !level + 2;
					after_fork := true)
				else ());
				let v2 = search_tableau (FNot fml2 :: rest) branch in
					(if is_debug_mode then level := !level - 2 else ());
					v1 @ v2
			| FOr (fml1, fml2) -> search_tableau (FNot fml1 :: FNot fml2 :: rest) branch
			| FNot fml'' -> search_tableau (fml'' :: rest) branch

(* 推論を受け取り、反例モデルを返す(妥当な場合は空リスト) *)
let solve inf = 
	match inf with
	| Inf (cn, pr) ->
		let init_list = FNot pr :: cn in
		(if is_debug_mode then
			(print_endline "\n===== search start =====";
			level := 0;
			after_fork := false)
		else ());
		let counter_models = search_tableau init_list [] in
		(if is_debug_mode then
			print_endline "===== search end =====\n"
		else ());
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
		in remove_dup counter_models