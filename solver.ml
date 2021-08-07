open Syntax
open Util

(*
原子論理式(あるいはその否定)のリストを受け取り、以下を返す
- そのリストが矛盾していれば、None
- そうでなければ、そのリストに整合する原子論理式への付値
*)
exception SolveErr
let find_valuation branch =
	let rec find_valuation_inner branch acc =
		match branch with
		| [] -> Some acc
		| fml :: rest ->
			match fml with
			| FVar x ->
				if List.mem (x, false) acc then
					None (* 矛盾したら終わり *)
				else
					if List.mem_assoc x acc then (* 既に入っている場合は加えない *)
						find_valuation_inner rest acc
					else
						find_valuation_inner rest ((x, true) :: acc)
			| FNot (FVar x) ->
				if List.mem (x, true) acc then
					None
				else
					if List.mem_assoc x acc then
						find_valuation_inner rest acc
					else
						find_valuation_inner rest ((x, false) :: acc)
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
		let res = find_valuation branch in
			if is_debug_mode then
				match res with
				| Some v -> print_endline_in_tree ("→ valuation found: " ^ string_of_valuation v) !level
				| None -> print_endline_in_tree "→ contradiction" !level
			else ();
			res
	| fml :: rest ->
		(if is_debug_mode then
			if !after_fork then (* "-"記号の直後の場合は、改行しない *)
				(after_fork := false; print_endline (string_of_formula fml (* ^ ", " ^ string_of_formula_list rest *)))
			else
				print_endline (string_of_formula_in_tree fml !level (* ^ ", " ^ string_of_formula_list rest*))
		else ());
		match fml with
		| FVar _ -> search_tableau rest (fml :: branch) (* 原子論理式の場合はbranchに追加 *)
		| FIf (fml1, fml2) ->
			if is_debug_mode then (print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else (); (* 分岐する場合はlevelを深くする *)
			(match search_tableau (FNot fml1 :: rest) branch with (* 前半部分の探索 *)
			| Some v -> Some v (* 付値があった場合は、後半は探索せず付値を返す *)
			| None -> 
				if is_debug_mode then (level := !level - 2; print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else (); (* levelを一度戻し、再度深くする *)
				let res = search_tableau (fml2 :: rest) branch in (* 後半部分の探索 *)
					if is_debug_mode then level := !level - 2 else (); (* levelを戻す *)
					res)
		| FAnd (fml1, fml2) -> search_tableau (fml1 :: fml2 :: rest) branch
		| FOr (fml1, fml2) ->
			if is_debug_mode then (print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
			(match search_tableau (fml1 :: rest) branch with
			| Some v -> Some v
			| None -> 
				if is_debug_mode then (level := !level - 2; print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
				let res = search_tableau (fml2 :: rest) branch in
					if is_debug_mode then level := !level - 2 else ();
					res)
		| FNot fml' -> (* 否定記号から始まる場合 *)
			match fml' with
			| FVar _ -> search_tableau rest (fml :: branch)
			| FIf (fml1, fml2) -> search_tableau (fml1 :: FNot fml2 :: rest) branch
			| FAnd (fml1, fml2) ->
				if is_debug_mode then (print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
				(match search_tableau (FNot fml1 :: rest) branch with
				| Some v -> Some v
				| None -> 
					if is_debug_mode then (level := !level - 2; print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
					let res = search_tableau (FNot fml2 :: rest) branch in
						if is_debug_mode then level := !level - 2 else ();
						res)
			| FOr (fml1, fml2) -> search_tableau (FNot fml1 :: FNot fml2 :: rest) branch
			| FNot fml'' -> search_tableau (fml'' :: rest) branch

(* 推論を受け取り、反例モデルを1つ返す(証明可能な場合は空リスト) *)
let solve inf = 
	match inf with
	| Inf (cn, pr) ->
		let init_list = FNot pr :: cn in
		let vars = get_vars_list init_list in
		if is_debug_mode then (print_endline "\n===== search start ====="; level := 0; after_fork := false) else (); (* 変数の初期化 *)
		let counter_models =
			match search_tableau init_list [] with
			| Some v -> v
			| None -> []
		in if is_debug_mode then print_endline "===== search end =====\n" else ();
		fill_false (remove_dup counter_models) vars
