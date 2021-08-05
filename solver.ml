open Syntax

exception SolveErr

(*
原子論理式(あるいはその否定)のリストを受け取り、その中に矛盾があるかどうかを判定し、以下を返す
- 矛盾があれば空リスト
- 矛盾がなければ、原子論理式への付値
 *)
let is_closed branch =
	let rec is_closed_inner branch acc =
		match branch with
		| [] -> acc
		| fml :: rest ->
			match fml with
			| FVar x ->
				if List.mem (FNot (FVar x)) acc then
					[] (* 矛盾したら終わり *)
				else
					if List.mem fml acc then
						is_closed_inner rest acc (* 既に入っている場合は加えない *)
					else
						is_closed_inner rest (fml :: acc)
			| FNot (FVar x) ->
				if List.mem (FVar x) acc then
					[]
				else
					if List.mem fml acc then
						is_closed_inner rest acc
					else
						is_closed_inner rest (fml :: acc)
			| _ -> raise SolveErr
	in is_closed_inner branch []

(*
	fml_list: 探索の際に使う論理式の集合, 
	branch: 探索の中で出てきた原子論理式(あるいはその否定)のリスト(tableauが分岐する場合はこれも分岐する)
*)
let rec search_tableau fml_list branch = 
	match fml_list with
	| [] -> [is_closed branch]
	| fml :: rest ->
		match fml with
		| FVar _ -> search_tableau rest (fml :: branch) (* 原子論理式の場合はbranchに追加 *)
		| FIf (fml1, fml2) -> search_tableau (FNot fml1 :: rest) branch @ search_tableau (fml2 :: rest) branch
		| FAnd (fml1, fml2) -> search_tableau (fml1 :: fml2 :: rest) branch
		| FOr (fml1, fml2) -> search_tableau (fml1 :: rest) branch @ search_tableau (fml2 :: rest) branch
		| FNot fml' ->
			match fml' with
			| FVar _ -> search_tableau rest (fml :: branch) (* 原子論理式の否定の場合はbranchに追加 *)
			| FIf (fml1, fml2) -> search_tableau (fml1 :: FNot fml2 :: rest) branch
			| FAnd (fml1, fml2) -> search_tableau (FNot fml1 :: rest) branch @ search_tableau (FNot fml2 :: rest) branch
			| FOr (fml1, fml2) -> search_tableau (FNot fml1 :: FNot fml2 :: rest) branch
			| FNot fml'' -> search_tableau (fml'' :: rest) branch

let solve inf = 
	match inf with
	| Inf (cn, pr) ->
		let init_list = FNot pr :: cn in
		let counter_models = search_tableau init_list [] in
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