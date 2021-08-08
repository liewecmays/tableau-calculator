open Syntax
open Util

(*
命題変数,世界,その真偽の組のリストを受け取り、以下を返す
- そのリストが矛盾していれば、None
- そうでなければ、そのリストに整合する原子論理式への付値
*)
let find_valuation branch =
	let rec find_valuation_inner branch acc =
		match branch with
		| [] -> Some acc
		| (x, w, b) :: rest ->
			if List.mem ((w, x), not b) acc then
				None (* 矛盾したら終わり *)
			else
				if List.mem_assoc (w, x) acc then (* 既に入っている場合は加えない *)
					find_valuation_inner rest acc
				else
					find_valuation_inner rest (((w, x), b) :: acc)
	in find_valuation_inner branch []


(* 新しい論理式と世界に対して、到達可能性関係をもとに新しく作ることができるノードを列挙する *)
let rec create_nodes_from_formula (fml, world) relation =
	match relation with
	| [] -> []
	| (i, j) :: rest -> if i = world then (fml, j) :: create_nodes_from_formula (fml, world) rest else create_nodes_from_formula (fml, world) rest

(* 新しい到達可能性関係に対して、論理式と世界のリストをもとに新しく作ることができるノードを列挙する *)
let rec create_nodes_from_world (world, new_world) boxes =
	match boxes with
	| [] -> []
	| (fml, i) :: rest -> if i = world then (fml, new_world) :: create_nodes_from_world (world, new_world) rest else create_nodes_from_world (world, new_world) rest

(*
- node_list: 探索の際に使う論理式と世界の組のリスト
- worlds: 探索の中で出てきた世界のリスト
- relation: 探索の中で出てきた到達可能性関係のリスト
- boxes: 「Boxが一番外側にある論理式の中身」と世界の組のリスト(探索の中で保存)
- branch: 探索の中で出てきた原子論理式(あるいはその否定)と世界の組のリスト(タブローが分岐する場合はこれも分岐する)
*)
let init_world = 0
let level = ref 0
let after_fork = ref false
let world_count = ref init_world
let rec search_tableau node_list worlds relation boxes branch =
	match node_list with
	| [] ->
		let res = find_valuation branch in
		(match res with
		| Some v ->
			if is_debug_mode then print_endline_in_tree ("→ valuation found: " ^ string_of_model (worlds, relation, v, []) Debug) !level else ();
			Some (worlds, relation, v)
		| None ->
			if is_debug_mode then print_endline_in_tree "→ contradiction" !level else ();
			None)
	| (fml, world) :: rest ->
		(if is_debug_mode then
			if !after_fork then (* "-"記号の直後の場合は、改行しない *)
				(after_fork := false; print_endline (string_of_formula fml ^ ", " ^ string_of_int world (* ^ ", " ^ string_of_formula_list rest *)))
			else
				print_endline (string_of_formula_in_tree fml !level ^ ", " ^ string_of_int world (* ^ ", " ^ string_of_formula_list rest *))
		else ());
		match fml with
		| FVar x -> search_tableau rest worlds relation boxes ((x, world, true) :: branch) (* 原子論理式の場合はbranchに追加 *)
		| FIf (fml1, fml2) ->
			if is_debug_mode then (print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else (); (* 分岐する場合はlevelを深くする *)
			(match search_tableau ((FNot fml1, world) :: rest) worlds relation boxes branch with (* 前半部分の探索 *)
			| Some v -> Some v (* 付値があった場合は、後半は探索せず付値を返す *)
			| None -> 
				if is_debug_mode then (level := !level - 2; print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else (); (* levelを一度戻し、再度深くする *)
				let res = search_tableau ((fml2, world) :: rest) worlds relation boxes branch in (* 後半部分の探索 *)
					if is_debug_mode then level := !level - 2 else (); (* levelを戻す *)
					res)
		| FAnd (fml1, fml2) -> search_tableau ((fml1, world) :: (fml2, world) :: rest) worlds relation boxes branch
		| FOr (fml1, fml2) ->
			if is_debug_mode then (print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
			(match search_tableau ((fml1, world) :: rest) worlds relation boxes branch with
			| Some v -> Some v
			| None -> 
				if is_debug_mode then (level := !level - 2; print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
				let res = search_tableau ((fml2, world) :: rest) worlds relation boxes branch in
					if is_debug_mode then level := !level - 2 else ();
					res)
		| FBox fml' ->
			let new_nodes = create_nodes_from_formula (fml', world) relation in (* この論理式と世界に対して到達可能性関係をもとにノードを増やす *)
				search_tableau (new_nodes @ rest) worlds relation ((fml', world) :: boxes) branch
		| FDia fml' ->
			world_count := !world_count + 1;
			let new_world = !world_count in (* 新しい世界を導入 *)
			let new_relation = (world, new_world) in
			if is_debug_mode then print_endline (string_of_int world ^ "r" ^ string_of_int new_world) else ();
			let new_nodes = create_nodes_from_world new_relation boxes in (* 新しい到達可能性関係に対応してBoxをもとにノードを増やす *)
			let res = search_tableau (((fml', new_world) :: new_nodes) @ rest) (new_world :: worlds) (new_relation :: relation) boxes branch in
				world_count := !world_count - 1;
				res
		| FNot fml' -> (* 否定記号から始まる場合 *)
			match fml' with
			| FVar x -> search_tableau rest worlds relation boxes ((x, world, false) :: branch)
			| FIf (fml1, fml2) -> search_tableau ((fml1, world) :: (FNot fml2, world) :: rest) worlds relation boxes branch
			| FAnd (fml1, fml2) ->
				if is_debug_mode then (print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
				(match search_tableau ((FNot fml1, world) :: rest) worlds relation boxes branch with
				| Some v -> Some v
				| None -> 
					if is_debug_mode then (level := !level - 2; print_string_in_tree "- " !level; level := !level + 2; after_fork := true) else ();
					let res = search_tableau ((FNot fml2, world) :: rest) worlds relation boxes branch in
						if is_debug_mode then level := !level - 2 else ();
						res)
			| FOr (fml1, fml2) -> search_tableau ((FNot fml1, world) :: (FNot fml2, world) :: rest) worlds relation boxes branch
			| FNot fml'' -> search_tableau ((fml'', world) :: rest) worlds relation boxes branch
			| FBox fml'' -> search_tableau ((FDia (FNot fml''), world) :: rest) worlds relation boxes branch
			| FDia fml'' -> search_tableau ((FBox (FNot fml''), world) :: rest) worlds relation boxes branch

(* 推論を受け取り、反例モデルを1つ返す(証明可能な場合は空リスト) *)
let solve (premises, conclusion) =
	let init_list =
		let rec make_init_list fml_list =
			match fml_list with
			| [] -> []
			| fml :: rest -> (fml, init_world) :: make_init_list rest
		in make_init_list (FNot conclusion :: premises)
	in if is_debug_mode then (print_endline "\n\x1b[1m===== search start =====\x1b[0m"; level := 0; after_fork := false) else (); (* 変数の初期化 *)
	let counter_model =
		match search_tableau init_list [init_world] [] [] [] with
			| Some (w, r, v) ->
				let w' = List.sort compare w in
				let r' = List.sort (fun (i, j) (i', j') ->
					let c = compare i i' in
						if c = 0 then compare j j' else c	
				) r in
				let vars = List.sort String.compare (remove_dup (get_vars_list (conclusion :: premises))) in
					(w', r', v, vars)
			| None -> ([], [], [], [])
	in if is_debug_mode then print_endline "\x1b[1m===== search end =====\x1b[0m\n" else ();
	counter_model
