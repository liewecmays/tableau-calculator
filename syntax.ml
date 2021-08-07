type formula =
	| FVar of string
	| FNot of formula
	| FAnd of formula * formula
	| FOr of formula * formula
	| FIf of formula * formula
	| FBox of formula
	| FDia of formula

type valuation = ((int * string) * bool) list (* 世界と命題変数に対して真偽値を割り当てる *)
type model = (valuation * (int * int) list) (* 付値と到達可能性関係の組 *)

type mode = Classical | Modal | Debug
