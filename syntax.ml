type formula =
	| FVar of string
	| FNot of formula
	| FAnd of formula * formula
	| FOr of formula * formula
	| FIf of formula * formula
	| FBox of formula
	| FDia of formula

type model = (int list * (int * int) list * ((int * string) * bool) list * string list) (* 世界の集合, 到達可能性関係, 付値, 命題変数の集合 *)

type mode = Classical | Modal | Debug
