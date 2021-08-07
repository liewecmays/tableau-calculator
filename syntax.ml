type formula =
	| FVar of string
	| FNot of formula
	| FAnd of formula * formula
	| FOr of formula * formula
	| FIf of formula * formula

type inference = Inf of formula list * formula (* premises and conclusion *)
