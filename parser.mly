%{
	open Syntax
%}

%token <string> ID
%token LPAR RPAR COMMA PERIOD
%token VDASH
%token NOT AND OR IF

%start toplevel 
%type <(Syntax.formula list * Syntax.formula)> toplevel
%%

toplevel:
	| formula PERIOD { ([], $1) }
	| VDASH formula PERIOD { ([], $2) }
	| formula_list VDASH formula PERIOD { ($1, $3) }
;

formula_list:
	| formula COMMA formula_list { $1 :: $3 }
	| formula { $1 :: [] }
;

formula:
	| formula_or IF formula { FIf ($1, $3) }
	| formula_or { $1 }
;

formula_or:
	| formula_and OR formula_or { FOr ($1, $3) }
	| formula_and { $1 }
;

formula_and:
	| formula_not AND formula_and { FAnd ($1, $3) }
	| formula_not { $1 }
;

formula_not:
	| NOT formula_not { FNot $2 }
	| formula_atomic { $1 }
;

formula_atomic:
	| ID { FVar $1 }
	| LPAR formula RPAR { $2 }
;