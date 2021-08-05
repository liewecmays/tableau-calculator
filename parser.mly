%{
  open Syntax
%}

%token <string> ID
%token LPAR RPAR COMMA PERIOD
%token VDASH
%token NOT AND OR IF

%left AND
%left OR
%right IF

%start toplevel 
%type <Syntax.inference> toplevel
%%

toplevel:
	| VDASH formula PERIOD { Inf ([], $2) }
	| formula_list VDASH formula PERIOD { Inf ($1, $3) }
;

formula_list:
	| formula COMMA formula_list { $1 :: $3 }
	| formula { $1 :: [] }
;

formula:
	| ID { FVar $1 }
	| NOT formula { FNot $2 }
	| formula AND formula { FAnd ($1, $3) }
	| formula OR formula { FOr ($1, $3) }
	| formula IF formula { FIf ($1, $3) }
	| LPAR formula RPAR { $2 }
;