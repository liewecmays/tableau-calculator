let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z'] 
let ident = alpha (alpha | digit)*

rule token = parse
| space+ { token lexbuf }
| "|-" { Parser.VDASH }
| "~" { Parser.NOT }
| "/\\" { Parser.AND }
| "\\/" { Parser.OR }
| "->" { Parser.IF }
| "(" { Parser.LPAR }
| ")" { Parser.RPAR }
| "," { Parser.COMMA }
| "." { Parser.PERIOD }
| ident as id { Parser.ID id }
| _ { failwith ("lex error") }