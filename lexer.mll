{
  open Parser
}

let var = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let int = ['0'-'9'] ['0'-'9']*
let str = ['"'] [^'\n''"']* ['"']
let ws = [' ' '\t' '\r' '\n']

rule token = parse
| '\n'     { Lexing.new_line lexbuf; token lexbuf }
| ws       { token lexbuf }
| "//"     { sl_comment lexbuf }
| "/*"     { ml_comment 0 lexbuf }
| int as n { INT(int_of_string n) }
| str as s { STR(String.sub s 1 (String.length s - 2)) }
| "true"   { TRUE }
| "false"  { FALSE }
| "fun"    { FUN }
| "->"     { ARROW }
| "let"    { LET }
| "rec"    { REC }
| "in"     { IN }
| "+"      { ADD }
| "-"      { SUB }
| "*"      { MUL }
| "/"      { DIV }
| "%"      { MOD }
| "&&"     { AND }
| "||"     { OR }
| "<"      { LT }
| "<="     { LE }
| ">="     { GE }
| ">"      { GT }
| "="      { EQ }
| "<>"     { NE }
| "^"      { CAT }
| ":="     { ASSIGN }
| "not"    { NOT }
| "!"      { DEREF }
| "ref"    { REF }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "spawn"  { SPAWN }
| "match"  { MATCH }
| "|"      { CASE }
| "with"   { WITH }
| "::"     { CONS }
| "await"  { AWAIT }
| "send"   { SEND }
| "to"     { TO }
| "recv"   { RECV }
| "join"   { JOIN }
| "pick"   { PICK }
| "return" { RETURN }
| "("      { LPAREN }
| ")"      { RPAREN }
| "["      { LBRACK }
| "]"      { RBRACK }
| "begin"  { BEGIN }
| "end"    { END }
| ","      { COMMA }
| ";"      { SEMICOLON }
| "_"      { WILDCARD }
| var as v { VAR(v) }
| eof      { EOF }

and sl_comment = parse
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { sl_comment lexbuf }

and ml_comment depth = parse
| "/*" { ml_comment (depth + 1) lexbuf }
| "*/" { if depth = 0 then token lexbuf else ml_comment (depth - 1) lexbuf }
| _    { ml_comment depth lexbuf }
