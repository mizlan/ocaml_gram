{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| ("true" | "false") as b
    { BOOL (bool_of_string b) }
| '+'
    { PLUS }
| '*'
    { TIMES }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '_'
    { USCORE }
| '='
    { EQ }
| ','
    { COMMA }
| "->"
    { ARROW }
| '<'
    { LE }
| '|'
    { BAR }
| '['
    { LBRACKET }
| ']'
    { RBRACKET }
| ';'
    { SEMICOLON }
| "::"
    { CONS }
| "&&"
    { AND }
| "||"
    { OR }
| "match"
    { MATCH }
| "with"
    { WITH }
| "in"
    { IN }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "rec"
    { REC }
| "fun"
    { FUN }
| (['a'-'z'] ['A'-'Z' '_' 'a'-'z']*) as v
    { VAR v }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character." (Lexing.lexeme_start lexbuf))) }
