%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS TIMES LPAREN RPAREN EQ COMMA ARROW LE BAR LBRACKET RBRACKET SEMICOLON CONS AND OR MATCH WITH IN IF THEN ELSE LET REC USCORE FUN EOF

%nonassoc IN
(* %nonassoc MATCH *)
(* %nonassoc WITH *)
%nonassoc ARROW
%nonassoc ELSE
%left BAR
%nonassoc below_COMMA
%left COMMA
%right OR
%right AND
%nonassoc EQ LE
(* %nonassoc LPAREN *)
(* %nonassc RPAREN *)
%left PLUS
%left TIMES
%right CONS
(* %nonassoc VAR *)
(* %nonassoc below_APP *)

%{ open Syntax %}
%start <Syntax.expr> main

%%

let main :=
  | ~ = expr; EOF; <>

let expr :=
  | ~ = simple_expr; <>
  | esec = expr_tuple; { ETuple (List.rev esec) } %prec below_COMMA
  | IF; a = expr; THEN; b = expr; ELSE; c = expr; <EIfThenElse>
  | MATCH; ~ = expr; WITH; ~ = nonempty_list(BAR; p = pattern; ARROW; e = expr; { (p, e) }); <EMatch>
  | LET; ~ = boption(REC); ~ = pattern; EQ; a = expr; IN; b = expr; <ELet>
  | a = expr; CONS; b = expr; <ECons>
  | FUN; ~ = nonempty_list(pattern); ARROW; ~ = expr; <EAbs>
  | f = simple_expr; arg = nonempty_list(simple_expr); <EApp>
  | a = expr; PLUS; b = expr; { EBop (a, BPlus, b) }
  | a = expr; TIMES; b = expr; { EBop (a, BTimes, b) }
  | a = expr; OR; b = expr; { EBop (a, BOr, b) }
  | a = expr; AND; b = expr; { EBop (a, BAnd, b) }
  | a = expr; LE; b = expr; { EBop (a, BLe, b) }
  | a = expr; EQ; b = expr; { EBop (a, BEq, b) }

let simple_expr :=
  | ~ = VAR; <EVar>
  | LBRACKET; ~ = separated_list(SEMICOLON, expr); RBRACKET; <EList>
  | ~ = INT; <EInt>
  | ~ = BOOL; <EBool>
  | LPAREN; ~ = expr; RPAREN; <>
  | LPAREN; RPAREN; { ETuple [] }

let expr_tuple :=
  | es = expr_tuple; COMMA; e = expr; { e :: es }
  | e1 = expr; COMMA; e2 = expr; { [e2; e1] }

let pattern :=
  | ~= simple_pattern; <>
  | psec = pattern_tuple; { PTuple (List.rev psec) } %prec below_COMMA
  | a = pattern; CONS; b = pattern; <PCons>
  | a = pattern; BAR; b = pattern; <POr>

let simple_pattern :=
  | ~ = VAR; <PVar>
  | USCORE; { PUnderscore }
  | LBRACKET; ~ = separated_list(SEMICOLON, pattern); RBRACKET; <PList>
  | ~ = INT; <PInt>
  | ~ = BOOL; <PBool>
  | LPAREN; ~ = pattern; RPAREN; <>
  | LPAREN; RPAREN; { PTuple [] }

let pattern_tuple :=
  | ps = pattern_tuple; COMMA; p = pattern; { p :: ps }
  | p1 = pattern; COMMA; p2 = pattern; { [p2; p1] }

let separated_twoplus_list(sep, X) :=
  | x = X; sep; xs = separated_nonempty_list(sep, X); { x :: xs }
