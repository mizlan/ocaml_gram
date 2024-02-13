open Ocaml_gram

let%expect_test "commas and plus" =
  let lexbuf = Lexing.from_string "x, fun y -> z + y, 2" in
  try
    Parser.main Lexer.token lexbuf |> Syntax.show_expr |> print_endline;
    [%expect{|
      (Syntax.ETuple
         [(Syntax.EVar "x");
           (Syntax.EAbs ([(Syntax.PVar "y")],
              (Syntax.ETuple
                 [(Syntax.EBop ((Syntax.EVar "z"), Syntax.BPlus, (Syntax.EVar "y")
                     ));
                   (Syntax.EInt 2)])
              ))
           ]) |}]
  with Parser.Error ->
    Printf.printf "At offset %d: unexpected character."
      (Lexing.lexeme_start lexbuf)

let%expect_test "literals" =
  let lexbuf = Lexing.from_string "[] () [1;2,3]" in
  try
    Parser.main Lexer.token lexbuf |> Syntax.show_expr |> print_endline;
    [%expect{|
      (Syntax.EApp ((Syntax.EList []),
         [(Syntax.ETuple []);
           (Syntax.EList
              [(Syntax.EInt 1); (Syntax.ETuple [(Syntax.EInt 2); (Syntax.EInt 3)])])
           ]
         )) |}]
  with Parser.Error ->
    Printf.printf "At offset %d: unexpected character."
      (Lexing.lexeme_start lexbuf)

let%expect_test "normal exprs" =
  let lexbuf = Lexing.from_string "match x with | a :: b -> fun x y -> z, n" in
  try
    Parser.main Lexer.token lexbuf |> Syntax.show_expr |> print_endline;
    [%expect{|
      (Syntax.EMatch ((Syntax.EVar "x"),
         [((Syntax.PCons ((Syntax.PVar "a"), (Syntax.PVar "b"))),
           (Syntax.EAbs ([(Syntax.PVar "x"); (Syntax.PVar "y")],
              (Syntax.ETuple [(Syntax.EVar "z"); (Syntax.EVar "n")]))))
           ]
         )) |}]
  with Parser.Error ->
    Printf.printf "At offset %d: unexpected character."
      (Lexing.lexeme_start lexbuf)
