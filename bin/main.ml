open Ocaml_gram

let () =
  Format.set_margin 120;
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.main Lexer.token lexbuf in
    ast |> Syntax.show_expr |> print_endline;
    ast |> Prolog_unparse.unparse
  with Parser.Error ->
    Printf.printf "At offset %d: unexpected character."
      (Lexing.lexeme_start lexbuf)
