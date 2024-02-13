open Ocaml_gram

let () =
  let lexbuf = Lexing.from_channel stdin in
  try Parser.main Lexer.token lexbuf |> Syntax.show_expr |> print_endline
  with Parser.Error ->
    Printf.printf "At offset %d: unexpected character."
      (Lexing.lexeme_start lexbuf)
