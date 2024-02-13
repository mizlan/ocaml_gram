The executable works via stdin and stdout.

$ echo 'let x, y = p in x + 2, y + 2' | dune exec ocaml_gram
(Syntax.ELet (false, (Syntax.PTuple [(Syntax.PVar "x"); (Syntax.PVar "y")]),
   (Syntax.EVar "p"),
   (Syntax.ETuple
      [(Syntax.EBop ((Syntax.EVar "x"), Syntax.BPlus, (Syntax.EInt 2)));
        (Syntax.EBop ((Syntax.EVar "y"), Syntax.BPlus, (Syntax.EInt 2)))])
   ))
