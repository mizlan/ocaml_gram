The executable works via stdin and stdout.

$ cat some_input.txt
let rec short_fold = fun f -> fun init -> fun list -> match list with
  | x :: xs -> let ( cont , acc ) = f init x in
    if cont then short_fold f acc xs else acc
  | _ -> init
in
  let prod = short_fold ( fun acc x -> ( not ( x = 0 ) , acc * x ) ) 1
  in
    prod [ 3 ; 4 ; 5 ; 0 ; 6 ; 7 ]
$ dune exec ocaml_gram < some_input.txt
(Syntax.ELet (true, (Syntax.PVar "short_fold"),
   (Syntax.EAbs ([(Syntax.PVar "f")],
      (Syntax.EAbs ([(Syntax.PVar "init")],
         (Syntax.EAbs ([(Syntax.PVar "list")],
            (Syntax.EMatch ((Syntax.EVar "list"),
               [((Syntax.PCons ((Syntax.PVar "x"), (Syntax.PVar "xs"))),
                 (Syntax.ELet (false,
                    (Syntax.PTuple
                       [(Syntax.PVar "cont"); (Syntax.PVar "acc")]),
                    (Syntax.EApp ((Syntax.EVar "f"),
                       [(Syntax.EVar "init"); (Syntax.EVar "x")])),
                    (Syntax.EIfThenElse ((Syntax.EVar "cont"),
                       (Syntax.EApp ((Syntax.EVar "short_fold"),
                          [(Syntax.EVar "f"); (Syntax.EVar "acc");
                            (Syntax.EVar "xs")]
                          )),
                       (Syntax.EVar "acc")))
                    )));
                 (Syntax.PUnderscore, (Syntax.EVar "init"))]
               ))
            ))
         ))
      )),
   (Syntax.ELet (false, (Syntax.PVar "prod"),
      (Syntax.EApp ((Syntax.EVar "short_fold"),
         [(Syntax.EAbs ([(Syntax.PVar "acc"); (Syntax.PVar "x")],
             (Syntax.ETuple
                [(Syntax.EApp ((Syntax.EVar "not"),
                    [(Syntax.EBop ((Syntax.EVar "x"), Syntax.BEq,
                        (Syntax.EInt 0)))
                      ]
                    ));
                  (Syntax.EBop ((Syntax.EVar "acc"), Syntax.BTimes,
                     (Syntax.EVar "x")))
                  ])
             ));
           (Syntax.EInt 1)]
         )),
      (Syntax.EApp ((Syntax.EVar "prod"),
         [(Syntax.EList
             [(Syntax.EInt 3); (Syntax.EInt 4); (Syntax.EInt 5);
               (Syntax.EInt 0); (Syntax.EInt 6); (Syntax.EInt 7)])
           ]
         ))
      ))
   ))

o_let(true, o_var(short_fold),
  o_fun([o_var(f)],
    o_fun([o_var(init)],
      o_fun([o_var(list)],
        o_match(o_var(list), [
          o_branch(o_cons(o_var(x), o_var(xs)),
            o_let(false, o_tup([o_var(cont), o_var(acc)]),
              o_app(o_var(f), o_var(init), o_var(x)),
              o_if(o_var(cont),
                o_app(o_var(short_fold), o_var(f), o_var(acc), o_var(xs)),
                o_var(acc)))),
          o_branch(o_wild, o_var(init))])))),
  o_let(false, o_var(prod),
    o_app(o_var(short_fold),
      o_fun([o_var(acc), o_var(x)],
        o_tup([
          o_app(o_var(not), o_bop(o_var(x), "=", o_int(0))),
          o_bop(o_var(acc), "*", o_var(x))])),
      o_int(1)),
    o_app(o_var(prod),
      o_list([o_int(3), o_int(4), o_int(5), o_int(0), o_int(6), o_int(7)]))))
