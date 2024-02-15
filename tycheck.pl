tycheck_expr(o_int(_), _, ty_int).
tycheck_expr(o_bool(_), _, ty_bool).
tycheck_expr(o_var(V), E, T) :- T = E.V.
tycheck_expr(o_if(A, B, C), E, TB) :-
  tycheck_expr(A, E, TA),
  unify_with_occurs_check(TA, bool),
  tycheck_expr(B, E, TB),
  tycheck_expr(C, E, TC),
  unify_with_occurs_check(TB, TC).

tycheck_expr(o_bop(A, "+", B), E, int) :-
  tycheck_expr(A, E, TA),
  tycheck_expr(B, E, TB),
  unify_with_occurs_check(TA, int),
  unify_with_occurs_check(TB, int).
tycheck_expr(o_bop(A, "*", B), E, int) :-
  tycheck_expr(A, E, TA),
  tycheck_expr(B, E, TB),
  unify_with_occurs_check(TA, int),
  unify_with_occurs_check(TB, int).
tycheck_expr(o_bop(A, "=", B), E, bool) :-
  tycheck_expr(A, E, TA),
  tycheck_expr(B, E, TB),
  unify_with_occurs_check(TA, bool),
  unify_with_occurs_check(TB, bool).
tycheck_expr(o_bop(A, "<", B), E, bool) :-
  tycheck_expr(A, E, TA),
  tycheck_expr(B, E, TB),
  unify_with_occurs_check(TA, bool),
  unify_with_occurs_check(TB, bool).
tycheck_expr(o_bop(A, "||", B), E, bool) :-
  tycheck_expr(A, E, TA),
  tycheck_expr(B, E, TB),
  unify_with_occurs_check(TA, bool),
  unify_with_occurs_check(TB, bool).
tycheck_expr(o_bop(A, "&&", B), E, bool) :-
  tycheck_expr(A, E, TA),
  tycheck_expr(B, E, TB),
  unify_with_occurs_check(TA, bool),
  unify_with_occurs_check(TB, bool).

tycheck_expr(o_fun([], B), E, TB) :- tycheck_expr(B, E, TB).
tycheck_expr(o_fun([H|T], B), E, ty_arrow(TH, TR)) :-
  tycheck_patt(H, E, TH, E1),
  tycheck_expr(o_fun(T, B), E1, TR).

tycheck_expr(o_let(false, Patt, Expr, Body), E, TBody) :-
  tycheck_patt(Patt, E, TPatt, E1),
  tycheck_expr(Expr, E, TExpr),
  tycheck_expr(Body, E1, TBody),
  unify_with_occurs_check(TPatt, TExpr).
tycheck_expr(o_let(true, Patt, Expr, Body), E, TBody) :-
  tycheck_patt(Patt, E, TPatt, E1),
  tycheck_expr(Expr, E1, TExpr),
  tycheck_expr(Body, E1, TBody),
  unify_with_occurs_check(TPatt, TExpr).


tycheck_patt(o_int(_), _, ty_int, _).
tycheck_patt(o_bool(_), _, ty_bool, _).
tycheck_patt(o_var(V), E, TV, EV) :-
  TV = _,
  EV = E.put(V, TV).
tycheck_patt(o_cons(X, XS), E, TXS, EXS) :-
  tycheck_patt(X, E, TX, EX),
  tycheck_patt(XS, EX, TXS, EXS),
  unify_with_occurs_check(list(TX), TXS).
tycheck_patt(o_wild, E, Twild, E) :-
  Twild = _.
