open CCFormat
open Syntax

let pp_pl_list pp =
  CCList.pp ~pp_start:(return "[@,") ~pp_stop:(return "]") pp

let pp_pl_list_nb pp =
  CCList.pp ~pp_start:(return "[@,") ~pp_stop:(return "]") pp

let rec pp_expr f = function
  | EVar v -> fprintf f "ovar(%a)" string v
  | EInt i -> fprintf f "oint(%a)" int i
  | EBool b -> fprintf f "obool(%a)" bool b
  | EIfThenElse (c, a, b) ->
      fprintf f "@[oif(@,%a,@ %a,@ %a@,)@]" pp_expr c pp_expr a pp_expr b
  | EList l -> fprintf f "@[<hv 2>olist(@,%a@,)@]" (pp_pl_list pp_expr) l
  | ETuple t -> fprintf f "@[<hv 2>otup(%a)@]" (pp_pl_list pp_expr) t
  | EAbs (pl, e) ->
      fprintf f "@[<hv 2>@[<hv 2>ofun(%a@],@ %a)@]" (pp_pl_list pp_patt) pl
        pp_expr e
  | EBop (a, o, b) ->
      fprintf f "@[obop(@,%a,@ %a,@ %a@,)@]" pp_expr a pp_bop o pp_expr b
  | ECons (x, xs) ->
      fprintf f "@[<hv 2>ocons(@,%a,@ %a@,)@]" pp_expr x pp_expr xs
  | ELet (r, p, e, b) ->
      fprintf f "@[<hov 2>olet(%a,@ %a,@ %a,@ %a@,)@]" bool r pp_patt p pp_expr
        e pp_expr b
  | EMatch (e, pl) ->
      fprintf f "@[<v 2>omatch(%a, %a)@]" pp_expr e
        (pp_pl_list (pp_match pp_patt pp_expr))
        pl
  | EApp (fn, xs) ->
      fprintf f "@[<hv 2>oapp(%a,@ %a)@]" pp_expr fn (CCList.pp pp_expr) xs

and pp_match p e f (a, b) = fprintf f "@[<hv 2>obranch(%a,@ %a)@]" p a e b

and pp_patt f = function
  | PVar v -> fprintf f "ovar(%a)" string v
  | PInt i -> fprintf f "oint(%a)" int i
  | PBool b -> fprintf f "obool(%a)" bool b
  | PUnderscore -> fprintf f "o_"
  | PTuple t -> fprintf f "@[<hv 2>otup(@,%a@,)@]" (pp_pl_list pp_patt) t
  | POr (a, b) -> fprintf f "@[oor(%a,@ %a)@]" pp_patt a pp_patt b
  | PCons (x, xs) ->
      fprintf f "@[<hv 2>ocons(@,%a,@ %a@,)@]" pp_patt x pp_patt xs
  | PList l -> fprintf f "@[olist(%a)@]" (pp_pl_list pp_patt) l

and pp_bop f = function
  | BEq -> fprintf f "%a" string_quoted "="
  | BLe -> fprintf f "%a" string_quoted "<"
  | BOr -> fprintf f "%a" string_quoted "||"
  | BAnd -> fprintf f "%a" string_quoted "&&"
  | BPlus -> fprintf f "%a" string_quoted "+"
  | BTimes -> fprintf f "%a" string_quoted "*"

let unparse e = printf "%a" pp_expr e
