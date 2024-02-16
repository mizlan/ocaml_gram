open CCFormat
open Syntax

let pp_pl_list pp = CCList.pp ~pp_start:(return "[@,") ~pp_stop:(return "]") pp

let pp_pl_list_nb pp =
  CCList.pp ~pp_start:(return "[@,") ~pp_stop:(return "]") pp

let rec pp_expr f = function
  | EVar v -> fprintf f "o_var(%a)" string v
  | EInt i -> fprintf f "o_int(%a)" int i
  | EBool b -> fprintf f "o_bool(%a)" bool b
  | EIfThenElse (c, a, b) ->
      fprintf f "@[<v 2>o_if(%a,@ %a,@ %a)@]" pp_expr c pp_expr a pp_expr b
  | EList l -> fprintf f "@[<hv 2>o_list(@,%a@,)@]" (pp_pl_list pp_expr) l
  | ETuple t -> fprintf f "@[<hv 2>o_tup(%a)@]" (pp_pl_list pp_expr) t
  | EAbs (pl, e) ->
      fprintf f "@[<hv 2>o_fun(%a,@ %a)@]" pp_patt pl pp_expr e
  | EBop (a, o, b) ->
      fprintf f "@[o_bop(@,%a,@ %a,@ %a@,)@]" pp_expr a pp_bop o pp_expr b
  | ECons (x, xs) ->
      fprintf f "@[<hv 2>o_cons(@,%a,@ %a@])" pp_expr x pp_expr xs
  | ELet (r, p, e, b) ->
      fprintf f "@[<hov 2>o_let(%a,@ %a,@ %a,@ %a@,)@]" bool r pp_patt p pp_expr
        e pp_expr b
  | EMatch (e, pl) ->
      fprintf f "@[<v 2>o_match(%a, %a)@]" pp_expr e
        (pp_pl_list (pp_match pp_patt pp_expr))
        pl
  | EApp (fn, xs) ->
      fprintf f "@[<hv 2>o_app(%a,@ %a)@]" pp_expr fn (CCList.pp pp_expr) xs

and pp_match p e f (a, b) = fprintf f "@[<hv 2>o_branch(%a,@ %a)@]" p a e b

and pp_patt f = function
  | PVar v -> fprintf f "o_var(%a)" string v
  | PInt i -> fprintf f "o_int(%a)" int i
  | PBool b -> fprintf f "o_bool(%a)" bool b
  | PUnderscore -> fprintf f "o_wild"
  | PTuple t -> fprintf f "@[<hv 2>o_tup(@,%a@,)@]" (pp_pl_list pp_patt) t
  | POr (a, b) -> fprintf f "@[o_or(%a,@ %a)@]" pp_patt a pp_patt b
  | PCons (x, xs) ->
      fprintf f "@[<hv 2>o_cons(@,%a,@ %a@])" pp_patt x pp_patt xs
  | PList l -> fprintf f "@[o_list(%a)@]" (pp_pl_list pp_patt) l

and pp_bop f = function
  | BEq -> fprintf f "%a" string_quoted "="
  | BLe -> fprintf f "%a" string_quoted "<"
  | BOr -> fprintf f "%a" string_quoted "||"
  | BAnd -> fprintf f "%a" string_quoted "&&"
  | BPlus -> fprintf f "%a" string_quoted "+"
  | BTimes -> fprintf f "%a" string_quoted "*"

let unparse e = printf "%a" pp_expr e
