type expr =
  | EVar of string
  | EIfThenElse of expr * expr * expr
  | EList of expr list
  | EApp of expr * expr list (* f a b c *)
  | ETuple of expr list
  | ECons of expr * expr
  | EAbs of pattern * expr (* fun a b c -> d *)
  | EBop of expr * bop * expr
  | EMatch of expr * (pattern * expr) list
  | ELet of bool * pattern * expr * expr (* let rec? x = y in z *)
  | EInt of int
  | EBool of bool
and pattern =
  | PVar of string
  | PUnderscore
  | PTuple of pattern list
  | PList of pattern list
  | PCons of pattern * pattern
  | POr of pattern * pattern
  | PInt of int
  | PBool of bool
and bop =
  | BPlus
  | BTimes
  | BOr
  | BAnd
  | BLe
  | BEq
[@@deriving show]
