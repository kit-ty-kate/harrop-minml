type expr =
  | Int of int
  | Var of string
  | BinOp of [ `Add | `Sub | `Leq ] * expr * expr
  | If of expr * expr * expr
  | Apply of expr * expr

type defn =
  | LetRec of string * string * expr

type prog = defn list * expr
