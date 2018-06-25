open! Combinators

(*
 * <expr>  := ("+"|"-") <term> ( ("+"|"-") <term> )*
 * <term>  := ("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9")+
*)

type expr    = Expr of parity * term * expr_op list
type expr_op = Add of term | Minus of term
type term    = Term of Int
