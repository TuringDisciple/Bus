open! Combinators
open! Parser
open! Parser.ParserFunctor
open! Parser.ParserApp
open! Parser.ParserAlt
open! Parser.ParserMonad

(*
 * <expr>  := ("+"|"-") <term> ( ("+"|"-") <term> )*
 * <term>  := ("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9")+
*)

type parity  = Pos | Neg
type term    = Term of int
type expr_op = Add of term | Minus of term
type expr    = Expr of parity * term * expr_op list

let parity = Pos <$ ( Combinators.char_s "+"  ) <|> Neg <$ ( Combinators.char_s "-" )
