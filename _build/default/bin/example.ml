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

let parity_p : parity parser=
    ( Pos <$ ( char_s "+"  ) ) <|> ( Neg <$ ( char_s "-" ) )
<|> pure Pos

let term_p : term parser = ( fun x -> Term x ) <$> number

let expr_op_p : expr_op parser =
   ( ( fun t -> Add t )   <$ char_s "+" <*> term_p )
<|>( ( fun t -> Minus t ) <$ char_s "-" <*> term_p )

let expr_p : expr parser =
   ( fun p t eop -> Expr (p, t, eop) ) <$> parity_p <*> term_p <*> many expr_op_p
