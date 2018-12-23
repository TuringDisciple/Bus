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

let parity_p : parity parser =
    ( Pos <$ ( char_s "+"  ) ) <|> ( Neg <$ ( char_s "-" ) )

let term_p : term parser = ( fun x -> Term x ) <$> number

let expr_op_p : expr_op parser =
   ( ( fun t -> Add t )   <$ char_s "+" <*> term_p  )
<|>( ( fun t -> Minus t ) <$ char_s "-" <*> term_p )

let expr_p : expr parser =
   ( fun p t eop -> Expr (p, t, eop) ) <$> parity_p <*> term_p <*> many expr_op_p

let print_parity = function Pos -> print_string "Pos" | Neg -> print_string "Neg"

let rec print_term = function (Term x) -> print_string "Term "; print_int x

let rec print_expr_op = function 
        Add t -> print_string "Add "; print_term t
      | Minus t -> print_string "Minus "; print_term t

let rec print_expr = function Expr (p, t, exp_li) -> print_parity p; print_term t; List.iter (print_expr_op) (exp_li)

let parse_parity_pos = print_string "example: \"+\"\n "; print_parse (print_parity) (parse parity_p "+")
let parse_parity_neg =  print_string "example: \"-\"\n "; print_parse (print_parity) (parse parity_p "-")
let parse_term_ex = print_string "example: \"1234567890\"\n"; print_parse (print_term) (parse term_p "1234567890")

(* TODO: Testimg *)
let main () = parse_parity_pos; parse_parity_neg; parse_term_ex

let _ = main ()
