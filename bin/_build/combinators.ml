(*
          _____                    _____                    _____
         /\    \                  /\    \                  /\    \
        /::\    \                /::\____\                /::\    \
       /::::\    \              /:::/    /               /::::\    \
      /::::::\    \            /:::/    /               /::::::\    \
     /:::/\:::\    \          /:::/    /               /:::/\:::\    \
    /:::/__\:::\    \        /:::/    /               /:::/__\:::\    \
   /::::\   \:::\    \      /:::/    /                \:::\   \:::\    \
  /::::::\   \:::\    \    /:::/    /      _____    ___\:::\   \:::\    \
 /:::/\:::\   \:::\ ___\  /:::/____/      /\    \  /\   \:::\   \:::\    \
/:::/__\:::\   \:::|    ||:::|    /      /::\____\/::\   \:::\   \:::\____\
\:::\   \:::\  /:::|____||:::|____\     /:::/    /\:::\   \:::\   \::/    /
 \:::\   \:::\/:::/    /  \:::\    \   /:::/    /  \:::\   \:::\   \/____/
  \:::\   \::::::/    /    \:::\    \ /:::/    /    \:::\   \:::\    \
   \:::\   \::::/    /      \:::\    /:::/    /      \:::\   \:::\____\
    \:::\  /:::/    /        \:::\__/:::/    /        \:::\  /:::/    /
     \:::\/:::/    /          \::::::::/    /          \:::\/:::/    /
      \::::::/    /            \::::::/    /            \::::::/    /
       \::::/    /              \::::/    /              \::::/    /
        \::/____/                \::/____/                \::/    /
         ~~                       ~~                       \/____/

 *                         A parser library
 *               Author:Nashe Mncube/TuringDisciple
 *)

open! Parser
open! Parser.ParserFunctor
open! Parser.ParserApp
open! Parser.ParserAlt
open! Parser.ParserMonad



(* Parser combinators *)

(* val check : ( string -> bool ) -> string parser *)
let check p = read_char >>= ( fun c -> if p c then pure c else empty )

(* val char_p : string -> string parser *)
let char_s c = check <|( fun c_ -> c = c_ )

(* val string_p : string -> string parser *)
let rec string_p s =
   match s with
   | ""    -> pure ""
   | _     -> char_s ( chr_str <| s.[0] ) <+> ( string_p <| tail s )


(* val one_of : string -> string parser *)
let one_of = check << flip elem

(* val not_of : string -> string parser *)
let not_of s = check( not << flip elem s )

(* val carriage_ret : string parser *)
let carriage_ret = many ( one_of [ "\n"; "\t" ] )

(* val whitespace : () parser *)
let whitespace = many ( one_of [ "\t";" " ] ) >*> pure ()

(* val tok : string -> string parser *)
let tok s = string_p s <*< whitespace

(* val number : int parser *)
let number =
   ( ( some_s ( one_of ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"] ) )
             >>= ( return << int_of_string ) ) <*< whitespace
