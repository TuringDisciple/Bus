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

(* val not_of : string list -> string parser *)
let not_of s = check( not << flip elem s )

(* val carriage_ret : string parser *)
let carriage_ret = many ( one_of [ "\n"; "\t" ] )

(* val whitespace : () parser *)
let whitespace = many ( one_of [ "\t";" " ] ) >*> pure ()

(* val tok : string -> string parser *)
let tok s = string_p s <*< whitespace
