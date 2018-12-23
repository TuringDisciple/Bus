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

let item  = Parser(fun t -> match t with "" -> [] | s -> let ts = tail s in let t = head s in [(ts, t )] ) 

let satisfy p =  item >>= (fun t -> if p t then pure t else empty)
let rec one_of  = satisfy << flip elem
let not_of s = check( not << flip elem s )


let carriage_ret = many_s ( one_of [ "\n"; "\t" ] )

let whitespace = many ( one_of [ "\t";" " ] ) >*> pure ()


let tok s =

let number = 