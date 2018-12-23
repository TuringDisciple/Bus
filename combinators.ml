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

(* Obtain the head of some sort of string*) 
let item  = Parser(fun t -> match t with "" -> [] | s -> let ts = tail s in let t = head s in [(ts, t )] ) 

(* Return a parsed value if it satisfies condition *)
let satisfy p =  item >>= (fun t -> if p t then pure t else empty)

(* Check if any elements within a list satisfy a condition and return that item wrapped*)
let one_of  = satisfy << flip elem
let not_of s = check( not << flip elem s )

(* Parse any carriage return characters within an input *)
let carriage_ret = many_s ( one_of [ "\n"; "\t" ] )

(* Parse any whitespace in input *)
let whitespace = many ( one_of [ "\t";" " ] ) >*> pure ()

(* Seperate input out by whitespace *)
let rec tok s  = string_p s <*< whitespace

let int_list_10 = ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
let number = ((some_s (one_of int_list_10)) >>= (return << int_of_string)) <*< whitespace