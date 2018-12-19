open! Parser
open! Parser.ParserFunctor
open! Parser.ParserApp
open! Parser.ParserAlt
open! Parser.ParserMonad

val check : ( string -> bool ) -> string parser
val char_s : string -> string parser
val string_p : string -> string parser
val one_of : string list -> string parser
val not_of : string list -> string parser
val number : int parser
val tok    : string -> string parser
val carriage_ret : string parser 
