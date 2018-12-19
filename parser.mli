(* Useful types and functions *)
type 'a parser = Parser of (string -> (string * 'a) list)
type 'a maybe  = Nothing | Just of 'a

val snd     : 'a * 'b -> 'b
val fst     : 'a * 'b -> 'a
val (<<)    : ( 'b -> 'c ) -> ( 'a -> 'b ) -> 'a -> 'c
val (>>)    : ( 'a -> 'b ) -> ( 'b -> 'c ) -> 'a -> 'c
val id      : 'a -> 'a
val const   : 'a -> 'b -> 'a
val (|>)    : 'a -> ( 'a -> 'b ) -> 'b
val (<|)    : ( 'a -> 'b ) -> 'a -> 'b
val flip    : ('a -> 'b -> 'c ) -> 'b -> 'a -> 'c
val (+:)    : 'a  -> 'a list -> 'a list
val chr_str : char -> string
val elem    : 'a -> 'a list -> bool
val tail    : string -> string

val read_char   : string parser
val parse_maybe : ( 'a -> (string * 'a) list ) -> 'a  -> 'a maybe

val parse : 'a parser -> string -> ( string * 'a ) list

val print_parse: ('a -> unit) -> (string * 'a) list -> unit 
(* MODULAR TYPECLASSES *)

(* FUNCTOR *)
module type Functor = sig
   type 'a f
   val fmap  : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$>) : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$)  :         'a   -> 'b f -> 'a f
end

module ParserFunctor : sig
   val fmap  : ( 'a -> 'b ) -> 'a parser -> 'b parser
   val (<$>) : ( 'a -> 'b ) -> 'a parser -> 'b parser
   val (<$)  :         'a   -> 'b parser -> 'a parser
end

module MaybeFunctor : sig
   val fmap  : ( 'a -> 'b ) -> 'a maybe -> 'b maybe
   val (<$>) : ( 'a -> 'b ) -> 'a maybe -> 'b maybe
   val (<$)  :         'a   -> 'b maybe -> 'a maybe
end

(* APPLICATIVE *)
module type Applicative = sig
   type 'a f
   include Functor with type 'a f := 'a f
   val pure : 'a -> 'a f
   val (<*>): ( 'a -> 'b ) f -> 'a f -> 'b f
   val (<*<):       'a f     -> 'b f -> 'a f
   val (>*>):       'a f     -> 'b f -> 'b f
   val (<:>):       'a f     -> 'a list f -> 'a list f
   val (<+>):       string f -> string f  -> string f (* Not in applicative but useful *)
end

module ParserApp : sig
   val pure : 'a -> 'a parser
   val (<*>): ( 'a -> 'b ) parser -> 'a parser -> 'b parser
   val (<*<):       'a parser     -> 'b parser -> 'a parser
   val (>*>):       'a parser     -> 'b parser -> 'b parser
   val (<:>):       'a parser     -> 'a list parser -> 'a list parser
   val (<+>):        string parser -> string parser -> string parser
end

(* ALTERNATIVE *)
module type Alternative = sig
   type 'a f
   include Applicative with type 'a f := 'a f
   val empty : 'a f
   val (<|>) : 'a f -> 'a f -> 'a f
   val some  : 'a f -> ('a list) f
   val many  : 'a f -> ('a list) f
   val some_s: string parser -> string parser
   val many_s: string parser -> string parser
end

module ParserAlt : sig
   val empty : 'a parser
   val (<|>) : 'a parser -> 'a parser -> 'a parser
   val some  : 'a parser -> ('a list) parser
   val many  : 'a parser -> ('a list) parser
   val some_s: string parser -> string parser
   val many_s: string parser -> string parser
end

(* MONAD *)
module type Monad = sig
   type 'a f
   include Alternative with type 'a f := 'a f
   val return : 'a -> 'a f
   val (>>=)  : 'a f -> ('a -> 'b f) -> 'b f
end

module ParserMonad : sig
   val return: 'a -> 'a parser
   val (>>=) : 'a parser -> ( 'a -> 'b parser ) -> 'b parser
end


