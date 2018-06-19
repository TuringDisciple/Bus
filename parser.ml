(* This is a parser combinator library
 *
 * Author: Nashe Mncube/TuringDisciple *)

(* Useful types *)
type 'a maybe  = Nothing | Just of 'a

(* A parser type takes in a string, and produces a list of possible parses *)
type 'a parser = Parser of (string -> (string * 'a) list)

(* val parse_maybe : 'a parser -> string -> Maybe a *)
let parse_maybe px s =
   match px s with
   | []          -> Nothing
   | p::ps -> let (_, x) = p in Just x

(* Here's a parser that simply tries to get a character from an input string
 * val item : char parser *)
let item = Parser( fun s ->
    match s with
    | ""   -> []
    | _ when String.length s = 1  -> [ ("", s.[0]) ]
    | _    ->
      let t = (String.sub s 1 ( ( String.length s ) - 1)) in [ (t, s.[0]) ] )

(* Useful functions *)
(* val tail_of_string : string -> string *)
let tail s =
   match s with
   | _ when String.length s <= 1  -> ""
   | _ -> let t = (String.sub s 1 ( ( String.length s ) - 1)) in t

(* val snd   : 'a * 'b -> 'b *)
let snd pair = let (_, s) = pair in s

(* val fst   : 'a * 'b -> 'a *)
let fst pair = let (f, _) = pair in f

(* val (<<)  : ( 'b -> 'c ) -> ( 'a -> 'b ) -> 'a -> 'c *)
let (<<) f g x = f ( g x )

(* val (>>)  : ( 'a -> 'b ) -> ( 'b -> 'c ) -> 'a -> 'c *)
let (>>) f g   = g << f

(* val id    : 'a -> 'a *)
let id x       = x

(* val const : 'a -> 'b -> 'a *)
let const x    = fun _ -> x

(* val (|>)  : 'a -> ( 'a -> 'b ) -> 'b *)
let (|>) v f   = f v

(* val (<|)  : ( 'a -> 'b ) -> 'a -> 'b *)
let (<|) f v   = f v

(* val flip  : ('a -> 'b -> 'c ) -> 'b -> 'a -> 'c *)
let flip f x y = f y x

let (+:) x l = x :: l

(* Functor
 * Functors give us the ability to modify the output of parses *)
module type Functor = sig
   type 'a f
   val fmap  : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$>) : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$)  :         'a   -> 'b f -> 'a f
end

module MaybeFunctor : Functor with type 'a f  = 'a maybe = struct
   type 'a f = 'a maybe
   let fmap g mx =
      match mx with
      | Nothing -> Nothing
      | Just x  -> Just ( g x )
   let (<$>) = fmap
   let (<$) a mx  =
      match mx with
      | Nothing -> Nothing
      | _       -> Just a
end

module ParserFunctor : Functor with type 'a f = 'a parser = struct
   type 'a f = 'a parser
   let fmap g = function
      Parser px -> Parser( fun s ->
         List.map
            ( fun pair -> let (ts, x) = pair in (ts, g x) )
            ( px s ) )
   let (<$>) = fmap
   (* Derived combi  *)
   let (<$) a = function
      Parser _ -> Parser(fun s -> [ (s, a) ])
end

(* Applicative
 * Applicative gives us the ability to chain parsers through <*> *)
module type Applicative = sig
   type 'a f
   include Functor with type 'a f := 'a f
   val pure : 'a -> 'a f
   val (<*>): ( 'a -> 'b ) f -> 'a f -> 'b f
   val (<*<):       'a f     -> 'b f -> 'a f
   val (>*>):       'a f     -> 'b f -> 'b f
   val (<:>):       'a f     -> ( 'a list ) f -> ( 'a list ) f
end

module ParserApp : ( Applicative with type 'a f = 'a parser ) = struct
   include ParserFunctor
   let pure x      = Parser(fun s -> [ (s, x) ])
   let (<*>) ( Parser pf ) ( Parser px ) =
      Parser( fun s ->
         List.flatten <| List.map
            ( fun pair -> let (ss, fs) = pair in
                  List.map
                     ( fun pair_ -> let (sss, x ) = pair_ in (sss, ( fs x )) )
                     ( px ss ) )
            ( pf s ) )
   (* Derived combi *)
   let (<*<) px py = const <$> px <*> py
   let (>*>) px py = id <$ px <*> py
   let (<:>) x xs  = (+:) <$> x <*> xs
end

(* Alternative
 * Alternative gives us choices between parsers via <|> *)
module type Alternative = sig
   type 'a f
   (* include Functor with type 'a f := 'a f *)
   include Applicative with type 'a f := 'a f
   val empty : 'a f
   val (<|>) : 'a f -> 'a f -> 'a f
   val some  : 'a f -> ('a list) f
   val many  : 'a f -> ('a list) f
end

module ParserAlt : ( Alternative with type 'a f := 'a parser ) = struct
   include ParserApp
   (* include ParserFunctor *)
   let empty = Parser( fun _ -> [] )
   let (<|>) ( Parser px ) ( Parser py ) =
      Parser( fun s -> List.append ( px s ) ( py s ) )

   (* Derived combi *)
   let rec some px = px <:> some px <|> empty
   let     many px = some px <|> empty
end

(* Monad
 * The Monad typeclass allows for the output of one parser to afect the final
 * output via >>= *)
module type Monad = sig
   type 'a f
   include Alternative with type 'a f := 'a f
   val return : 'a -> 'a f
   val (>>=)  : 'a f -> ('a -> 'b f) -> 'b f
end

module ParserMonad : ( Monad with type 'a f := 'a parser)  = struct
   include ParserAlt
   let return x = pure x
   let (>>=) ( Parser px ) f =
      Parser( fun s ->
         List.flatten <| List.map
            ( fun p -> let (ss, x) = p in
               match ( f x ) with Parser px -> ( px ss ))
            ( px s ) )
end

(* Parsing functions *)
