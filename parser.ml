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
   | pair::pairs -> let (_, x) = pair in Just x

(* Here's a parser that simply tries to get a character from an input string
 * val item : string parser *)
let item = Parser(fun s ->
    match String.explode s with
    | []   -> []
    | x::xs -> [ (String.implode xs, x) ] )

(* Useful functions *)
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

(* Functor *)
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
   let fmap f = function
      Parser px -> Parser( fun s ->
         List.map
            ( fun pair -> let (ts, x) = pair in (ts, f x) )
            ( px s ) )
   let (<$>) = fmap
   let (<$) a = function
      Parser _ -> Parser(fun s -> [ (s, a) ])
end

(* Applicative *)
module type Applicative = sig
   include Functor
   type 'a f'
   val pure : 'a -> 'a f'
   val (<*>): ( 'a -> 'b ) f' -> 'a f' -> 'b f'
   val (<*<):       'a f'     -> 'b f' -> 'a f'
   val (>*>):       'a f'     -> 'b f' -> 'b f'
end

module ParserApp : Applicative with type 'a f' = 'a parser = struct
   include ParserFunctor
   type 'a f' = 'a parser
   let pure x      = Parser(fun s -> [ (s, x) ])
   let (<*>) ( Parser pf ) ( Parser px ) =
      Parser( fun s ->
         List.flatten <| List.map
            ( fun pair -> let (ss, fs) = pair in
                  List.map
                     ( fun pair_ -> let (sss, x ) = pair_ in (sss, ( fs x )) )
                     ( px ss ) )
            ( pf s ) )
   let (<*<) px py = const <$> px <*> py
   let (>*>) px py = id <$ px <*> py
end

(* Alternative *)
