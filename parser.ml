(* This is a parser combinator library
 *
 * Author: Nashe Mncube *)

(* Useful types *)
type 'a maybe  = Nothing | Just of 'a
(* A parser type takes in a string, and produces a list of possible parses *)
type 'a parser = Parser of (string -> (string * 'a) list)

(* Useful functions *)
let snd pair =
   let (_, s) = pair in
   s

let fst pair =
   let (f, _) = pair in
   f

let (<<) f g x = f (g x)
let (>>) f g   = g << f
let const x _  = x
let id x       = x

(* Functor *)
module type Functor = sig
   type 'a f
   val fmap  : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$>) : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$)  :          'a  -> 'b f -> 'a f
end

module MaybeFunctor : Functor with type 'a f  = 'a maybe = struct
   type 'a f = 'a maybe
   let fmap g mx =
      match mx with
      | Nothing -> Nothing
      | Just x  -> Just (g x)
   let (<$>) g mx = fmap g mx
   let (<$) = fmap << const
end

module ParserFunctor : Functor with type 'a f = 'a parser = struct
   type 'a f = 'a parser
   let fmap f = function
      Parser px -> Parser( fun s ->
         List.map
            ( fun pair -> let (ts, x) = pair in (ts, f x) )
            ( px s ) )
   let (<$>) = fmap
   let (<$)  = fmap << const
end

(* Applicative *)
module type Applicative = sig
   include Functor
   type 'a f
   val pure : 'a -> 'a f
   val (<*>): ('a -> 'b) f -> 'a f -> 'b f
end

module ParserApp : Applicative with type 'a f = 'a parser = struct
   include ParserFunctor
   type 'a f = 'a parser
   let pure x      = Parser(fun s -> [ (s, x) ])
   let (<*>) px py = 
