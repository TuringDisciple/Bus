(* This is a parser combinator library
 *
 * Author: Nashe Mncube *)

(* Useful types *)
type 'a maybe  = Nothing | Just of 'a
type 'a parser = { parse : string -> ( string * 'a ) list }

(* Functor *)
module type Functor = sig
   type 'a f
   val fmap  : ( 'a -> 'b ) -> 'a f -> 'b f
   val (<$>) : ( 'a -> 'b ) -> 'a f -> 'b f
end

module MaybeFunctor : Functor with type 'a f  = 'a maybe = struct
   type 'a f = 'a maybe
   let fmap g mx =
      match mx with
      | Nothing -> Nothing
      | Just x  -> Just (g x)
   let (<$>) g mx = fmap g m
end

module ParserFunctor : Functor with type 'a f = 'a parser = struct
???
end
