(** A monadic implementation of lazy values with caching *)
open Containers

type 'a t =
    Thunk of (unit -> 'a)
  | Forced of 'a

let is_forced = function Forced _ -> true | _ -> false

let of_thunk f = Thunk f

let force = function
    Thunk f -> f ()
  | Forced a -> a

let map f = function
    Thunk g -> Thunk (fun () -> f (g ()))
  | Forced a -> Forced (f a)

let (<$>) = map

let pure a = Forced a

let (<*>) fa a =
  match fa with
    Thunk ft -> Thunk (fun () -> (ft ()) (force a))
  | Forced f -> map f a

let product a b = Pair.make <$> a <*> b

let return = pure

let join = function
    Thunk f -> f ()
  | Forced (Thunk f) -> Thunk f
  | Forced a -> a

let (>>=) a f = join (map f a)

let (let*) = (>>=)

let (and*) = product

let (let+) a f = map f a

let (and+) = product
