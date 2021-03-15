type t = Str of string | Int of int | Bool of bool

type 'a printer = Format.formatter -> 'a -> unit

val equal : t -> t -> bool

val of_string : [`String | `Int | `Bool] -> string -> (t, [`Msg of string]) result

val pp : t printer
