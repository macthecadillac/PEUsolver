type 'a printer = Format.formatter -> 'a -> unit

type error = SolutionNotFound

val error_pp : error printer

val list_pp : string -> (string -> 'a printer) -> 'a list printer

val str_pp : ('a, Format.formatter, unit) format -> Format.formatter -> unit -> 'a

val while_do_result : ('a -> bool) -> ('a -> ('a, 'b) result) -> ('a, 'b) result -> ('a, 'b) result

module I : sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  val (<>) : t -> t -> bool
  val pp : t printer
end

module G : sig
  type t
  val zero : t
  val inc : t -> t
  val of_int : int -> t
  val to_int : t -> int
  val (<=) : t -> t -> bool
  val (<) : t -> t -> bool
  val pp : t printer
end
