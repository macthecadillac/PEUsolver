type 'a printer = Format.formatter -> 'a -> unit

type error = SolutionNotFound

val error_pp : error printer

val list_pp : string -> (string -> 'a printer) -> 'a list printer

val str_pp : ('a, Format.formatter, unit) format -> Format.formatter -> unit -> 'a

val while_do_result : ('a -> bool) -> ('a -> ('a, 'b) result) -> ('a, 'b) result -> ('a, 'b) result
