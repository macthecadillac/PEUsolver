open Containers

type t

val of_string : string -> t

val compare : t -> t -> int

module Map : CCMap.S with type key = t

val init : t

val parse_spec : string -> (Sexp.t list, [`Msg of string]) result

val build_succession_map : Sexp.t -> t list Map.t -> (t list Map.t, [`Msg of string]) result

val grammar : Sexp.t list -> (Sexp.t list, [`Msg of string]) result

val is_hole : t list Map.t -> t -> bool

val ast_cost : float Map.t -> t Tree.t -> float

val ast_to_string : t Tree.t -> string
