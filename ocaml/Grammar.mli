open Containers

type t

type 'a printer = Format.formatter -> 'a -> unit

(** Pretty printer *)
val pp : t printer

val equal : t -> t -> bool

val of_string : string -> t

val to_string : t -> string

val compare : t -> t -> int

module Map : sig
  include CCMap.S with type key = t
  val pp : key printer -> 'a printer -> 'a t printer
end

(** The initial symbol in Euphony *)
val init : t

(** [parse_spec path] parses the specification at the given path *)
val parse_spec : string -> (Sexp.t list, [`Msg of string]) result

(** [succession_map specs] builds a map that terms to their successor terms *)
val succession_map : Sexp.t list -> (t list Map.t, [`Msg of string]) result

(** [rule_nttype_map specs] builds a map that maps a production to a non-terminal
    type from a collection of parsed specs *)
val rule_nttype_map : Sexp.t list -> (t Map.t, [`Msg of string]) result

(** [rule_nttype_map_union map1 map2] computes the union of the two
    rule_nttype_map's. *)
val merge_rule_nttype_maps : t Map.t -> t Map.t -> (t Map.t, [`Msg of string]) result

(** [build_solution_ast spec] builds an AST based on the solution in the given
    spec *)
val build_solution_ast : t Map.t -> Sexp.t list -> (t Tree.t, [`Msg of string]) result

(* [parse_constraints specs] returns a list of tuples that are pairs of
   arguments and expected results **)
val parse_constraints : Sexp.t list -> ((string list * string) list, [`Msg of string]) result

(** [is_hole successorMap rule] checks if the production rule is a hole *)
val is_hole : t list Map.t -> t -> bool
