open Containers

type 'a printer = Format.formatter -> 'a -> unit

module Context : sig
  type t = Grammar.t list
  val to_string : t -> string
  val of_string : string -> t
  val compare : t -> t -> int
  val pp : t printer
end

module ContextRule : sig
  type t = Context.t * Grammar.t
  val compare : t -> t -> int
end

module ContextMap : Map.S with type key = Context.t
module ContextRuleMap : Map.S with type key = ContextRule.t

type move =
    Up
  | Left
  | Right
  | DownFirst
  | DownLast
  | PrevDFS

type write = WriteValue

type tcond = Empty | W of write | M of move

type p = tcond list

(** [apply loc ast p] applies TCOND program [p] to [ast] starting at location
    [loc] and return the starting node and the context. [loc] describes the
    location on the AST using TCOND operators starting at the root of the tree. *)
val apply : tcond list -> AST.t -> p -> Grammar.t * Context.t

(** [mutate p r] mutates the TCOND program [p] *)
val mutate : p -> Random.State.t -> p

val enumerate_context_rule_pairs : p -> AST.t list -> ContextRule.t list

val generate_probability_map : p -> AST.t list -> float ContextRuleMap.t

val cost : AST.t list -> p -> float -> float

(** A very simple train routine *)
val train : AST.t list -> float -> int -> p

val save : p -> Fpath.t -> (unit, [`Msg of string]) result

val load : Fpath.t -> (p, [`Msg of string]) result

(** Pretty printer for TCOND operators *)
val pp : tcond printer

(** Pretty printer for TCOND programs *)
val pp' : p printer
