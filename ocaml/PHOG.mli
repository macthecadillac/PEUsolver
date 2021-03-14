type t

type 'a printer = Format.formatter -> 'a -> unit

module Context : sig
  type t = Grammar.t list
  val pp : t printer
end

(** An intermediate representation for interoperability across modules. *)
type raw_t = PCFG.raw_t * (Context.t * PCFG.raw_t) list

(** [encode raw] encodes the raw format into json *)
val encode : raw_t -> JSON.t

(** [decode json] converts a json configuration into [raw_t] *)
val decode : JSON.t -> (raw_t, [`Msg of string]) result

(** [train ntMap p asts] trains the PHOG based on the TCOND program [p] and a
    given collection of solution ASTs. It spits out the raw format. *)
val train : Grammar.t Grammar.Map.t -> TCOND.p -> Grammar.t Tree.t list -> raw_t

(** [compile ntMap raw] builds the PHOG type. [ntMap] is a map that connects
    production rules to their corresponding non-terminal types. *)
val compile : Grammar.t Grammar.Map.t -> raw_t -> t

(** [rule_cost t context rule] computes the cost of a given rule. *)
val rule_cost : t -> Context.t -> Grammar.t -> float

(** [ast_cost t context ast] computes the cost of an entire AST. *)
val ast_cost : t -> TCOND.p -> Grammar.t Tree.t -> float

(** Pretty printer *)
val pp : t printer

module ContextMap : CCMap.S with type key = Grammar.t list
