(** An abstract interface for both PCFG and PHOG *)
module type P = sig
  type t

  type raw_t

  (** [encode raw] encodes the raw format into json *)
  val encode : raw_t -> JSON.t

  (** [decode json] converts a json configuration into [raw_t] *)
  val decode : JSON.t -> (raw_t, [`Msg of string]) result

  (** [train ntMap p asts] trains the synthesizer based on the TCOND program [p]
      and a given collection of solution ASTs. It spits out the raw format. *)
  val train : Grammar.t Grammar.Map.t -> TCOND.p -> Grammar.t Tree.t list -> raw_t

  (** [compile ntMap raw] builds the internal type. [ntMap] is a map that connects
      production rules to their corresponding non-terminal types. *)
  val compile : Grammar.t Grammar.Map.t -> raw_t -> t

  (** [ast_cost t context ast] computes the cost of an entire AST. *)
  val ast_cost : t -> TCOND.p -> Grammar.t Tree.t -> float

  val compute_heuristic : Grammar.t list Grammar.Map.t -> t -> float Grammar.Map.t

  val compute_heuristic_with_context : Grammar.t list Grammar.Map.t -> t -> float Grammar.Map.t
                                       -> float TCOND.ContextRuleMap.t
end
