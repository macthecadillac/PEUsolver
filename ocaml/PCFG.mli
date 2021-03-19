type t

(** An intermediate representation for interoperability across modules. *)
type raw_t = (Grammar.t * (Grammar.t * int) list) list

type 'a printer = Format.formatter -> 'a -> unit

(** [encode raw] encodes the raw format into json *)
val encode : raw_t -> JSON.t

(** [decode json] converts a json configuration into [raw_t] *)
val decode : JSON.t -> (raw_t, [`Msg of string]) result

(** [count ntMap rules] takes in a list of rules, counts them, and returns the
    raw format. *)
val count : Grammar.t Grammar.Map.t -> Grammar.t list -> raw_t

(** [train ntMap asts] trains the PHOG based on a given collection of solution
    ASTs. It spits out the raw format. *)
val train : Grammar.t Grammar.Map.t -> AST.t list -> raw_t

(** [compile ntMap raw] builds the PCFG type. [ntMap] is a map that connects
    production rules to their corresponding non-terminal types. *)
val compile : Grammar.t Grammar.Map.t -> raw_t -> t

(** [rule_prob t rule] computes the probability of a given rule. *)
val rule_prob : t -> Grammar.t -> float

(** [rule_cost t rule] computes the cost of a given rule. *)
val rule_cost : t -> Grammar.t -> float

(** [ast_cost t [] ast] computes the cost of an entire AST.  *)
val ast_cost : t -> AST.t -> float

(** Pretty printer *)
val pp : t printer
