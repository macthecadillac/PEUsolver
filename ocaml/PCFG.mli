type t

(** An intermediate representation for interoperability across modules. *)
type raw_t = (Grammar.t * (Grammar.t * int) list) list

type 'a printer = Format.formatter -> 'a -> unit

(** [encode raw] encodes the raw format into json *)
val encode : raw_t -> JSON.t

(** [decode json] converts a json configuration into [raw_t] *)
val decode : JSON.t -> (raw_t, [`Msg of string]) result

(** [compile ntMap raw] builds the PCFG type. [ntMap] is a map that connects
    production rules to their corresponding non-terminal types. *)
val compile : Grammar.t Grammar.Map.t -> raw_t -> t

(** [rule_cost t rule] computes the cost of a given rule. *)
val rule_cost : t -> Grammar.t -> float

(** [ast_cost t ast] computes the cost of an entire AST. *)
val ast_cost : t -> Grammar.t Tree.t -> float

(** Pretty printer *)
val pp : t printer
