(** Interface for a grammar definition *)
module type GRAMMAR = sig
  type t

  (** Provide the initial term *)
  val init : t list

  (** Test whether a given term is a hole *)
  val is_hole : t -> bool

  (** Enumerate the successors of a given term *)
  val successors : t -> t list

  (** Compute the cost of a given AST *)
  val ast_cost : t Tree.t -> int

  (** Convert a given AST to string *)
  val ast_to_string : t Tree.t -> string
end

(** Generate an enumerator for a given grammar *)
module AS (G : GRAMMAR) : sig
  (* A sequence of ground sentential forms. Enumerated best first. *)
  val sequence : G.t Tree.t Seq.t
end
