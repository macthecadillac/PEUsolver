module type ENV = sig
  val order : [`HMax | `HMin | `LeftFirst | `RightFirst]
  val succMap : Grammar.t list Grammar.Map.t
  val ast_cost : Grammar.t Tree.t -> float
  val heuristics_w_context : TCOND.tcond list -> AST.t -> float
  val heuristics : Grammar.t -> float
end

module type S = sig
  (* A sequence of ground sentential forms. Enumerated best first. *)
  val sequence : Grammar.t Tree.t Seq.t
end

module Make (E : ENV) : S
