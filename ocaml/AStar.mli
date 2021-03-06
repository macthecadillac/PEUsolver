module type ENV = sig
  val grammar : Grammar.t list Grammar.Map.t
  val pcfg : float Grammar.Map.t
end

module type S = sig
  (* A sequence of ground sentential forms. Enumerated best first. *)
  val sequence : Grammar.t Tree.t Seq.t
end

module Make (E : ENV) : S
