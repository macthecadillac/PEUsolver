module type PATHORDER = sig
  type t = (int * Grammar.t) list
  val compare : t -> t -> int
end

module type ENV = sig
  val successorsMap : Grammar.t list Grammar.Map.t
  val pcfg : float Grammar.Map.t
end

module type S = sig
  (* A sequence of ground sentential forms. Enumerated best first. *)
  val sequence : Grammar.t Tree.t Seq.t
end

module Make (E : ENV) (O : PATHORDER) : S
