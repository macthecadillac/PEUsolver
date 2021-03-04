module type S = sig
  type t

  val init : t list

  (* compute the cost of a sentential form by assessing the weights of the edges
     and the costs of the nodes*)
  val cost : t -> int

  val is_ground : t -> bool

  (* take a sentential form and unroll a non-terminal to create a list of
     sentential forms *)
  val unroll : t -> t list
end

module AS (M : S) : sig
  val sequence : M.t Seq.t
end
