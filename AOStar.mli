type mark = Nil | Min | Solved
type 'a tree = Node of 'a * 'a tree list
type 'a node = And of mark | Or of mark * 'a

module type Node = sig
  type t
  val successors : t -> (int * t) node tree list
  val est_cost : t -> int
end

module type S = sig
  type elt
  type u
  val init : elt -> u
  val solve : u -> u
  val trace : u -> elt tree
end

module Make (N : Node) : S with type elt = N.t
