type mark = Nil | Min | Solved
type 'a node = And of mark | Or of mark * 'a

module type Node = sig
  type t

  val successors : t -> (int * t) node Tree.t list
  (** [successors t] finds the successors of element [t]. The return tree makes
      up of the costs and the corresponding node elements *)

  val est_cost : t -> int
  (** [est_cost t] estimates the cost of element [t] *)
end

module type S = sig
  type elt
  (** The node element type *)

  type t
  (** The internal representation of the And-Or tree with node element type
      [elt] *)

  val init : elt -> t
  (** [init a] returns a singleton tree in the interal representation *)

  val solve : t -> t
  (** [solve t] expands the branches using the AO* algorithm and finds a
      solution *)

  val trace : t -> elt Tree.t
  (** [trace t] takes a solved And-Or tree in the internal representation and
      returns the solution subtree as an ordinary tree *)
end

module Make (N : Node) : S with type elt = N.t
