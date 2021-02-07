type 'a printer = Format.formatter -> 'a -> unit

type mark = Nil | Solved

val mark_pp : mark printer

type 'a node = And of mark * 'a | Or of mark * 'a

val node_pp : 'a printer -> 'a node printer

module type I = sig
  type t

  val equal : t -> t -> bool
  (** Equality test *)

  val successors : t -> (int * t) node Tree.t list
  (** [successors t] finds the successors of element [t]. The return tree is
      made of up of the costs and the corresponding node elements. Note:
      successors of elements that are parts of an And node must all be Or nodes.
      *)

  val est_cost : t -> int
  (** [est_cost t] estimates the cost of element [t] *)

  val validate : t Tree.t -> bool
  (** [validate t] validates that t fits the specification *)

  val pp : t printer
  (** a pretty printer. Technically not necessary but it a nice-to-have for
      debug purposes *)
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

  val extract : t -> elt Tree.t
  (** [extract t] takes a solved And-Or tree in the internal representation and
      returns the solution subtree as an ordinary tree *)

  val run : t -> elt Tree.t
  (** [run t] repeatedly solves [t] using the AO* algorithm until a solution
      validates *)
end

module Make (N : I) : S with type elt = N.t
