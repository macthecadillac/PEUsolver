(** A modified AO* Algorithm that continues solving until it finds as solution
    that validates *)

type 'a printer = Format.formatter -> 'a -> unit

type mark = Nil | Solved

val mark_pp : mark printer

type error = SolutionNotFound

val error_pp : error printer

module type I = sig
  type t

  (** Equality test *)
  val equal : t -> t -> bool

  (** [successors t] finds the successors of element [t]. The return tree is
      made of up of the costs and the corresponding node elements. Note:
      successors of elements that are parts of an And node must all be Or nodes.
      *)
  val successors : t -> (int * t) list list

  (** [validate t] validates that t fits the specification *)
  val validate : t Tree.t -> bool

  (** a pretty printer. Technically not necessary but it a nice-to-have for
      debug purposes *)
  val pp : t printer
end

module type S = sig
  (** The node element type *)
  type elt

  (** The internal representation of the And-Or tree with node element type
      [elt] *)
  type t

  (** [init a] returns a singleton tree in the interal representation *)
  val init : (int * elt) list -> t

  (** [try_solve t] expands the branches using the AO* algorithm and finds a
      solution *)
  val try_solve : t -> (t, error) result

  (** [extract t] takes a solved And-Or tree in the internal representation and
      returns the solution subtree as an ordinary tree *)
  val extract : t -> elt Tree.t

  (** [run t] repeatedly solves [t] using the AO* algorithm until a solution
      validates *)
  val run : t -> (elt Tree.t, error) result

  (** Pretty printer for the internal rep *)
  val pp : t printer
end

module Make (N : I) : S with type elt = N.t
