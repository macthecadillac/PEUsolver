open Containers

type 'a printer = Format.formatter -> 'a -> unit

module type S = sig
  type elt
  type t
  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val find_min : t -> elt option
  val find_min_exn : t -> elt
  val meld : t -> t -> t
  val insert : t -> elt -> t
  val delete_min : t -> t
  val to_list : t -> elt list
  val of_list : elt list -> t
  val pp : elt printer -> t printer
  val pp' : elt printer -> t printer
end

module type ORDERING = sig
  type t
  val compare : t -> t -> int
end

module Make (E : ORDERING) : S with type elt = E.t
