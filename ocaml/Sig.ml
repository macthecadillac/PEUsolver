open Common

module type I = sig
  type t
  val equal : t -> t -> bool
  val successors : t -> (int * (int * t) list) list
  val validate : t Tree.t -> bool
  val pp : t printer
end

module type S = sig
  type elt
  type t
  val init : (int * elt) list -> t
  val try_solve : t -> (t, error) result
  val extract : t -> elt Tree.t
  val run : t -> (elt Tree.t, error) result
  val pp : t printer
end
