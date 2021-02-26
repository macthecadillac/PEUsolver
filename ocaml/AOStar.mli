(** A modified AO* Algorithm that continues solving until it finds as solution
    that validates *)
open Sig
open Common

type mark = Nil | Solved

val mark_pp : mark printer

module Make (N : I) : S with type elt = N.t
