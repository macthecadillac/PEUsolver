(** A modified A* Algorithm that continues solving until it finds a solution
    that validates *)
module Make (N : Sig.I) : Sig.S with type elt = N.t
