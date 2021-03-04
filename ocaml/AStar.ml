open Containers

module type S = sig
  type t
  val init : t list
  val cost : t -> int
  val is_ground : t -> bool
  val unroll : t -> t list
end

module AS (M : S) : sig
  val sequence : M.t Seq.t
end = struct
  module H = PairingHeap.Make (struct
    type t = M.t
    let compare a b = Int.compare (M.cost a) (M.cost b)
  end)

  let rec enumerate h =
    let open Option in
    let* sf = H.find_min h in
    if M.is_ground sf then Some (sf, H.delete_min h)
    else
      let sf's = M.unroll sf in
      let h' = List.fold_left H.insert (H.delete_min h) sf's in
      enumerate h'

  let sequence =
    let h = H.of_list M.init in
    Seq.unfold enumerate h
end
