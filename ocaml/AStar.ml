open Containers
open Fun

module type GRAMMAR = sig
  type t
  val init : t list
  val is_hole : t -> bool
  val successors : t -> t list
  val ast_cost : t Tree.t -> int
  val ast_to_string : t Tree.t -> string
end

module AS (G : GRAMMAR) = struct
  module Paths = PairingHeap.Make (struct
    type t = int list
    let rec compare a b =
      match a, b with
        [], [] -> 0 | [], _ -> -1 | _, [] -> 1
      | h1::t1, h2::t2 when h1 = h2 -> compare t1 t2
      | h1::_, h2::_ -> Int.compare h1 h2
  end)

  module rec T : PairingHeap.ORDERING
    with type t = Paths.t * G.t Tree.t = struct
    type t = Paths.t * G.t Tree.t
    let cost = G.ast_cost % snd
    let compare a b = Int.compare (cost a) (cost b)
  end

  and H : PairingHeap.S with type elt = T.t = PairingHeap.Make (T)

  type t = T.t

  let init = List.map (fun s -> Paths.singleton [0], Tree.return s) G.init

  let is_ground = Paths.is_empty % fst

  let to_string = G.ast_to_string % snd

  let unroll (h, t) =
    let open List in
    let expand s =
      let l = G.successors s in
      pure <$> (0 --^ length l), Tree.Node (s, Tree.return <$> l) in
    let rec go acc i n p = function
        [] -> raise (Invalid_argument "Index out of bounds")
      | h'::t' when n <> i -> go (h'::acc) i (n + 1) p t'
      | h'::t' ->
          let+ p', n = aux p h' in
          cons i <$> p', rev_append acc (n::t')
    and aux p t =
      match p, t with
        [], Tree.Node (a, []) when G.is_hole a -> expand <$> G.successors a
      | [], Tree.Node (a, []) -> [expand a]
      | i::t, Tree.Node (a, l) -> let+ ps, ts = go [] i 0 t l in ps, Tree.Node (a, ts)
      | _ -> raise (Invalid_argument "Inconsistent path/tree") in
    let path = Paths.find_min_exn h |> tl in
    let+ ps, t' =
      let+ ps, t' = aux path t in
      cons 0 <$> ps, t' in
    List.fold_left Paths.insert (Paths.delete_min h) ps, t'

  let rec enumerate h =
    let open Option in
    let* sf = H.find_min h in
    if is_ground sf then Some (sf, H.delete_min h)
    else
      let sf's = unroll sf in
      let h' = List.fold_left H.insert (H.delete_min h) sf's in
      enumerate h'

  let sequence =
    let h = H.of_list init in
    Seq.unfold enumerate h |> Seq.map snd
end
