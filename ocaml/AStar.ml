open Containers
open Fun

module type ENV = sig
  val grammar : Grammar.t list Grammar.Map.t
  val pcfg : float Grammar.Map.t
end

module type S = sig
  val sequence : Grammar.t Tree.t Seq.t
end

module Make (E : ENV) : S = struct
  module Paths = PairingHeap.Make (struct
    type t = int list
    let rec compare a b =
      match a, b with
        [], [] -> 0 | [], _ -> -1 | _, [] -> 1
      | h1::t1, h2::t2 when h1 = h2 -> compare t1 t2
      | h1::_, h2::_ -> Int.compare h1 h2
  end)

  module rec T : PairingHeap.ORDERING
    with type t = float * Paths.t * Grammar.t Tree.t = struct
    type t = float * Paths.t * Grammar.t Tree.t
    let compare (a, _, _) (b, _, _) = Float.compare a b
  end

  and H : PairingHeap.S with type elt = T.t = PairingHeap.Make (T)

  let is_ground (_, ps, _) = Paths.is_empty ps

  let successors = Option.get_exn % flip Grammar.Map.get E.grammar

  let init = successors Grammar.init

  let unroll (_, h, t) =
    let open List in
    let expand s =
      let l = successors s in
      pure <$> (0 --^ length l), Tree.Node (s, Tree.return <$> l) in
    let rec go acc i n p = function
        [] -> raise (Invalid_argument "Index out of bounds")
      | h'::t' when n <> i -> go (h'::acc) i (n + 1) p t'
      | h'::t' ->
          let+ p', n = aux p h' in
          cons i <$> p', rev_append acc (n::t')
    and aux p t =
      match p, t with
        [], Tree.Node (a, []) when Grammar.is_hole E.grammar a -> expand <$> successors a
      | [], Tree.Node (a, []) -> [expand a]
      | i::t, Tree.Node (a, l) -> let+ ps, ts = go [] i 0 t l in ps, Tree.Node (a, ts)
      | _ -> raise (Invalid_argument "Inconsistent path/tree") in
    let path = Paths.find_min_exn h |> tl in
    let+ ps, t' =
      let+ ps, t' = aux path t in
      cons 0 <$> ps, t' in
    Grammar.ast_cost E.pcfg t', fold_left Paths.insert (Paths.delete_min h) ps, t'

  let rec enumerate h =
    let open Option in
    let* sf = H.find_min h in
    if is_ground sf then Some (sf, H.delete_min h)
    else
      let sf's = unroll sf in
      let h' = List.fold_left H.insert (H.delete_min h) sf's in
      enumerate h'

  let sequence =
    let cost s = Grammar.ast_cost E.pcfg (Tree.return s) in
    List.map (fun s -> cost s, Paths.singleton [0], Tree.return s) init
    |> H.of_list
    |> Seq.unfold enumerate
    |> Seq.map (function _, _, t -> t)
end
