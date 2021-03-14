open Containers
open Fun

type 'a printer = Format.formatter -> 'a -> unit

module type PATHORDER = sig
  type t = (int * Grammar.t) list
  val compare : t -> t -> int
end

module type ENV = sig
  val successorsMap : Grammar.t list Grammar.Map.t
  val ast_cost : Grammar.t Tree.t -> float
end

module type S = sig
  val sequence : Grammar.t Tree.t Seq.t
end

module Make (E : ENV) (O : PATHORDER) : S = struct
  module Paths = PairingHeap.Make (O)

  module rec T : sig
    include PairingHeap.ORDERING with type t = float * Paths.t * Grammar.t Tree.t
    val pp : t printer
  end = struct
    type t = float * Paths.t * Grammar.t Tree.t
    let compare (a, _, _) (b, _, _) = Float.compare a b
    let pp fmt (f, p, t) =
      Format.fprintf fmt "Cost: %f AST: %a" f Grammar.ast_pp t
  end

  and H : PairingHeap.S with type elt = T.t = PairingHeap.Make (T)

  let is_ground (_, ps, _) = Paths.is_empty ps

  let successors = Option.get_exn % flip Grammar.Map.get E.successorsMap

  let init = successors Grammar.init

  let unroll (_, h, t) =
    let open List in
    let expand s =
      let l = successors s in
      pure <$> (mapi Pair.make l), Tree.Node (s, Tree.return <$> l) in
    let rec go acc (i, s) n p = function
        [] -> raise (Invalid_argument "Index out of bounds")
      | h'::t' when n <> i -> go (h'::acc) (i, s) (n + 1) p t'
      | h'::t' ->
          let+ p', n = aux p h' in
          cons (i, s) <$> p', rev_append acc (n::t')
    and aux p t =
      match p, t with
        [], Tree.Node (a, []) when Grammar.is_hole E.successorsMap a -> expand <$> successors a
      | [], Tree.Node (a, []) -> [expand a]
      | (i, s)::t, Tree.Node (a, l) -> let+ ps, ts = go [] (i, s) 0 t l in ps, Tree.Node (a, ts)
      | _ -> raise (Invalid_argument "Inconsistent path/tree") in
    let path = Paths.find_min_exn h in
    let pathTl = tl path in
    let+ ps, t' =
      let+ ps, t' = aux pathTl t in
      cons (hd path) <$> ps, t' in
    E.ast_cost t', fold_left Paths.insert (Paths.delete_min h) ps, t'

  let rec enumerate h =
    let open Option.Infix in
    let* sf = H.find_min h in
    if is_ground sf then Some (sf, H.delete_min h)
    else
      let sf's = unroll sf in
      let h' = List.fold_left H.insert (H.delete_min h) sf's in
      enumerate h'

  let sequence =
    let cost s = E.ast_cost (Tree.return s) in
    List.map (fun s -> cost s, Paths.singleton [0, s], Tree.return s) init
    |> H.of_list
    |> Seq.unfold enumerate
    |> Seq.map (function _, _, t -> t)
end
