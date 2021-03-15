open Containers
open Fun

type 'a printer = Format.formatter -> 'a -> unit

module type PATHORDER = sig
  type t = (int * Grammar.t) list
  val compare : t -> t -> int
end

module type ENV = sig
  val succMap : Grammar.t list Grammar.Map.t
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
      Format.fprintf fmt "Cost: %f AST: %a" f AST.pp t
  end

  and H : PairingHeap.S with type elt = T.t = PairingHeap.Make (T)

  let is_ground (_, ps, _) = Paths.is_empty ps

  let successors = Option.get_exn % flip Grammar.Map.get E.succMap

  let init = successors Grammar.init

  let unroll (_, h, t) =
    let open List in
    let expand s =
      let l = successors s in
      pure <$> (mapi Pair.make l), Tree.Node (s, Tree.return <$> l) in
    (* find the i-th node and call new_paths *)
    let rec traverse acc (i, rule) n pair = function
        [] -> raise (Invalid_argument "Index out of bounds")
      | h'::t' when n <> i -> traverse (h'::acc) (i, rule) (n + 1) pair t'
      | h'::t' ->
          let+ pair', n = new_paths pair h' in
          cons (i, rule) <$> pair', rev_append acc (n::t')
    and new_paths p t =
      match p, t with
        [], Tree.Node (a, []) when Grammar.is_hole E.succMap a -> expand <$> successors a
      | [], Tree.Node (a, []) -> [expand a]  (* wouldn't that just return [[], node]? *)
      | (i, rule)::t, Tree.Node (a, l) ->
          let+ ps, ts = traverse [] (i, a) 0 t l in
          ps, Tree.Node (a, ts)
      | _ -> raise (Invalid_argument "Inconsistent path/tree") in
    let path = Paths.find_min_exn h in
    let pathtl = tl path in  (* head is always 0/the root node *)
    let+ ps, t' =
      let+ ps, t' = new_paths pathtl t in
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
