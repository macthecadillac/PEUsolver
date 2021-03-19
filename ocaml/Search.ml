open Containers
open Fun

type 'a printer = Format.formatter -> 'a -> unit

module type ENV = sig
  val order : [`HMax | `HMin | `LeftFirst | `RightFirst]
  val succMap : Grammar.t list Grammar.Map.t
  val ast_cost : Grammar.t Tree.t -> float
  val heuristics_w_context : TCOND.tcond list -> AST.t -> float
  val heuristics : Grammar.t -> float
end

module type S = sig
  val sequence : Grammar.t Tree.t Seq.t
end

module Make (E : ENV) : S = struct
  module Paths = PairingHeap.Make (struct
    type t = float * (int * Grammar.t) list
    let rec cmp_indx f a b =
      match a, b with
        [], [] -> 0 | [], _ -> -1 | _, [] -> 1
      | (i1, _)::t1, (i2, _)::t2 when i1 = i2 -> cmp_indx f t1 t2
      | (i1, _)::_, (i2, _)::_ -> f i1 i2
    let compare (c1, p1) (c2, p2) =
      match E.order with
        `LeftFirst -> cmp_indx Int.compare p1 p2
      | `RightFirst -> cmp_indx (flip Int.compare) p1 p2
      | `HMax -> flip Float.compare c1 c2
      | `HMin -> Float.compare c1 c2
  end)

  module rec T : sig
    include PairingHeap.ORDERING with type t = float * Paths.t * Grammar.t Tree.t
    val pp : t printer
  end = struct
    type t = float * Paths.t * Grammar.t Tree.t
    let compare (a, _, _) (b, _, _) = Float.compare a b
    let pp fmt (f, p, t) = Format.fprintf fmt "Cost: %f AST: %a" f AST.pp t
  end

  and H : PairingHeap.S with type elt = T.t = PairingHeap.Make (T)

  let is_ground (_, ps, _) = Paths.is_empty ps

  let successors = Option.get_exn % flip Grammar.Map.get E.succMap

  let init = successors Grammar.init

  let ast_heuristic nts ast =
    let nextNTs, locs = List.split nts in
    let gen_indx cmp init =
      List.foldi (fun (iNT, hNT) i (nt, loc) ->
        let h = E.heuristics_w_context loc ast in
        if cmp h hNT then i, h else iNT, hNT)
      init
      (List.combine nextNTs locs) |> fst in
    let ntIndx =
      match E.order with
        `LeftFirst -> 0
      | `RightFirst -> List.length nextNTs - 1
      | `HMax -> gen_indx (>.) (0, 0.)
      | `HMin -> gen_indx (<.) (0, Float.nan) in
    let heuristic_context_if acc i (nt, loc) =
      let mNT =
        if i = ntIndx
        then E.heuristics_w_context loc ast
        else E.heuristics nt in
      acc +. mNT in
    List.foldi heuristic_context_if 0. (List.combine nextNTs locs)

  let unroll (_, h, t) =
    let open List in
    let expand s =
      let l = successors s in
      l, pure <$> (mapi Pair.make l), Tree.Node (s, Tree.return <$> l) in
    (* find the i-th node and call new_paths *)
    let rec findi acc i n pair = function
        [] -> raise (Invalid_argument "Index out of bounds")
      | h'::t' when n <> i -> findi (h'::acc) i (n + 1) pair t'
      | h'::t' ->
          let+ l, pair', (Tree.Node (a, _) as n) = new_paths pair h' in
          l, cons (i, a) <$> pair', rev_append acc (n::t')
    and new_paths p t =
      match p, t with
        [], Tree.Node (a, []) when Grammar.is_hole E.succMap a -> expand <$> successors a
      | [], (Tree.Node (_, []) as node) -> [[], [], node]
      | (i, _)::t, Tree.Node (a, l) ->
          let+ l, ps, ts = findi [] i 0 t l in
          l, ps, Tree.Node (a, ts)
      | _ -> raise (Invalid_argument "Inconsistent path/tree") in
    let _, path = Paths.find_min_exn h in
    let pathtl = tl path in  (* head is always 0/the root node *)
    let+ ps, t' =
      let+ nextNTs, ps, (Tree.Node (a, _) as t') = new_paths pathtl t in
      let ps' =
        let+ nt, p = combine nextNTs (cons (0, a) <$> ps) in
        let loc =
          let* i = tl @@ fst @@ split p in
          TCOND.(M DownFirst::replicate i (M Right)) in
        let _, nt = Option.get_exn @@ last_opt p in
        E.heuristics_w_context loc t', nt, loc, p in
      ps', t' in
    let nts = (function _, nt, loc, _ -> nt, loc) <$> ps in
    let paths = (function c, _, _, p -> c, p) <$> ps in
    let cost = E.ast_cost t' +. ast_heuristic nts t' in
    cost, fold_left Paths.insert (Paths.delete_min h) paths, t'

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
    let initL =
      let open List.Infix in
      let+ s = init in
      let root = Tree.return s in
      cost s, Paths.singleton (0., [0, s]), root in
    H.of_list initL
    |> Seq.unfold enumerate
    |> Seq.map (function _, _, t -> t)
end
