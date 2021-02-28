(** A modified A* Algorithm that continues solving until it finds a solution
    that validates *)
open Containers
open Fun

module C = Common

(* Extend the List module with some handy functions *)
module List = struct
  include List
  let min = function
    [] -> None
  | hd::tl -> List.fold_left (fun acc i -> Option.(min i <$> acc)) (Some hd) tl
  let sum = List.fold_left (+) 0
end

type mark = Visited | Unvisited | Solved

(* Checks equality for mark *)
let mark_eq = curry (function
    Visited, Visited | Unvisited, Unvisited | Solved, Solved -> true
  | _ -> false)

(* Pretty printer for mark *)
let mark_pp fmt = function
    Visited -> Format.fprintf fmt "Visited"
  | Unvisited -> Format.fprintf fmt "Unvisited"
  | Solved -> Format.fprintf fmt "Solved"

module Make (N : Sig.I) : Sig.S with type elt = N.t = struct
  module rec T : sig
    type node = { elt : N.t; cost : int; mark : mark;
                  desc : H.t Lazy.t; right : H.t Lazy.t }
    val pp : node C.printer
  end = struct
    (* [desc] are the descendants of the node--they are the possible sentential
       forms that go into holes created by the current term.
       [right] are right siblings of the current node--they are terms that go
       into following holes of the parent of the current node. *)
    type node = { elt : N.t;
                  cost : int; (* g(n) + h(n) *)
                  mark : mark;
                  desc : H.t Lazy.t;
                  right : H.t Lazy.t }

    let rec pp fmt node =
      Format.fprintf fmt "Node %a, %i, %a\ndesc: %a\nright:%a"
      N.pp node.elt node.cost mark_pp node.mark
      (H.pp pp)
      (if Lazy.is_forced node.desc then Lazy.force node.desc else H.empty)
      (H.pp pp)
      (if Lazy.is_forced node.right then Lazy.force node.right else H.empty)
  end

  and I : PairingHeap.ORDERING with type t = T.node = struct
    open T
    type t = node
    let compare a b = Int.compare a.cost b.cost
  end

  and H : PairingHeap.S with type elt = I.t = PairingHeap.Make (I)

  open T

  type elt = N.t
  type t = H.t

  let pp = H.pp T.pp

  let is_solved_node n = mark_eq Solved n.mark

  let unvisited n = mark_eq Unvisited n.mark

  let is_solved_t t = Option.fold (fun _ -> is_solved_node) false (H.find_min t)

  let get_right n = n.right

  let get_desc n = n.desc

  let get_cost n = n.cost

  let init l =
    let mark = Unvisited
    and desc = Lazy.pure H.empty
    and right = Lazy.pure H.empty in
    let aux (cost, elt) = { elt; cost; mark; desc; right } in
    let elts : H.elt list = List.map aux l in
    List.fold_left H.insert H.empty elts

  (* list all siblings in the current sentential form line-up *)
  let rec list_siblings node =
    let open Lazy in
    let* right = node.right in
    match H.find_min right with
      None -> pure []
    | Some fstSibling -> List.cons fstSibling <$> list_siblings fstSibling

  (* list all descendants in the current sentential form line-up *)
  let list_descendants node =
    let open Lazy in
    let* desc = node.desc in
    match H.find_min desc with
      None -> Lazy.pure []
    | Some fstDesc -> List.cons fstDesc <$> list_siblings fstDesc

  let lowest_cost_among_siblings r =
    let open Lazy in
    let+ siblings = list_siblings r in
    List.min (List.map get_cost siblings)

  let pick_by_cost firstDesc nextSibling =
    let open Lazy in
    let* descMinCost = lowest_cost_among_siblings firstDesc in
    let+ siblingMinCost = lowest_cost_among_siblings nextSibling in
    let open Option in
    let+ d = descMinCost and+ s = siblingMinCost in
    if d > s then nextSibling else firstDesc

  let extract =
    H.find_min_exn
    %> Tree.unfold (fun t -> t.elt, Lazy.force (list_descendants t))

  let unmark h =
    let open Lazy in
    let min_cost n1 n2 = Option.(min <$> (get_cost <$> n1) <*> (get_cost <$> n2)) in
    (* Check that this node is connected to any other node outside of its parent *)
    let is_last_sentential_form t =
      let+ d = t.desc and+ r = t.right in
      H.is_empty d && H.is_empty r in
    let rec go tree : T.node option Lazy.t =
      (* Format.printf "\n\nGO:\n%a\n\n" T.pp tree; *)
      (* An abstracted helper function that removes the first child of either
         desc or right with the given getter then updates the cost of the node. *)
      let aux get1 get2 update t =
        let* nodes1 = get1 t and* nodes2 = get2 t in
        let firstChild = Option.get_exn (H.find_min nodes1) in (* should not fail *)
        let+ fc_opt = go firstChild in  (* dispatch a recursive call and repeat *)
        let open Option in
        let* fc = fc_opt in
        (* remove the lowest child from the chosen node since that's on the path
           of the discarded solution *)
        let nodes1' = H.delete_min nodes1 in
        (* if the now updated first child contains no more subtrees, remove it *)
        let b = force (is_last_sentential_form fc) in
        let ns = if b then nodes1' else H.insert nodes1' fc in
        (* calculate the new cost after node removal. This should not
           crash--terminal nodes should have been filtered before it got here *)
        let+ cost = min_cost (H.find_min ns) (H.find_min nodes2) in
        update t cost ns in
      (* remove minimum branches from t.desc and update the costs *)
      let rm_d t =
        let update t cost n = { t with cost; desc = pure n } in
        aux get_desc get_right update t in
      (* remove minimum branches from t.right and update the costs *)
      let rm_r t =
        let update t cost n = { t with cost; right = pure n } in
        aux get_right get_desc update t in
      (* remove a node from the minimum branch if it is found using a provided
         removal function *)
      let rm_if_found_min get rm t =
        (* Format.printf "\n\nRM IF FOUND MIN:\n%a\n\n" T.pp t; *)
        let* children = get t in
        (* Format.printf "HAS CHILDREN: %b\n\n" (H.find_min children |> Option.is_some); *)
        if Option.is_some (H.find_min children) then rm t else pure None in
      (* remove nodes with no descendants and siblings *)
      let rm_last_sf t =
        let* t_opt = rm_if_found_min get_right rm_r t in
        Option.fold (fun _ -> rm_if_found_min get_desc rm_d) (pure None) t_opt
      in
      let+ t = rm_last_sf tree in
      Option.map (fun t -> { t with mark = Visited }) t
    in
    match H.find_min h with
      None -> pure h
    | Some t ->
        let* b = is_last_sentential_form t in
        if b then pure @@ H.delete_min h
        else
          let+ t' = go t in
          Option.fold H.insert (H.delete_min h) t'

  let expand_tree tree =
    (* add descendants based on the output of N.successors *)
    let rec add_descs l () =
      match l with
        [] -> H.empty
      | (cost, descs)::tl ->
          let f acc (cost, elt) = H.insert acc {
            cost;
            elt;
            mark = Unvisited;
            desc = Lazy.pure H.empty;
            right = Lazy.of_thunk (add_descs tl) } in
          List.fold_left f H.empty descs in
    let desc = N.successors tree.elt |> add_descs |> Lazy.of_thunk in
    Lazy.pure @@ Some { tree with mark = Visited; desc }

  let mark_solved node = { node with mark = Solved }

  (* TODO: maybe recursively index descendant and sibling nodes to pick which to
     visit first *)
  let rec visit_cheapest_branch tree =
    let open Lazy in
    let* right = tree.right in
    let* desc = tree.desc in
    let set_r t cost mark right = { t with cost; mark; right } in
    let set_d t cost mark desc = { t with cost; mark; desc } in
    (* propagate changes from lower levels of the tree upward *)
    let propagate get set otherCost n =
      let mark = if is_solved_node n then Solved else Visited in
      let+ nodeH = get n in
      let nodes = H.insert (H.delete_min nodeH) n in
      let cost =
        (* find the smaller cost between the min of descendants and that of
           the right *)
        let n' = H.find_min_exn nodes in
        Option.fold (fun c1 c2 -> min c1 c2) n'.cost otherCost in
      set tree cost mark (pure nodes) in
    (* utility function to handle back propagating. otherCost is the cost of
       the other branch *)
    let aux get set otherCost n =
      traverse n >>= (function
          None -> Lazy.pure None
        | Some x' -> Option.return <$> propagate get set otherCost x') in
    let* all_desc_solved =
      let+ allDescs = list_descendants tree in
      List.for_all is_solved_node allDescs in
    if all_desc_solved
    then pure @@ Some (mark_solved tree)
    else
      match H.find_min desc, H.find_min right with
        Some a, None | None, Some a when is_solved_node a -> visit_cheapest_branch a
      (* if both the immediate descendant and the next sibling are solved, go
         visit a sibling that hasn't been solved *)
      | Some d, Some r when is_solved_node d && is_solved_node r -> visit_cheapest_branch r
      | Some d, Some r when is_solved_node d -> aux get_right set_r (Some d.cost) r  (* r isn't solved *)
      | Some d, Some r when is_solved_node r -> aux get_desc set_d (Some r.cost) d   (* d isn't solved *)
      | Some d, None -> aux get_desc set_d None d
      | None, Some r -> aux get_desc set_d None r
      | Some d, Some r -> (* solve whatever that has the lower cost first *)
          if d.cost > r.cost
          then aux get_desc set_d (Some r.cost) d
          else aux get_right set_r (Some d.cost) r
      | None, None ->
          let l = N.successors tree.elt in
          if List.is_empty l  (* A node is terminal if it has no successors *)
          then pure @@ Some { tree with mark = Solved }
          else pure None  (* not a terminal--no solution *)

  and traverse tree =
    if unvisited tree
    then expand_tree tree
    else visit_cheapest_branch tree

  let try_solve h =
    C.while_do_result
      (is_solved_t)
      (function h ->
        (* Format.printf "Current AST:\n%a\n\n\n" pp h; *)
        (* let _ = read_line () in *)
        Option.map (Lazy.force % traverse) (H.find_min h)
        |> Option.flatten
        |> Option.map (H.insert (H.delete_min h))
        |> Option.to_result C.SolutionNotFound)
      (Ok h)

  let run =
    try_solve
    %> C.while_do_result
       (N.validate % extract)
       (fun h ->
         Format.printf "Before SOLVING:\n%a\n\n" pp h;
         let h' = try_solve @@ Lazy.force @@ unmark h in
         Format.printf "AFTER SOLVING:\n%a\n\n" (Result.pp' pp C.error_pp) h';
         h')
    %> Result.map extract
end
