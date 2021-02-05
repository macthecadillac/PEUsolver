open Containers
open Fun

type mark = Nil | Min | Solved

(* And nodes with no content is just a different way of writting a
   hypergraph/tree. We'll need to enforce that And nodes are always followed by
   Or nodes *)
type 'a node = And of mark | Or of mark * 'a

module type Node = sig
  type t
  val successors : t -> (int * t) node Tree.t list
  val est_cost : t -> int
end

module type S = sig
  type elt
  type t
  val init : elt -> t
  val solve : t -> t
  val trace : t -> elt Tree.t
end

module Make (N : Node) : S with type elt = N.t = struct

  exception InvalidTree

  type elt = N.t
  type t = (Int.t * N.t) node Tree.t

  let mark m = function
      Tree.Node (Or (_, a), b) -> Tree.Node (Or (m, a), b)
    | Tree.Node (And _, a) -> Tree.Node (And m, a)

  let rec cost = function
      Tree.Node (Or (_, (c, _)), _) -> c
      (* assuming the cost per edge is 1 *)
    | Tree.Node (And _, l) -> List.fold_left (fun acc n -> acc + cost n + 1) 0 l

  let succ =
    N.successors %> function
      [] -> 0, []  (* the cost of a terminal node is 0 *)
    | l ->
        (* a sorted list of costs and corresponding sub-trees *)
        let subtrees =
          List.map cost l
          |> flip List.combine l
          |> List.sort (fun (a, _) (b, _) -> Int.compare a b) in
        (* mark the lowest cost and the corresponding sub-tree *)
        let fstTree = Pair.map_snd (mark Min) (List.hd subtrees) in
        fst fstTree, List.map snd @@ fstTree::List.tl subtrees

  let sort_branches =
    List.map (Pair.dup_map cost)
    %> List.sort (fun (_, a) (_, b) -> Int.compare a b)
    %> List.map fst

  let is_solved = function
      Tree.Node (Or (Solved, _), _) | Tree.Node (And Solved, _) -> true
    | _ -> false

  let init node = Tree.Node (Or (Nil, (N.est_cost node, node)), [])

  let solve t =
    (* a helper function that expands the nodes one level at a time *)
    let rec aux = function
      (* Don't continue solving if the tree is marked as solved *)
        (Tree.Node (Or (Solved, _), _)) as a -> a
      | (Tree.Node (And Solved, _)) as a -> a
      (* This representation of the And/Or tree is defined to be isomorphic to
         the hypergraph representation. This is one invariant we must guarantee.
         *)
      | Tree.Node (And _, []) -> raise InvalidTree
      (* if it is an And node, we need to call aux on every one of its
         descendants since with the way we defined the tree, And itself doesn't
         store data *)
      | Tree.Node (And m, l) ->
          let desc = List.map aux l in
          let m' = if List.for_all is_solved desc then Solved else m in
          Tree.Node (And m', desc)
      (* If it is an Or node with no computed descendants, compute all the
         descendants of the element. If none are found, mark the node as Solved *)
      | Tree.Node (Or (m, (_, n)), []) ->
          let c, l = succ n in
          let m' = if List.is_empty l then Solved else m in
          Tree.Node (Or (m', (c, n)), l)
      (* If it is an Or node with only one computed descendant, expand the
         descendant. Propagate the markings as appropriate *)
      | Tree.Node (Or (m, (_, n)), [a]) ->
          let a' = aux a in
          let m' = if is_solved a' then Solved else m in
          Tree.Node (Or (m', (cost a' + 1, n)), [a'])
      (* If it is an Or node with many computed descendants, expand the first
         node (since we keep the descendants sorted). If the cost of the now
         expanded first node is greater than that of the second node, unmark the
         expanded node, sort the descendants and repeat; otherwise, check for
         termination conditions and mark the expanded node accordingly *)
      | Tree.Node (Or (m, (c, n)), markedNode::((nextNode::_) as rest)) ->
          let expanded = aux markedNode in
          let expandedCost = cost expanded in
          let lowestCostInRest = cost nextNode in
          if expandedCost > lowestCostInRest
          then aux @@ Tree.Node (Or (m, (c, n)), sort_branches @@ mark Nil expanded::rest)
          else
            if is_solved expanded
            then Tree.Node (Or (Solved, (expandedCost + 1, n)), expanded::rest)
            else Tree.Node (Or (m, (expandedCost + 1, n)), mark Min expanded::rest)
    in
    let f t =
      if is_solved t then None
      else let t' = aux t in Some (t', t')
    in
    (* We tug away the "while unsolved do" loop in Seq.unfold which will lazily
       update the tree untill it is solved *)
    Seq.unfold f t
    (* We use fold to force the values. The last element in the sequence is the
       solution. *)
      |> Seq.fold (fun _ x -> x) t

  let rec trace = function
    (* The first node can only be Or, and that And must be followed by Or *)
      Tree.Node (And _, l) -> raise InvalidTree
    | Tree.Node (Or (_, (_, n)), l) ->
        let open List in  (* bring every function in List into scope *)
        let l' =
          (* find the branch marked as solved *)
          find_opt is_solved l |> Option.to_list
          >>= function
                Tree.Node (And _, l') -> map trace l'
              | Tree.Node (Or (_, (_, n')), l'') -> [Tree.Node (n', map trace l'')]
        in Tree.Node (n, l')
end
