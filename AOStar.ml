open Containers
open Fun

type mark = Nil | Min | Solved
type 'a tree = Node of 'a * 'a tree list
type 'a node = And of mark | Or of mark * 'a

module type Node = sig
  type t
  val successors : t -> (int * t) node tree list
  val est_cost : t -> int
end

module type S = sig
  type elt
  type u
  val init : elt -> u
  val solve : u -> u
  val trace : u -> elt tree
end

module Make (N : Node) : S with type elt = N.t = struct

  exception InvalidTree

  type elt = N.t
  type u = (Int.t * N.t) node tree

  let mark m = function
      Node (Or (_, a), b) -> Node (Or (m, a), b)
    | Node (And _, a) -> Node (And m, a)

  let rec cost = function
      Node (Or (_, (c, _)), _) -> c
      (* assuming the cost per edge is 1 *)
    | Node (And _, l) -> List.fold_left (fun acc n -> acc + cost n + 1) 0 l

  let succ =
    N.successors %> function
      [] -> 0, []
    | (hd::_) as l ->
        let subtrees =
          List.map cost l
          |> flip List.combine l
          |> List.sort (fun (a, _) (b, _) -> Int.compare a b) in
        let fstTree = Pair.map_snd (mark Min) (List.hd subtrees) in
        fst fstTree, List.map snd @@ fstTree::List.tl subtrees

  let sort_branches =
    List.map (Pair.dup_map cost)
    %> List.sort (fun (_, a) (_, b) -> Int.compare a b)
    %> List.map fst


  let is_solved = function
      Node (Or (Solved, _), _) | Node (And Solved, _) -> true
    | _ -> false

  let init node = Node (Or (Nil, (N.est_cost node, node)), [])

  let rec solve = function
      (* This representation of the And/Or tree is defined to be isomorphic to
         that of the hypergraph representation. This is one invariant we must
         guarantee. *)
      Node (And _, []) -> raise InvalidTree
    | Node (And m, l) ->
        let desc = List.map solve l in
        let m' = if List.for_all is_solved desc then Solved else m in
        Node (And m', desc)
    | Node (Or (m, (_, n)), []) ->
        let c, l = succ n in
        let m' = if List.is_empty l then Solved else m in
        Node (Or (m', (c, n)), l)
    | Node (Or (m, (_, n)), [a]) ->
        let a' = solve a in
        let m' = if is_solved a' then Solved else m in
        Node (Or (m', (cost a' + 1, n)), [a'])
    | Node (Or (m, (c, n)), markedNode::((nextNode::_) as rest)) ->
        let expanded = solve markedNode in
        let expandedCost = cost expanded in
        let lowestCostInRest = cost nextNode in
        if expandedCost > lowestCostInRest
        then solve @@ Node (Or (m, (c, n)), sort_branches @@ mark Nil expanded::rest)
        else
          if is_solved expanded
          then Node (Or (Solved, (expandedCost + 1, n)), expanded::rest)
          else Node (Or (m, (expandedCost + 1, n)), mark Min expanded::rest)

  let rec trace = function
      (* The first node can only be Or, and that And must be followed by Or *)
      Node (And _, l) -> raise InvalidTree
    | Node (Or (_, (_, n)), l) ->
        let open List in
        let l' =
          find_opt is_solved l |> Option.to_list
          >>= function
                Node (And _, l') -> List.map trace l'
              | Node (Or (_, (_, n')), l'') -> [Node (n', List.map trace l'')]
        in Node (n, l')
end
