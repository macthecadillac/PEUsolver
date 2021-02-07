(** A modified AO* Algorithm that continues solving until it finds as solution
    that validates *)
open Containers
open Fun

type 'a printer = Format.formatter -> 'a -> unit

type mark = Nil | Solved

let mark_eq = curry (function
    Nil, Nil | Solved, Solved -> true
  | _ -> false)

let mark_pp fmt = function
    Nil -> Format.fprintf fmt "Nil"
  | Solved -> Format.fprintf fmt "Solved"

type 'a node = And of 'a | Or of 'a

let node_pp pp_item fmt = function
    And a -> Format.fprintf fmt "And %a" pp_item a
  | Or a -> Format.fprintf fmt "Or %a" pp_item a

module type I = sig
  type t
  val equal : t -> t -> bool
  val successors : t -> (mark * (int * t)) node Tree.t list
  val est_cost : t -> int
  val validate : t Tree.t -> bool
  val pp : t printer
end

module type S = sig
  type elt
  type t
  val init : elt -> t
  val try_solve : t -> t
  val extract : t -> elt Tree.t
  val run : t -> elt Tree.t
  val pp : t printer
end

module Make (N : I) : S with type elt = N.t = struct

  exception InvalidTree
  exception CorruptState
  exception SolutionNotFound

  let rec cost = function
      Tree.Node (Or (_, (c, _)), _) | Tree.Node (And (_, (c, _)), _) -> c

  let is_solved =
    mark_eq Solved
    % function Tree.Node (Or (m, _), _) | Tree.Node (And (m, _), _) -> m

  module H = PairingHeap.Make (struct
    type t = (mark * (Int.t * N.t)) node Tree.t
    let compare t1 t2 = Int.compare (cost t1) (cost t2)
  end)

  type elt = N.t
  type t = H.t * (mark * (Int.t * N.t)) node Tree.t

  let pp fmt (h, t) =
    let pp_sep fmt () = Format.fprintf fmt ", " in
    let pp_start fmt () = Format.fprintf fmt "(" in
    let pp_stop fmt () = Format.fprintf fmt ")" in
    let tree_pp f =
      f
      @@ node_pp
      @@ Pair.pp ~pp_start ~pp_stop ~pp_sep mark_pp
      @@ Pair.pp ~pp_start ~pp_stop ~pp_sep Int.pp N.pp in
    Format.fprintf fmt "Heap:\n%a\nTree:\n%a"
      (H.pp (tree_pp Tree.debug)) h
      (tree_pp Tree.pp) t

  let init node =
    let t = Tree.Node (Or (Nil, (N.est_cost node, node)), []) in
    let h = H.insert H.empty t in
    h, t

  let sort_branches =
    List.map (Pair.dup_map cost)
    %> List.sort (fun (_, a) (_, b) -> Int.compare a b)
    %> List.map fst

  let succ = N.successors %> function [] -> [] | l -> sort_branches l

  (* Computes the shallow equality of two trees. We only care about the equality
     of the top-most element *)
  let shallow_eq = curry (function
      Tree.Node (Or (_, (_, a)), _), Tree.Node (Or (_, (_, b)), _) -> N.equal a b
    | Tree.Node (And (_, (_, a)), _), Tree.Node (And (_, (_, b)), _) -> N.equal a b
    | _ -> false)

  let rec unmark = function
      Tree.Node (Or (_, a), l) -> Tree.Node (Or (Nil, a), List.map unmark l)
    | Tree.Node (And (_, a), l) -> Tree.Node (And (Nil, a), List.map unmark l)

  let sum_desc_costs = List.fold_left (fun acc n -> cost n + acc + 1) 0

  let try_solve a =
    (* a helper function that expands the nodes one level at a time *)
    let rec aux path tree =
      match path, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, ((Tree.Node (Or (Solved, _), _)) as a) -> [path], a
      | _, ((Tree.Node (And (Solved, _), _)) as a) -> [path], a
      (* If the And node has an element (an ordinary tree), then expand to its
         descendants. Afterward, compute all the legal descendant combintations. *)
      | Tree.Node (And (_, (_, pn)), []), Tree.Node (And (m, (_, n)), []) ->
          let l = succ n in
          let c = sum_desc_costs l in
          let newPaths = [Tree.Node (And (Nil, (c, pn)), l)] in
          let m' = if List.is_empty l then Solved else m in
          newPaths, Tree.Node (And (m', (c, n)), l)
      (* if it is an And node, we need to call aux on every one of its
         descendants since with the way we defined the tree, And itself doesn't
         store data. Afterward, compute all the legal descndant combinations. *)
      | Tree.Node (And (_, (_, pn)), pl), Tree.Node (And (m, (_, n)), l) ->
          let pls', desc =
            List.combine (sort_branches pl) l
            |> List.map (uncurry aux)
            |> List.split in
          let c = sum_desc_costs desc in
          let m' = if List.for_all is_solved desc then Solved else m in
          let newPaths =
            let open List in
            (* Compute the cartesian product of all the newly computed paths
               (paths from branch 1 x paths from branch 2 x paths from branch 3
               and so on). This gives the possible paths from this node onward.
               Afterward, combine with the current path *)
            fold_left (fun acc l ->
              let* x = l in
              let* a = acc in
              return @@ x::a)
            [[]] pls'
            |> map (fun ts -> Tree.Node (And (Nil, (sum_desc_costs ts, pn)), ts)) in
          newPaths, Tree.Node (And (m', (c, n)), desc)
      (* If it is an Or node with no computed descendants, compute all the
         descendants of the element. If none are found, mark the node as Solved *)
      | Tree.Node (Or (_, (_, pn)), []), Tree.Node (Or (m, (_, n)), []) ->
          let l = succ n in
          let m', c = if List.is_empty l then Solved, 0 else m, cost (List.hd l) + 1 in
          let newPaths = List.map (fun t -> Tree.Node (Or (Nil, (cost t + 1, pn)), [t])) l in
          newPaths, Tree.Node (Or (m', (c, n)), l)
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. Afterward,
         compute a list of possible paths with the newly computed nodes and
         combine with the current path. *)
      | Tree.Node (Or (_, (_, pn)), [pa]), Tree.Node (Or (m, (_, n)), l) ->
          let eq = List.find (shallow_eq pa) l in
          let pas', expanded = aux pa eq in
          let rest = List.filter (not % shallow_eq pa) l in
          let m' = if is_solved expanded then Solved else m in
          (* the expanded node might have a higher cost than the rest *)
          let sortedBranches = sort_branches @@ expanded::rest in
          let c = cost (List.hd sortedBranches) + 1 in
          let newPaths = List.map (fun t -> Tree.Node (Or (Nil, (cost t + 1, pn)), [t])) pas' in
          newPaths, Tree.Node (Or (m', (c, n)), sortedBranches)
      | _ -> raise CorruptState
    in
    (* We tuck the "while unsolved do" loop into Seq.t. The Seq.unfold function
       lazily evaluates the tree after each iteration of the loop and the
       Seq.fold function forces the values until the last element, which will be
       our solution. *)
    Seq.unfold (fun (h, t) ->
      if is_solved t then None
      else
        let p =
          match H.find_min h with
            Some a -> a
          | None -> raise SolutionNotFound in
        let ts, t' = aux p t in
        let h' = List.fold_left H.insert (H.delete_min h) ts in
        Some ((h', t'), (h', t'))) a
      |> Seq.fold (fun _ x -> x) a

  let extract (_, t) =
    let rec aux = function
        Tree.Node (And (_, (_, n)), [])
      | Tree.Node (Or (_, (_, n)), []) -> Tree.Node (n, [])
      | Tree.Node (And (_, (_, n)), l) -> Tree.Node (n, List.map aux l)
      | Tree.Node (Or (_, (_, n)), l) ->
          Tree.Node (n, [aux @@ List.find is_solved l])
    in aux t

  let run a =
    let a' = try_solve a in
    (* keep solving until validated (or errors out with no solution found) *)
    Seq.unfold (fun s ->
      if N.validate @@ extract s then None
      else 
        let s' = try_solve @@ Pair.map_snd unmark s in
        Some (s', s')) a'
      |> Seq.fold (fun _ x -> x) a'
      |> extract
end
