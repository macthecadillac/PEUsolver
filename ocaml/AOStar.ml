(** A modified AO* Algorithm that continues solving until it finds a solution
    that validates *)
open Containers
open Fun

module C = Common

(* Extend the List module with some handy functions *)
module List = struct
  include List
  let min = function [] -> 0 | hd::tl -> List.fold_left min hd tl
  let sum = List.fold_left (+) 0
end

type mark = Nil | Solved

(* Checks equality for mark *)
let mark_eq = curry (function
    Nil, Nil | Solved, Solved -> true
  | _ -> false)

(* Pretty printer for mark *)
let mark_pp fmt = function
    Nil -> Format.fprintf fmt "Nil"
  | Solved -> Format.fprintf fmt "Solved"

module Make (N : Sig.I) : Sig.S with type elt = N.t = struct

  exception CorruptState
  exception InvalidTree

  (* our and-or tree structure *)
  type and_node = And of mark * int * N.t * or_node list
  and or_node = Or of mark * int * and_node list Lazy.t

  (* compute the costs of And nodes *)
  let and_cost = function And (_, c, _, _) -> c

  (* compute the costs of Or nodes *)
  let or_cost = function Or (_, c, _) -> c

  (* An efficient heap for our priority queue of sentential forms *)
  module H = PairingHeap.Make (struct
    type t = or_node
    let compare a b = Int.compare (or_cost a) (or_cost b)
  end)

  type elt = N.t
  type t = H.t * or_node

  (* Check the status of the search *)
  let is_solved_and = mark_eq Solved % function And (m, _, _, _) -> m

  let is_solved_or = mark_eq Solved % function Or (m, _, _) -> m

  (* printers for pretty printing *)
  let rec and_pp' s fmt = function
      And (m, c, n, []) -> Format.fprintf fmt "And %a, %i, %a" mark_pp m c N.pp n
    | And (m, c, n, l) ->
        Format.fprintf fmt "And %a %i %a %a" mark_pp m c N.pp n (C.list_pp s or_pp') l

  and or_pp' s fmt (Or (m, c, l)) =
    let l' = if Lazy.is_forced l then Lazy.force l else [] in
    Format.fprintf fmt "Or %a %i %a" mark_pp m c (C.list_pp s and_pp') l'

  let and_pp = and_pp' ""
  let or_pp = or_pp' ""

  let list_pp' p =
    List.pp ~pp_sep:(C.str_pp "; ") ~pp_start:(C.str_pp "[") ~pp_stop:(C.str_pp "]") p

  let rec and_boring_pp fmt (And (m, c, n, l)) =
    Format.fprintf fmt "And %a %i %a %a" mark_pp m c N.pp n (list_pp' or_boring_pp) l

  and or_boring_pp fmt (Or (m, c, l)) =
    let l' = if Lazy.is_forced l then Lazy.force l else [] in
    Format.fprintf fmt "Or %a %i %a" mark_pp m c (list_pp' and_boring_pp) l'

  let pp fmt (h, t) =
    Format.fprintf fmt "Heap:\n%a\nTree:\n%a" (H.pp or_boring_pp) h or_pp t

  (* initialize computation *)
  let init l =
    let costs, nodes = List.map (fun (c, n) -> c, And (Nil, c, n, [])) l
      |> List.split in
    let cost = List.min costs in
    let t = Or (Nil, cost, Lazy.pure nodes) in
    let paths = List.map (fun n -> Or (Nil, and_cost n, Lazy.pure [n])) nodes in
    let h = List.fold_left H.insert H.empty paths in
    h, t

  (* Extract the element of an And node *)
  let and_elt (And (_, _, a, _)) = a

  (* Computes the shallow equality of two trees. We only care about the equality
     of the top-most element *)
  let and_eq a b = N.equal (and_elt a) (and_elt b)

  (* Unmark a given And node *)
  let rec unmark_and (And (_, c, a, l)) = And (Nil, c, a, List.map unmark_or l)

  (* Unmark a given Or node *)
  and unmark_or (Or (_, c, l)) = Or (Nil, c, Lazy.map (List.map unmark_and) l)

  (* Helper function to sum all the costs of descendants with path costs *)
  let sum_desc_costs = List.fold_left (fun acc n -> or_cost n + acc + 1) 0

  (* Extract the the result from our internal repr as a Tree.t *)
  let extract =
    let aux_or (Or (_, _, l)) = List.find is_solved_and (Lazy.force l) in
    let aux_and = function
        And (_, _, n, []) -> n, []
      | And (_, _, n, l) -> n, List.map aux_or l
    in Tree.unfold aux_and % aux_or % snd

  (* Expand the branches and try to find a solution *)
  let try_solve (h, t) =
    let rec aux_and chosenPath tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, And (Solved, _, _, _) -> raise InvalidTree
      (* If an And node has no computed descendants, expand to its descendants. *)
      | And (_, pc, _, []), And (m, c, n, l) ->
          let open List in
          let s = N.successors n in
          let desc =
            let+ cost, l' = s in
            let orNode () = map (fun (c, n) -> And (Nil, c, n, [])) l' in
            Or (Nil, cost, Lazy.of_thunk orNode) in
          let c' = sum_desc_costs desc in
          let l' = if is_empty l then desc else l in
          let m' = if is_empty l' then Solved else m in
          [And (Nil, pc + c', n, desc)], And (m', c + c', n, l')
      (* We need to call aux_or on every one of And node's descendants. *)
      | And (_, pc, pn, pl), And (m, c, n, l) ->
          let lowestCost = List.map or_cost pl |> List.min in
          let rec f acc = function
            [] -> [], List.rev acc |> List.split |> snd
          | (p, n) as head::tail ->
              let open List in
              if or_cost p <> lowestCost
              then f (head::acc) tail
              else
                let ps, n' = aux_or p n in
                let desc = rev_append acc ((p, n')::tail) |> split |> snd in
                let t = pure % fst <$> tail in
                let h = pure % fst <$> acc in
                let paths = rev_append h (ps::t) in
                paths, desc in
          let ps, desc = List.combine pl l |> f [] in
          (* Format.printf "Node: %a\n" N.pp n; *)
          (* Format.printf "PS LENGTH: %i\n" (List.length ps); *)
          (* Format.printf "PATHS BEFORE:\n%a\n\n" (list_pp' or_boring_pp) pl; *)
          (* Format.printf "PATHS AFTER:\n%a\n\n" (list_pp' (list_pp' or_boring_pp)) ps; *)
          (* Format.print_flush (); *)
          (* let _ = read_line () in *)
          let c' = c + sum_desc_costs desc in
          let m' = if List.for_all is_solved_or desc then Solved else m in
          let paths =
            let open List in
            let+ branches = cartesian_product ps in
            let cost = pc + sum_desc_costs branches in
            And (Nil, cost, n, branches) in
          paths, And (m', c', n, desc)
    and aux_or chosenPath tree : or_node list * or_node =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, Or (Solved, _, _) -> raise InvalidTree
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | Or (_, _, pl), Or (m, _, ll) when Lazy.is_forced pl ->
          let pa = match Lazy.force pl with
              [a] -> a
            | _ -> raise CorruptState in
          let l = Lazy.force ll in
          let eq = List.find (and_eq pa) l in
          let ps, expanded = aux_and pa eq in
          let rest = List.filter (not % and_eq pa) l in
          let branches = expanded::rest in
          let c = and_cost (List.hd branches) + 1 in
          let m' = if is_solved_and expanded then Solved else m in
          let paths =
            let open List in
            let+ p = ps in
            Or (Nil, and_cost p + 1, Lazy.pure [p]) in
          paths, Or (m', c, Lazy.pure branches)
      (* Descendants have not been enumerated--expand *)
      | Or (_, _, pl), Or (m, c, ll) ->
          let desc = Lazy.force pl in
          let paths =
            let open List in
            let+ p = desc in
            let cost = and_cost p + 1 in
            Or (Nil, cost, Lazy.return [p]) in
          let ll' = if Lazy.is_forced ll then ll else Lazy.return desc in
          paths, Or (m, c, ll')
    in
    C.while_do_result
       (is_solved_or % function _, _, t -> t)
       (function l, h, t ->
         (* Format.printf "New iter:\n\n"; *)
         let open Result in
         let h' = List.fold_left H.insert h l in
         let+ p = H.find_min h' |> of_opt in
         let paths, t' = aux_or p (unmark_or t) in
         paths, H.delete_min h', t')
       (Ok ([], h, t))
       (* we drop the path list when a solution is found--the list is a
          singleton that contains the solution *)
    |> Result.map (function _, h, t -> h, t)
    |> Result.map_err (const C.SolutionNotFound)

  (* Repeatedly calls try_solve until a solution validates *)
  let run =
    try_solve
    %> C.while_do_result
       (N.validate % extract)
       (try_solve % Pair.map_snd unmark_or)
    %> Result.map extract
end
