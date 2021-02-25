(** A modified AO* Algorithm that continues solving until it finds as solution
    that validates *)
open Containers
open Fun

module List = struct
  include List
  let min = function [] -> 0 | hd::tl -> List.fold_left min hd tl
  let sum = List.fold_left (+) 0
end

type 'a printer = Format.formatter -> 'a -> unit

type mark = Nil | Solved

let mark_eq = curry (function
    Nil, Nil | Solved, Solved -> true
  | _ -> false)

let mark_pp fmt = function
    Nil -> Format.fprintf fmt "Nil"
  | Solved -> Format.fprintf fmt "Solved"

type error = SolutionNotFound

let error_pp fmt _ = Format.fprintf fmt "SolutionNotFound"

module type I = sig
  type t
  val equal : t -> t -> bool
  val successors : t -> (int * (int * t) list) list
  val est_cost : t -> int
  val validate : t Tree.t -> bool
  val pp : t printer
end

module type S = sig
  type elt
  type t
  val init : (int * elt) list -> t
  val try_solve : t -> (t, error) result
  val extract : t -> elt Tree.t
  val run : t -> (elt Tree.t, error) result
  val pp : t printer
end

module Make (N : I) : S with type elt = N.t = struct

  exception CorruptState
  exception InvalidTree

  type gen = unit -> (int * N.t) list list

  type and_node = And of mark * int * N.t * or_node list
  and or_node = Or of mark * int * and_node list Lazy.t

  let and_cost = function And (_, c, _, _) -> c

  let or_cost = function Or (_, c, _) -> c

  module H = PairingHeap.Make (struct
    type t = or_node
    let compare a b = Int.compare (or_cost a) (or_cost b)
  end)

  type elt = N.t
  type t = H.t * or_node

  let is_solved_and = mark_eq Solved % function And (m, _, _, _) -> m
  let is_solved_or = mark_eq Solved % function Or (m, _, _) -> m

  (* A bunch of different printers for debugging *)
  let rec list_pp s pp fmt = function
      [] -> ()
    | [a] -> Format.fprintf fmt "\n%s└── %a" s (pp (s ^ "    ")) a
    | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (pp (s ^ "│   ")) hd;
                list_pp s pp fmt tl

  let rec and_pp' s fmt = function
      And (m, c, n, []) -> Format.fprintf fmt "And %a, %i, %a" mark_pp m c N.pp n
    | And (m, c, n, l) ->
        Format.fprintf fmt "And %a %i %a %a" mark_pp m c N.pp n (list_pp s or_pp') l

  and or_pp' s fmt = function
    Or (m, c, l) ->
        Format.fprintf fmt "Or %a %i %a" mark_pp m c (list_pp s and_pp')
    (if Lazy.is_forced l then Lazy.force l else [])

  let and_pp = and_pp' ""
  let or_pp = or_pp' ""

  let rec and_boring_pp fmt (And (m, c, n, l)) =
    Format.fprintf fmt "And %a %i %a %a" mark_pp m c N.pp n
    (List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
             ~pp_start:(fun f () -> Format.fprintf f "[")
             ~pp_stop:(fun f () -> Format.fprintf f "]")
             or_boring_pp) l

  and or_boring_pp fmt (Or (m, c, l)) =
    Format.fprintf fmt "Or %a %i %a" mark_pp m c
    (List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
             ~pp_start:(fun f () -> Format.fprintf f "[")
             ~pp_stop:(fun f () -> Format.fprintf f "]")
             and_boring_pp)
    (if Lazy.is_forced l then Lazy.force l else [])

  let pp fmt (h, t) =
    Format.fprintf fmt "Heap:\n%a\nTree:\n%a" (H.pp or_boring_pp) h or_pp t

  let init l =
    let costs, nodes = List.map (fun (c, n) -> c, And (Nil, c, n, [])) l
      |> List.split in
    let cost = List.min costs in
    let t = Or (Nil, cost, Lazy.pure nodes) in
    let paths = List.map (fun n -> Or (Nil, and_cost n, Lazy.pure [n])) nodes in
    let h = List.fold_left H.insert H.empty paths in
    h, t

  (* Computes the shallow equality of two trees. We only care about the equality
     of the top-most element *)
  let and_eq (And (_, _, a, _)) (And (_, _, b, _)) = N.equal a b

  let rec unmark_and (And (_, c, a, l)) = And (Nil, c, a, List.map unmark_or l)
  and unmark_or (Or (_, c, l)) = Or (Nil, c, Lazy.map (List.map unmark_and) l)

  let sum_desc_costs = List.fold_left (fun acc n -> or_cost n + acc + 1) 0

  let rec while_do_result pred f x =
    let open Result in
    let* y = x in
    if pred y then x
    else while_do_result pred f @@ f y

  let extract (_, t) =
    let rec aux_and = function
        And (_, _, n, []) -> Tree.Node (n, [])
      | And (_, _, n, l) -> Tree.Node (n, List.map aux_or l)
    and aux_or (Or (_, _, l)) = aux_and (List.find is_solved_and (Lazy.force l))
    in aux_or t

  let try_solve =
    let rec aux_and chosenPath tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, And (Solved, _, _, _) -> None
      (* If an And node has no computed descendants, expand to its descendants. *)
      | And (_, _, pn, []), And (m, c, n, []) ->
          let open List in
          let l = N.successors n in
          let desc =
            let+ cost, l' = l in
            let orNode () = map (fun (c, n) -> And (Nil, c, n, [])) l' in
            Or (Nil, cost, Lazy.of_thunk orNode) in
          let c' = c + sum_desc_costs desc in
          let m' = if is_empty l then Solved else m in
          Some ([And (Nil, c', n, desc)], And (m', c', n, desc))
      (* We need to call aux_and on every one of And node's descendants. *)
      | And (_, pc, pn, pl), And (m, c, n, l) ->
          try
            let open Option in
            let* ps, desc =
              List.combine pl l
              |> List.map (uncurry aux_or)
              |> sequence_l
              |> map List.split in
            let c' = c + sum_desc_costs desc in
            let m' = if List.for_all is_solved_or desc then Solved else m in
            let paths =
              let open List in
              let+ branches = List.cartesian_product ps in
              let cost = pc + sum_desc_costs branches in
              And (Nil, cost, n, branches) in
            Some (paths, And (m', c', n, desc))
          with
            (* combine fails when pl is empty. Discard the path and continue *)
            Invalid_argument _ -> None
    and aux_or chosenPath tree : (or_node list * or_node) option =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, Or (Solved, _, _) -> None
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | Or (_, _, pl), Or (m, _, ll) when Lazy.is_forced pl ->
          let open Option in
          let pa = match Lazy.force pl with
              [a] -> a
            | _ -> raise CorruptState in
          let l = Lazy.force ll in
          let eq = List.find (and_eq pa) l in
          let* ps, expanded = aux_and pa eq in
          let rest = List.filter (not % and_eq pa) l in
          let branches = expanded::rest in
          let c = and_cost (List.hd branches) + 5 in
          let m' = if is_solved_and expanded then Solved else m in
          let paths =
            let open List in
            let+ p = ps in
            Or (Nil, and_cost p + 1, Lazy.pure [p]) in
          Some (paths, Or (m', c, Lazy.pure branches))
      (* Descendants have not been enumerated--expand *)
      | _, Or (m, c, l) ->
          let desc = Lazy.force l in
          let paths =
            let open List in
            let+ p = desc in
            let cost = and_cost p + 1 in
            Or (Nil, cost, Lazy.return [p]) in
          Some (paths, Or (m, c, Lazy.return desc))
    in
    (function h, t -> Ok ([], h, t))
    %> while_do_result
       (is_solved_or % function _, _, t -> t)
       (function l, h, t ->
         let open Result in
         let h' = List.fold_left H.insert h l in
         let+ p = H.find_min h' |> of_opt in
         match aux_or p (unmark_or t) with
           None -> [], H.delete_min h', t
         | Some (paths, t') -> paths, H.delete_min h', t')
    %> Result.map (function _, h, t -> h, t)
    %> Result.map_err (const SolutionNotFound)

  let run =
    try_solve
    %> while_do_result
       (N.validate % extract)
       (try_solve % Pair.map_snd unmark_or)
    %> Result.map extract
end
