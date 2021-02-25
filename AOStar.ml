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

  module HashSet = Hashtbl.Make (struct
    type t = elt Tree.t
    let hash = Hash.poly
    let equal a b = hash a = hash b
  end)

  let hs = HashSet.create 10

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
        Format.fprintf fmt "Or %a %i %a" mark_pp m c (list_pp s and_pp') (Lazy.force l)

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
           and_boring_pp) (Lazy.force l)

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

  let stop_on_err _ = function Ok x -> Ok x, `Continue | e -> e, `Stop

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

  let to_tree =
    let rec aux_and = function
        And (_, _, n, []) -> Tree.Node (n, [])
      | And (_, _, n, l) ->
          let forced = List.filter (function Or (_, _, l) -> Lazy.is_forced l) l in
          Tree.Node (n, List.map aux_or forced)
    and aux_or (Or (_, _, l)) =
      match Lazy.force l with
        [] -> raise InvalidTree
      | [a] -> aux_and a
      | _ -> raise CorruptState
    in aux_or

  let enumerate_paths =
    let rec aux_and = function
        And (_, c, n, []) -> [And (Nil, c, n, [])]
      | And (_, _, n, l) ->
          List.map aux_or l
          |> List.cartesian_product
          |> List.map (fun l -> And (Nil, sum_desc_costs l, n, l))
    and aux_or = function
        Or (_, _, l) when Lazy.is_forced l ->
          Lazy.force l
          |> List.concat_map aux_and
          |> List.map (fun n -> Or (Nil, and_cost n, Lazy.pure [n]))
      | node -> [node]
    in aux_or

  let try_solve =
    let rec aux_and chosenPath tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, (And (Solved, _, _, _) as a) -> a
      (* If an And node has no computed descendants, expand to its descendants. *)
      | And (_, _, pn, []), And (m, _, n, []) ->
          let l = N.successors n in
          let desc =
            let open List in
            let+ cost, l' = l in
            let orNode () = map (fun (c, n) -> And (Nil, c, n, [])) l' in
            Or (Nil, cost, Lazy.of_thunk orNode) in
          let c = sum_desc_costs desc in
          let m' = if List.is_empty l then Solved else m in
          And (m', c, n, desc)
      (* We need to call aux_and on every one of And node's descendants. *)
      | And (_, _, pn, pl), And (m, _, n, l) ->
          let desc = List.combine pl l |> List.map (uncurry aux_or) in
          let c = sum_desc_costs desc in
          let m' = if List.for_all is_solved_or desc then Solved else m in
          And (m', c, n, desc)
    and aux_or chosenPath tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, (Or (Solved, _, _) as a) -> a
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | Or (_, _, pl), Or (m, _, ll) when Lazy.is_forced pl ->
          let pa = match Lazy.force pl with
              [a] -> a
            | _ -> raise CorruptState in
          let l = Lazy.force ll in
          let eq = List.find (and_eq pa) l in
          let expanded = aux_and pa eq in
          let rest = List.filter (not % and_eq pa) l in
          let branches = expanded::rest in
          let c = and_cost (List.hd branches) + 1 in
          let m' = if is_solved_and expanded then Solved else m in
          Or (m', c, Lazy.pure branches)
      (* Descendants have not been enumerated--expand *)
      | _, Or (m, c, l) -> Or (m, c, Lazy.(return @@ force l))
    in
    Result.return
    %> while_do_result
       (is_solved_or % snd)
       (function h, t ->
         let open Result in
         let+ hp, p =
           let aux h = H.find_min h |> of_opt |> map (Pair.make h) in
           while_do_result (not % HashSet.mem hs % to_tree % snd)
                           (function h, _ -> aux @@ H.delete_min h)
                           (aux h) in
         let t' = aux_or p t in
         let ts = enumerate_paths t' in
         let h' = List.fold_left H.insert H.empty ts in
         h', t')
    %> Result.map_err (const SolutionNotFound)

  let run =
    try_solve
    %> while_do_result
       (N.validate % extract)
       (fun s ->
         let minPath = extract s in
         HashSet.add hs minPath ();
         try_solve @@ Pair.map H.delete_min unmark_or s)
    %> Result.map extract
end
