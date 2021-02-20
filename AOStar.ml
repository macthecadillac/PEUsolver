(** A modified AO* Algorithm that continues solving until it finds as solution
    that validates *)
open Containers
open Fun

module List = struct
  include List
  let map_nth n f l =
    let rec aux m acc = function
        [] -> []
      | hd::tl when m = n -> rev_append acc @@ f hd::tl
      | hd::tl -> aux (m + 1) (hd::acc) tl
    in aux 0 [] l
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
  val successors : t -> (int * t) list list
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

  type or_node = Or of mark * int * and_node list
  and and_node = And of mark * int * N.t * or_node list

  let and_cost = function And (_, c, _, _) -> c

  let or_cost = function Or (_, c, _) -> c

  module H = PairingHeap.Make (struct
    type t = int * int list
    let compare (i, a) (j, b) = Int.compare i j
  end)

  type elt = N.t
  type t = H.t * or_node

  module HashSet = Hashtbl.Make (struct
    type t = int list
    let hash = Hash.poly
    let equal a b = hash a = hash b
  end)

  let hs : unit HashSet.t = HashSet.create 10
  let a = HashSet.add hs [1; 2; 3] ()

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
      Or (m, c, []) -> Format.fprintf fmt "Or %a, %i" mark_pp m c
    | Or (m, c, l) ->
        Format.fprintf fmt "Or %a %i %a" mark_pp m c (list_pp s and_pp') l

  let and_pp = and_pp' ""
  let or_pp = or_pp' ""

  let str_pp s fmt () = Format.fprintf fmt s

  let path_pp : int list printer =
    List.pp ~pp_sep:(str_pp "; ")
            ~pp_start:(str_pp "[")
            ~pp_stop:(str_pp "]")
            Int.pp

  let heap_pp : (int * int list) printer =
    Pair.pp ~pp_sep:(str_pp ", ")
            ~pp_start:(str_pp "(")
            ~pp_stop:(str_pp ")")
            Int.pp path_pp

  let pp fmt (h, t) =
    Format.fprintf fmt "Heap:\n%a\nTree:\n%a" (H.pp heap_pp) h or_pp t

  let pick_min = function [] -> 0 | hd::tl -> List.fold_left min hd tl

  let init nodes =
    let and_nodes = List.map (fun (cost, n) -> And (Nil, cost, n, [])) nodes in
    let cost = pick_min @@ List.map fst nodes in
    let t = Or (Nil, cost, and_nodes) in
    let _, h = List.fold_left
      (fun (i, h) (c, _) -> i + 1, H.insert h (c, [i]))
      (0, H.empty) nodes in
    h, t

  let sum_cost = List.fold_left (+) 0

  (* Computes the shallow equality of two trees. We only care about the equality
     of the top-most element *)
  let and_eq (And (_, _, a, _)) (And (_, _, b, _)) = N.equal a b

  let or_eq (Or (_, _, l1)) (Or (_, _, l2)) =
    List.length l1 = List.length l2
    && List.combine l1 l2
       |> List.for_all (uncurry and_eq)

  let rec unmark_and (And (_, c, a, l)) = And (Nil, c, a, List.map unmark_or l)
  and unmark_or (Or (_, c, l)) = Or (Nil, c, List.map unmark_and l)

  let stop_on_err _ = function Ok x -> Ok x, `Continue | e -> e, `Stop

  let sum_desc_costs = List.fold_left (fun acc n -> or_cost n + acc + 1) 0

  let enumerate_paths : or_node -> (int * int list) list =
    let build_path f c i n =
      f n |> List.split
          |> Pair.map (List.map ((+) (c + 1))) (List.map (List.cons i))
          |> uncurry List.combine in
    let rec aux_and = function
        And (_, c, n, []) -> [c, []]
      | And (_, c, n, l) -> List.concat @@ List.mapi (build_path aux_or c) l
    and aux_or = function
        Or (_, _, []) -> raise InvalidTree
      | Or (_, _, l) -> List.concat @@ List.mapi (build_path aux_and 0) l
    in aux_or

  let rec while_do_result pred f x =
    let open Result in
    let x' = x >>= f in
    let* y = x' in
    if pred y then x'
    else while_do_result pred f x'

  let process_tree pred =
    let rec aux_and = function
        And (_, _, n, []) -> Tree.Node (n, [])
      | And (_, _, n, l) -> Tree.Node (n, List.map aux_or l)
    and aux_or = function
        Or (_, _, []) -> raise InvalidTree
      | Or (_, _, l) -> aux_and (List.find pred l)
    in aux_or

  let extract = process_tree is_solved_and % snd

  let to_tree = process_tree (fun _ -> raise CorruptState)

  let try_solve =
    let rec aux_and (chosenPath : int list) tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, (And (Solved, _, _, _) as a) -> a
      (* If an And node has no computed descendants, expand to its descendants. *)
      | [], And (m, _, n, []) ->
          let l = N.successors n in
          let desc =
            List.map (fun l ->
              let orNode = List.map (fun (c, n) -> And (Nil, c, n, [])) l in
              let cost = pick_min @@ List.map and_cost orNode in
              Or (Nil, cost, orNode))
            l in
          let c = sum_desc_costs desc in
          let m' = if List.is_empty l then Solved else m in
          And (m', c, n, desc)
      (* We need to call aux_and on every one of And node's descendants. *)
      | i::p, And (m, _, n, l) ->
          let branches = List.map_nth i (aux_or p) l in
          let c = sum_desc_costs branches in
          let m' = if List.for_all is_solved_or branches then Solved else m in
          And (m', c, n, branches)
      | a, b ->
          Format.printf "a: %a\nb: %a\n" path_pp a and_pp b;
          raise CorruptState
    and aux_or (chosenPath : int list) tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        _, (Or (Solved, _, _) as a) -> a
      (* We do not allow Or nodes with no descendants *)
      | _, Or (m, _, []) -> raise InvalidTree
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | i::p, Or (m, _, l) ->
          let branches = List.map_nth i (aux_and p) l in
          let c = pick_min (List.map and_cost branches) + 1 in
          let m' = if List.exists is_solved_and branches then Solved else m in
          Or (m', c, branches)
      | a, b ->
          Format.printf "a: %a\nb: %a\n" path_pp a or_pp b;
          raise CorruptState
    in
    Result.return
    %> while_do_result
       (is_solved_or % snd)
       (function h, t ->
         let open Result in
         let* hp, p =
           let aux h = H.find_min h |> of_opt |> map (Pair.make h %> Pair.map_snd snd) in
           while_do_result (not % HashSet.mem hs % snd)
                           (function h, _ -> aux @@ H.delete_min h)
                           (aux h) in
         let t' = aux_or p t in
         (* Format.printf "%a\n" or_pp t'; *)
         (* read_line (); *)
         HashSet.add hs p ();
         let ts = enumerate_paths t' in
         let h' = List.fold_left H.insert H.empty ts in
         Ok (h', t'))
    %> Result.map_err (const SolutionNotFound)

  let run =
    try_solve
      %> while_do_result
         (N.validate % extract)
         (fun s ->
           (* let minPath = extract s in *)
           (* HashSet.add hs minPath (); *)
           try_solve @@ Pair.map H.delete_min unmark_or s)
      %> Result.map extract
end
