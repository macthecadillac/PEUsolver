(** Or modified AO* Algorithm that continues solving until it finds as solution
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

type error = SolutionNotFound

let error_pp fmt _ = Format.fprintf fmt "SolutionNotFound"

module type I = sig
  type t
  val equal : t -> t -> bool
  val successors : t -> (int * t) list list
  val est_cost : t -> int
  val validate : t Tree.t -> bool
  val pp : t printer
end

module type S = sig
  type elt
  type t
  val init : elt list -> t
  val try_solve : t -> t
  val extract : t -> elt Tree.t
  val run : t -> elt Tree.t
  val pp : t printer
end

module Make (N : I) : S with type elt = N.t = struct

  exception CorruptState
  exception InvalidTree

  module H = PairingHeap.Make (struct
    type t = int * (N.t list)
    let compare a b = Int.compare (fst a) (fst b)
  end)

  (* The And-Or tree type *)
  type and_node = And of mark * H.t * N.t * or_node list
  and or_node = Or of mark * and_node list

  let pick_min = function
      [] -> 0
    | hd::tl -> List.fold_left min hd tl

  let and_cost = function And (_, h, n, _) ->
    match H.find_min h with
      None -> N.est_cost n
    | Some m -> fst m

  let or_cost = function Or (_, l) -> pick_min @@ List.map and_cost l

  type elt = N.t
  type t = or_node

  module HashMap = Hashtbl.Make (struct
    type t = N.t list
    let hash = Hash.poly
    let equal a b = hash a = hash b
  end)

  let is_solved_and = mark_eq Solved % function And (m, _, _, _) -> m
  let is_solved_or = mark_eq Solved % function Or (m, _) -> m

  (* A bunch of different printers for debugging *)
  let str_pp s fmt () = Format.fprintf fmt s
  let lpp = List.pp ~pp_sep:(str_pp "; ") ~pp_start:(str_pp "[") ~pp_stop:(str_pp "]")
  let ppp = Pair.pp ~pp_sep:(str_pp ", ") ~pp_start:(str_pp "(") ~pp_stop:(str_pp ")")

  let rec list_pp s pp fmt = function
      [] -> ()
    | [a] -> Format.fprintf fmt "\n%s└── %a" s (pp (s ^ "    ")) a
    | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (pp (s ^ "│   ")) hd;
                list_pp s pp fmt tl

  let rec and_pp' s fmt = function
      And (m, h, n, []) -> Format.fprintf fmt "And %a, %a" mark_pp m N.pp n
    | And (m, h, n, l) ->
        Format.fprintf fmt "And %a %a %a" mark_pp m N.pp n (list_pp s or_pp') l

  and or_pp' s fmt = function
      Or (m, []) -> Format.fprintf fmt "Or %a" mark_pp m
    | Or (m, l) ->
        Format.fprintf fmt "Or %a %a" mark_pp m (list_pp s and_pp') l

  let and_pp = and_pp' ""
  let or_pp = or_pp' ""

  let rec tree_boring_pp fmt (And (m, h, n, l)) =
    Format.fprintf fmt "And %a %a %a" mark_pp m N.pp n
    (List.pp ~pp_sep:(str_pp "; ")
             ~pp_start:(str_pp "[")
             ~pp_stop:(str_pp "]")
             or_boring_pp) l

  and or_boring_pp fmt (Or (m, l)) =
    Format.fprintf fmt "Or %a %a" mark_pp m
    (List.pp ~pp_sep:(str_pp "; ")
             ~pp_start:(str_pp "[")
             ~pp_stop:(str_pp "]")
             tree_boring_pp) l

  let pp = or_pp

  let init nodes =
    let l = List.map (fun x -> And (Nil, H.empty, x, [])) nodes in
    Or (Nil, l)

  let sum = List.fold_left (+) 0

  (* Extracts the element from the given AND node *)
  let and_elt = function And (_, _, m, _) -> m

  (* Check if the element equals the element of the given node *)
  let eq_elt n = N.equal n % and_elt

  let is_leaf = function And (_, _, _, []) -> true | _ -> false

  (* Remove all the marks in the tree *)
  let unmark : or_node -> or_node =
    let rec aux_and (And (_, h, a, l)) = And (Nil, h, a, List.map aux_or l)
    and aux_or (Or (_, l)) = Or (Nil, List.map aux_and l)
    in aux_or

  let remove_previous_sol =
    let is_solved_leaf = function And (_, h, _, _) as n ->
        is_solved_and n && (is_leaf n || H.is_empty h) in
    let find_node (n, Or (_, l)) = List.find (eq_elt n) l in
    let chosen_nodes elts l = List.combine elts l |> List.map find_node in
    let rebuild_heap l h =
      let aux (c, elts) =
        chosen_nodes elts l
        |> List.map and_cost
        |> sum
        |> fun c' -> max c c', elts in
      H.to_seq h |> Seq.map aux |> H.of_seq in
    let rec aux_or = function
        Or (Nil, _) as n -> n
      | Or (Solved, l) -> Or (Nil, List.map aux_and l)
    and aux_and = function
        And (Nil, _, _, _) as n -> n
      | And (Solved, h, a, l) ->
          (* the min path is part of the discarded solution *)
          match H.find_min h with
            None -> And (Nil, h, a, l)
          | Some (_, elts) ->
              let choseAndNodes = chosen_nodes elts l in
              let l' = List.map aux_or l in
              if List.for_all is_solved_leaf choseAndNodes
              then And (Nil, rebuild_heap l' @@ H.delete_min h, a, l')
              else And (Nil, rebuild_heap l' h, a, l')
    in aux_or

  let sum_desc_costs = List.fold_left (fun acc n -> or_cost n + acc + 1) 0

  (* While loop *)
  let rec while_do pred f x =
    if pred x then x
    else while_do pred f @@ f x

  (* Sort a list of nodes with the provided cost function *)
  let sort_branches f =
    List.map (Pair.dup_map f)
    %> List.sort (fun (_, a) (_, b) -> Int.compare a b)
    %> List.map fst

  (* Helper function for extract *)
  let rec tree_conv f =
    let rec aux_and = function
        And (_, _, n, []) -> Tree.Node (n, [])
      | And (_, _, n, l) -> Tree.Node (n, List.map aux_or l)
    and aux_or = function
        Or (_, []) -> raise InvalidTree
      | Or (_, [a]) -> aux_and a
      | Or (_, l) -> aux_and @@ f l
    in aux_or

  (* Extract the solution from the internal representation *)
  let extract : or_node -> elt Tree.t = tree_conv (List.find is_solved_and)

  (* Enumerate possible combinations of descendant AND nodes (only to the next
     level of AND nodes and not beyond) *)
  let enumerate_paths pred hashmap = 
    List.cartesian_product
    %> List.map List.split
    %> List.filter pred
    %> List.map (function cs, ns ->
         let c = (List.fold_left (fun a c -> a + c + 1) 0 cs) in
         max c @@ HashMap.find hashmap ns, ns)
    %> List.fold_left H.insert H.empty

  let try_solve : t -> t =
    let rec aux_and tree =
      match tree with
      (* Don't continue solving if the tree is marked as solved *)
        And (Solved, _, _, _) -> raise InvalidTree
      (* If an And node has no computed descendants, expand to its descendants. *)
      | And (m, _, n, []) ->
          let l = N.successors n in
          let desc =
            List.mapi (fun i l ->
              let orNode = List.map (fun (c, n) -> And (Nil, H.empty, n, [])) l in
              Or (Nil, orNode))
            l in
          let h =
            if List.is_empty l then H.empty
            else
              List.cartesian_product l
                |> List.map (List.split %> Pair.map_fst (List.fold_left (fun a c -> a + c + 1) 0))
                |> List.fold_left H.insert H.empty in
          let m' = if List.is_empty l then Solved else m in
          Some (And (m', h, n, desc))
      (* We need to call aux_and on every one of and And node's descendants. *)
      | And (m, h, n, l) ->
          let open Option in
          let* _, ns = H.find_min h in
          let desc = List.map (uncurry aux_or) (List.combine ns l) in
          (* compile new heap using the costs of the computed descendants. *)
          let h' =
            let tbl = HashMap.of_seq (H.to_seq h |> Seq.map Pair.swap) in
            List.map (function Or (_, l) ->
              List.map (fun n -> and_cost n, and_elt n) l) desc
              |> enumerate_paths (snd %> HashMap.mem tbl) tbl in
          let m' = if List.for_all is_solved_or desc then Solved else m in
          Some (And (m', h', n, desc))
    and aux_or chosenNode tree =
      match tree with
      (* Don't continue solving if the tree is marked as solved *)
      (* We do not allow Or nodes with no descendants *)
        Or (Solved, _) | Or (_, []) -> raise InvalidTree
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | Or (m, l) ->
          let rest = List.filter (not % eq_elt chosenNode) l in
          let andNode = aux_and @@ List.find (eq_elt chosenNode) l in
          let branches =
            (* if a descendant And node no longer has viable descendants, remove
               it from the tree *)
            match andNode with
              None -> rest
            | Some expanded -> sort_branches and_cost @@ expanded::rest in
          let pred = Option.(map is_solved_and andNode |> get_or ~default:false) in
          let m' = if pred then Solved else m in
          Or (m', branches)
    in
    while_do
      is_solved_or
      (fun t ->
        let Or (m, l) = unmark t in
        let l' = sort_branches and_cost l in
        match aux_and @@ List.hd l' with
          None -> Or (m, List.tl l')
        | Some n ->
            let m' = if is_solved_and n then Solved else m in
            Or (m', n::List.tl l'))

  let run =
    try_solve
      %> while_do
         (N.validate % extract)
         (fun t ->
           let t' = remove_previous_sol t in
           let print_heap = function And (m, h, n, _) ->
             Format.printf "%a %a\n%a\n\n" mark_pp m N.pp n
               (H.pp (ppp Int.pp (lpp N.pp))) h in
           let print_orNode = function Or (_, l) -> List.iter print_heap l in
           print_orNode t';
           Format.printf "Next t: %a\n" pp t';
           Format.print_flush ();
           try_solve @@ remove_previous_sol t)
      %> extract
end
