(** Or modified AO* Algorithm that continues solving until it finds as solution
    that validates *)
open Containers
open Fun

module List = struct
  include List
  let min = function
      [] -> 0
    | hd::tl -> List.fold_left min hd tl
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

(* A bunch of different printers for debugging *)
let str_pp s fmt () = Format.fprintf fmt s

module Make (N : I) : S with type elt = N.t = struct

  exception CorruptState
  exception InvalidTree

  module rec HeapElt : sig
    type t = int * (AndOr.node list)
    val compare : t -> t -> int
  end = struct
    type t = int * (AndOr.node list)
    let compare a b = Int.compare (fst a) (fst b)
  end

  and H : PairingHeap.S with type elt = HeapElt.t = PairingHeap.Make (HeapElt)

  (* The And-Or tree type *)
  and AndOr : sig
    type t = H.t
    type node = Node of mark * N.t * t
  end = struct
    type t = H.t
    type node = Node of mark * N.t * t
  end

  type elt = N.t
  type t = AndOr.node

  open AndOr

  let desc (Node (_, n, h)) = H.find_min h |> Option.map snd

  let cost (Node (_, n, h)) =
    match H.find_min h with
      None -> N.est_cost n
    | Some m -> fst m

  let is_solved = mark_eq Solved % function Node (m, _, _) -> m

  let is_leaf (Node (_, _, h)) = H.is_empty h

  (* Extracts the element from the given AND node *)
  let node_elt (Node (_, n, _)) = n

  let replace_desc (Node (m, n, h)) ns =
    let m' = if List.for_all is_solved ns then Solved else m in
    let cost = List.map cost ns |> List.sum in
    let h' = H.delete_min h |> flip H.insert (cost, ns) in
    Node (m', n, h')

  let init =
    List.fold_left (fun acc elt ->
      (* An AND node with one descendant is the same as an or node with a
         chosen branch *)
      H.insert acc (N.est_cost elt, [Node (Nil, elt, H.empty)]))
    H.empty

  (* Check if the element equals the element of the given node *)
  let eq_elt n = N.equal n % node_elt

  (* Remove all the marks in the tree *)
  let unmark =
    let rec unmark_h h =
      H.to_seq h
      |> Seq.map (fun (c, l) -> c, List.map aux l)
      |> H.of_seq
    and aux (Node (_, n, h)) = Node (Nil, n, unmark_h h)
    in unmark_h

  let extract h =
    let open Option in
    let rec aux (Node (_, n, h)) =
      let* _, nodes = H.find_min h in
      let* l = List.map aux nodes |> Option.sequence_l in
      return @@ Tree.Node (n, l) in
    let* _, firstNode = H.find_min h in
    let* n = List.head_opt firstNode in
    aux n

  let is_solved_leaf n = is_solved n && is_leaf n

  let rec remove_previous_sol = function
      Node (Nil, _, _) as n -> n
    | Node (Solved, n, h) ->
        match H.find_min h with
          None -> Node (Nil, n, h)
        | Some (_, ns) ->
            if List.for_all is_solved_leaf ns
            then Node (Nil, n, H.delete_min h)
            else
              let h' =
                H.to_seq h
                |> Seq.map (fun (c, l) -> c, List.map remove_previous_sol l)
                |> H.of_seq in
              Node (Nil, n, h')

  (* let sum_desc_costs = List.fold_left (fun acc n -> or_cost n + acc + 1) 0 *)

  (* While loop *)
  let rec while_do pred f x =
    if pred x then x
    else while_do pred f @@ f x

  (* Enumerate possible combinations of descendant AND nodes (only to the next
     level of AND nodes and not beyond) *)
  let enumerate_paths =
    List.cartesian_product
    %> List.map List.split
    %> List.map (function cs, ns ->
         let c = (List.fold_left (fun a c -> a + c + 1) 0 cs) in
         c, ns)
    %> List.fold_left H.insert H.empty

  let try_solve : t -> t =
    (* let rec aux_and tree = *)
    (*   match tree with *)
    (*   (1* Don't continue solving if the tree is marked as solved *1) *)
    (*     And (Solved, _, _, _) -> raise InvalidTree *)
    (*   (1* If an And node has no computed descendants, expand to its descendants. *1) *)
    (*   | And (m, _, n, []) -> *)
    (*       let l = N.successors n in *)
    (*       let desc = *)
    (*         List.mapi (fun i l -> *)
    (*           let orNode = List.map (fun (c, n) -> And (Nil, H.empty, n, [])) l in *)
    (*           Or (Nil, orNode)) *)
    (*         l in *)
    (*       let h = *)
    (*         if List.is_empty l then H.empty *)
    (*         else *)
    (*           List.cartesian_product l *)
    (*             |> List.map (List.split %> Pair.map_fst (List.fold_left (fun a c -> a + c + 1) 0)) *)
    (*             |> List.fold_left H.insert H.empty in *)
    (*       let m' = if List.is_empty l then Solved else m in *)
    (*       Some (And (m', h, n, desc)) *)
    (*   (1* We need to call aux_and on every one of and And node's descendants. *1) *)
    (*   | And (m, h, n, l) -> *)
    (*       let open Option in *)
    (*       let* _, ns = H.find_min h in *)
    (*       let desc = List.map (uncurry aux_or) (List.combine ns l) in *)
    (*       (1* compile new heap using the costs of the computed descendants. *1) *)
    (*       let h' = *)
    (*         let tbl = HashMap.of_seq (H.to_seq h |> Seq.map Pair.swap) in *)
    (*         List.map (function Or (_, l) -> *)
    (*           List.map (fun n -> and_cost n, and_elt n) l) desc *)
    (*           |> enumerate_paths (snd %> HashMap.mem tbl) tbl in *)
    (*       let m' = if List.for_all is_solved_or desc then Solved else m in *)
    (*       Some (And (m', h', n, desc)) *)
    (* and aux_or chosenNode tree = *)
    (*   match tree with *)
    (*   (1* Don't continue solving if the tree is marked as solved *1) *)
    (*   (1* We do not allow Or nodes with no descendants *1) *)
    (*     Or (Solved, _) | Or (_, []) -> raise InvalidTree *)
    (*   (1* If it is an Or node with computed descendants, find the child that *)
    (*      matches the current node on the supplied path and expand. *1) *)
    (*   | Or (m, l) -> *)
    (*       let rest = List.filter (not % eq_elt chosenNode) l in *)
    (*       let andNode = aux_and @@ List.find (eq_elt chosenNode) l in *)
    (*       let branches = *)
    (*         (1* if a descendant And node no longer has viable descendants, remove *)
    (*            it from the tree *1) *)
    (*         match andNode with *)
    (*           None -> rest *)
    (*         | Some expanded -> sort_branches and_cost @@ expanded::rest in *)
    (*       let pred = Option.(map is_solved_and andNode |> get_or ~default:false) in *)
    (*       let m' = if pred then Solved else m in *)
    (*       Or (m', branches) *)
    (* in *)
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

  (* let run = *)
  (*   try_solve *)
  (*     %> while_do *)
  (*        (N.validate % extract) *)
  (*        (fun t -> *)
  (*          (1* let t' = remove_previous_sol t in *1) *)
  (*          (1* let print_heap = function And (m, h, n, _) -> *1) *)
  (*          (1*   Format.printf "%a %a\n%a\n\n" mark_pp m N.pp n *1) *)
  (*          (1*     (H.pp (ppp Int.pp (lpp N.pp))) h in *1) *)
  (*          (1* let print_orNode = function Or (_, l) -> List.iter print_heap l in *1) *)
  (*          (1* print_orNode t'; *1) *)
  (*          (1* Format.printf "Next t: %a\n" pp t'; *1) *)
  (*          (1* Format.print_flush (); *1) *)
  (*          try_solve @@ remove_previous_sol t) *)
  (*     %> extract *)
end
