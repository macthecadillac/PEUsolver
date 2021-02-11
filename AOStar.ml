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
  val init : elt -> t
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

  type tree = And of mark * H.t * N.t * or_node list
  and or_node = Or of mark * tree list

  let pick_min = function
      [] -> 0
    | hd::tl -> List.fold_left min hd tl

  let and_cost = function And (_, h, n, _) ->
    match H.find_min h with
      None -> N.est_cost n
    | Some m -> fst m

  let or_cost = function Or (_, l) -> pick_min @@ List.map and_cost l

  type elt = N.t
  type t = tree

  module HashMap = Hashtbl.Make (struct
    type t = N.t list
    let hash = Hash.poly
    let equal a b = hash a = hash b
  end)

  let is_solved_and = mark_eq Solved % function And (m, _, _, _) -> m
  let is_solved_or = mark_eq Solved % function Or (m, _) -> m

  (* A bunch of different printers for debugging *)
  let lpp = List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
                    ~pp_start:(fun f () -> Format.fprintf f "[")
                    ~pp_stop:(fun f () -> Format.fprintf f "]")
  let lpp' = List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
                    ~pp_start:(fun f () -> Format.fprintf f "[")
                    ~pp_stop:(fun f () -> Format.fprintf f "]")
  let lpp'' = List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
                    ~pp_start:(fun f () -> Format.fprintf f "[")
                    ~pp_stop:(fun f () -> Format.fprintf f "]")
  let lpp''' = List.pp ~pp_sep:(fun f () -> Format.fprintf f ";\n")
                    ~pp_start:(fun f () -> Format.fprintf f "\n")
                    ~pp_stop:(fun f () -> Format.fprintf f "\n")
  let ppp = Pair.pp ~pp_sep:(fun f () -> Format.fprintf f ", ")
                    ~pp_start:(fun f () -> Format.fprintf f "(")
                    ~pp_stop:(fun f () -> Format.fprintf f ")")
  let ppp' = Pair.pp ~pp_sep:(fun f () -> Format.fprintf f ", ")
                    ~pp_start:(fun f () -> Format.fprintf f "(")
                    ~pp_stop:(fun f () -> Format.fprintf f ")")

  let rec list_pp s pp fmt = function
      [] -> ()
    | [a] -> Format.fprintf fmt "\n%s└── %a" s (pp (s ^ "    ")) a
    | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (pp (s ^ "│   ")) hd;
                list_pp s pp fmt tl

  let rec tree_pp' s fmt = function
      And (m, h, n, []) -> Format.fprintf fmt "And %a, %a" mark_pp m N.pp n
    | And (m, h, n, l) ->
        Format.fprintf fmt "And %a %a %a" mark_pp m N.pp n (list_pp s or_pp') l

  and or_pp' s fmt = function
      Or (m, []) -> Format.fprintf fmt "Or %a" mark_pp m
    | Or (m, l) ->
        Format.fprintf fmt "Or %a %a" mark_pp m (list_pp s tree_pp') l

  let tree_pp = tree_pp' ""
  let or_pp = or_pp' ""

  let rec tree_boring_pp fmt (And (m, h, n, l)) =
    Format.fprintf fmt "And %a %a %a" mark_pp m N.pp n
    (List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
             ~pp_start:(fun f () -> Format.fprintf f "[")
             ~pp_stop:(fun f () -> Format.fprintf f "]")
             or_boring_pp) l

  and or_boring_pp fmt (Or (m, l)) =
    Format.fprintf fmt "Or %a %a" mark_pp m
    (List.pp ~pp_sep:(fun f () -> Format.fprintf f "; ")
             ~pp_start:(fun f () -> Format.fprintf f "[")
             ~pp_stop:(fun f () -> Format.fprintf f "]")
             tree_boring_pp) l

  let pp = tree_pp

  let init node = And (Nil, H.empty, node, [])

  let sum_cost = List.fold_left (+) 0

  let and_elt = function And (_, _, m, _) -> m

  let eq_elt n = N.equal n % and_elt

  let is_leaf = function And (_, _, _, []) -> true | _ -> false

  let rebuild_heaps l h =
    H.to_seq h
    |> Seq.map (function c, ns ->
        List.combine ns l
        |> List.map (function n, Or (_, l) ->
                      List.find (N.equal n % and_elt) l |> and_cost)
        |> sum_cost
        |> fun c' -> max c c', ns)
    |> H.of_seq

  let unmark =
    let rec aux_and (And (_, h, a, l) as x) =
      match H.find_min h with
        None -> x
      | Some (_, elts) ->
          let ns =
            List.combine elts l
            |> List.map (function n, Or (_, l) ->
                List.find (N.equal n % and_elt) l) in
          let l' = List.map aux_or l in
          if List.for_all (function And (_, h', _, _) as n ->
                            is_solved_and n && (is_leaf n || H.is_empty h')) ns
          then And (Nil, rebuild_heaps l' @@ H.delete_min h, a, l')
          else And (Nil, rebuild_heaps l' h, a, l')
    (* should not map but only rebuild where necessary *)
    and aux_or (Or (_, l)) = Or (Nil, List.map aux_and l)
    in aux_and

  let sum_desc_costs = List.fold_left (fun acc n -> or_cost n + acc + 1) 0

  let rec while_do pred f x =
    if pred x then x
    else while_do pred f @@ f x

  let sort_branches f =
    List.map (Pair.dup_map f)
    %> List.sort (fun (_, a) (_, b) -> Int.compare a b)
    %> List.map fst

  let rec tree_conv f =
    let rec aux_and = function
        And (_, _, n, []) -> Tree.Node (n, [])
      | And (_, _, n, l) -> Tree.Node (n, List.map aux_or l)
    and aux_or = function
        Or (_, []) -> raise InvalidTree
      | Or (_, [a]) -> aux_and a
      | Or (_, l) -> aux_and @@ f l
    in aux_and

  let to_tree = tree_conv (fun _ -> raise CorruptState)

  let extract = tree_conv (List.find is_solved_and)

  let enumerate_paths pred hashmap = 
    List.cartesian_product
    (* %> List.map (fun l -> Format.printf "%a\n" (lpp (ppp Int.pp N.pp)) l; l) *)
    %> List.map List.split
    %> List.filter pred
    %> List.map (function cs, ns ->
         let c = (List.fold_left (fun a c -> a + c + 1) 0 cs) in
         max c @@ HashMap.find hashmap ns, ns)
    %> List.fold_left H.insert H.empty

  let try_solve =
    let rec aux_and tree =
      match tree with
      (* Don't continue solving if the tree is marked as solved *)
        (And (Solved, _, _, _) as a) -> Some a
      (* If an And node has no computed descendants, expand to its descendants. *)
      | And (m, _, n, []) ->
          (* Format.printf "And []\n"; *)
          let l = N.successors n in
          let desc =
            List.mapi (fun i l ->
              let orNode = List.map (fun (c, n) -> And (Nil, H.empty, n, [])) l in
              Or (Nil, orNode))
            l in
          let h =
            List.cartesian_product l
              (* |> List.map (fun l -> Format.printf "%a\n" (lpp (ppp Int.pp N.pp)) l; l) *)
              |> List.map (List.split %> Pair.map_fst (List.fold_left (fun a c -> a + c + 1) 0))
              |> List.fold_left H.insert H.empty in
          let m' = if List.is_empty l then Solved else m in
          (* Format.printf "NODE: %a\n" N.pp n; *)
          (* Format.printf "SUCC: \n%a\n" (lpp (lpp' (ppp Int.pp N.pp))) l; *)
          (* Format.printf "HEAP: \n%a\n\n\n" (H.pp (ppp' Int.pp (lpp'' N.pp))) h; *)
          (* Format.printf "RETURN: \n%a\n\n\n" tree_pp @@ And (m', c, h, n, desc); *)
          (* Format.print_flush (); *)
          (* read_line (); *)
          Some (And (m', h, n, desc))
      (* We need to call aux_and on every one of and And node's descendants. *)
      | And (m, h, n, l) ->
          (* Format.printf "And l\n"; *)
          (* Format.printf "NODE: %a\n" N.pp n; *)
          (* Format.printf "ORIG HEAP: \n%a\n\n\n" (H.pp (ppp' Int.pp (lpp'' N.pp))) h; *)
          (* read_line (); *)
          let open Option in
          (* if H.find_min h |> Option.is_none then Format.printf "HEAP EMPTY!!!\n\n\n\n"; *)
          let* _, ns = H.find_min h in
          let desc = List.map (uncurry aux_or) (List.combine ns l) in
          (* compile new heap using the costs of the computed descendants. *)
          (* Format.printf "DESC: %a\n\n" (lpp' or_pp) desc; *)
          let h' =
            let tbl = HashMap.of_seq (H.to_seq h |> Seq.map Pair.swap) in
            List.map (function Or (_, l) ->
              List.map (fun n -> and_cost n, and_elt n) l) desc
            |> enumerate_paths (snd %> HashMap.mem tbl) tbl in
          let m' = if List.for_all is_solved_or desc then Solved else m in
          (* Format.printf "FINAL HEAP: \n%a\n\n\n" (H.pp (ppp' Int.pp (lpp'' N.pp))) h'; *)
          (* Format.printf "RETURN: \n%a\n\n\n" tree_pp @@ And (m', c, h', n, desc); *)
          (* read_line (); *)
          Some (And (m', h', n, desc))
    and aux_or chosenNode tree =
      match tree with
      (* Don't continue solving if the tree is marked as solved *)
        (Or (Solved, _) as a) -> a
      (* We do not allow Or nodes with no descendants *)
      | Or (m, []) -> raise InvalidTree
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | Or (m, l) ->
          (* Format.printf "Or l\n"; *)
          (* Format.printf "SUCC: \n%a\n" (lpp''' tree_pp) l; *)
          (* Format.printf "CHOSEN NODE: %a\n\n\n" N.pp chosenNode; *)
          Format.print_flush ();
          (* read_line (); *)
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
          (* Format.printf "RETURN: \n%a\n\n\n" or_pp @@ Or (m', c, branches); *)
          (* read_line (); *)
          Or (m', branches)
    in
    while_do is_solved_and (fun t ->
                              (* Format.printf "NEW ITER:\n%a\n\n" pp t; *)
                              (* Format.print_flush (); *)
                              (* read_line (); *)
                              aux_and t |> Option.get_exn)

  let run =
    try_solve
      %> while_do
         (N.validate % extract)
         (try_solve % unmark)
      %> extract
end
