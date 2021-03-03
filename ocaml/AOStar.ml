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

  let and_elt = function And (_, _, n, _) -> n

  let list_pp' p =
    List.pp ~pp_sep:(C.str_pp "; ") ~pp_start:(C.str_pp "[") ~pp_stop:(C.str_pp "]") p

  module P = struct
    type or_node = Or of int * C.I.t * and_node option
    and and_node = And of int * N.t * or_node option
    type t = or_node
    let option_pp pp fmt = function
        Some a -> Format.fprintf fmt "Some %a" pp a
      | None -> Format.fprintf fmt "None"
    let rec or_pp fmt (Or (c, i, n)) =
      Format.fprintf fmt "Or %i %a %a" c C.I.pp i (option_pp and_pp) n
    and and_pp fmt (And (c, n, o)) =
      Format.fprintf fmt "And %i %a %a" c N.pp n (option_pp or_pp) o
    let rec compare_a a b =
      match a, b with
        And (c1, _, _), And (c2, _, _) when c1 <> c2 -> Int.compare c1 c2
      | And (c1, _, Some o1), And (c2, _, Some o2) -> compare o1 o2
      (* we prefer leaf nodes--also leaf nodes have a cost of 0 *)
      | And (_, _, None), And (_, _, Some _) -> -1
      | And (_, _, Some _), And (_, _, None) -> 1
      | And (_, _, None), And (_, _, None) -> 0
    and compare o1 o2 =
      match o1, o2 with
        Or (_, _, None), Or (_, _, None) -> 0
      | Or (_, _, Some _), Or (_, _, None) -> 1
      | Or (_, _, None), Or (_, _, Some _) -> -1
      | Or (c1, _, Some _), Or (c2, _, Some _) when c1 <> c2 -> Int.compare c1 c2
      | Or (c1, _, Some a1), Or (c2, _, Some a2) -> compare_a a1 a2
  end

  module M = PairingHeap.Make (P)

  (* a single sentential form is represented by a heap of paths *)
  module SF = struct
    type t = C.G.t * C.G.t * P.t list * M.t
    let find_min = M.find_min
    let singleton = M.singleton
    let delete_min = M.delete_min
    let insert = M.insert
    let to_list = M.to_list
    let of_list = M.of_list
    let is_empty = M.is_empty
    let compare (_, _, _, a) (_, _, _, b) =
      match M.find_min a, M.find_min b with
        None, None -> 0
      | None, Some _ -> -1  (* prefer possible terminal nodes *)
      | Some _, None -> 1
      | Some x, Some y -> P.compare x y
    let pp fmt (g1, g2, pl, m) =
      Format.fprintf fmt "%a, %a, %a, %a"
      C.G.pp g1 C.G.pp g2 (list_pp' P.or_pp) pl (M.pp' P.or_pp) m
  end

  (* An efficient heap for our priority queue of sentential forms *)
  module Q = PairingHeap.Make (SF)

  type elt = N.t
  type t = C.G.t * C.G.t * SF.t list * Q.t * or_node

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

  let rec and_boring_pp fmt (And (m, c, n, l)) =
    Format.fprintf fmt "And %a %i %a %a" mark_pp m c N.pp n (list_pp' or_boring_pp) l

  and or_boring_pp fmt (Or (m, c, l)) =
    let l' = if Lazy.is_forced l then Lazy.force l else [] in
    Format.fprintf fmt "Or %a %i %a" mark_pp m c (list_pp' and_boring_pp) l'

  let pp fmt (g1, g2, sf, q, t) =
    Format.fprintf fmt "Major-G: %a\nMinor-G: %a\nSF:\n%a\nHeap:\n%a\nTree:\n%a"
    C.G.pp g1
    C.G.pp g2
    (list_pp' SF.pp) sf
    (Q.pp SF.pp) q
    or_pp t

  let pp_ = pp

  (* initialize computation *)
  let init l =
    let open List in
    let costs, nodes = map (fun (c, n) -> c, And (Nil, c, n, [])) l |> split in
    let cost = min costs + 1 in
    let t = Or (Nil, cost, Lazy.pure nodes) in
    let paths =
      let+ n = nodes in
      let andNode = P.And (and_cost n, and_elt n, None) in
      C.G.zero, C.G.zero, [], SF.singleton @@ P.Or (cost, C.I.of_int 0, Some andNode) in
    let h = List.fold_left Q.insert Q.empty paths in
    C.G.zero, C.G.zero, [], h, t

  (* Extract the element of an And node *)
  let and_elt (And (_, _, a, _)) = a

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
    in Tree.unfold aux_and % aux_or % (function _, _, _, _, a -> a)

  let update_costs ps orNode =
    Format.printf "UPDATE:\nPath: %a\nNode:\n%a\n\n" (list_pp' P.or_pp) ps or_pp orNode;
    let rec aux_or sf orNode =
      match sf, orNode with
        P.Or (_, i, None), Or (_, c, _) -> P.Or (c, i, None)
      | P.Or (_, i, Some (P.And (_, n, _) as a)), Or (_, c, lazyl) ->
          let l = Lazy.force lazyl in
          let eq = List.find (N.equal n % and_elt) l in
          P.Or (c, i, Some (aux_and a eq))
    and aux_and sf andNode =
      match sf, andNode with
        P.And (_, n, None), And (_, c, _, _) -> P.And (c, n, None)
      | P.And (_, n, Some (P.Or (_, i, _) as o)), And (_, c, _, l) ->
          let eq = List.nth l (C.I.to_int i) in
          P.And (c, n, Some (aux_or o eq))
    in List.map (fun sf -> aux_or sf orNode) ps

  (* Expand the branches and try to find a solution *)
  let try_solve (gMajor, gMinor, s, q, t) =
    let rec aux_and chosenPath tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        p, (And (Solved, _, _, _) as a) -> `Skip, [[p]], a
      (* If an And node has no computed descendants, expand to its descendants. *)
      | P.And (_, _, None), And (m, c, n, l) ->
          let open List in
          let s = N.successors n in
          let desc =
            let+ cost, l' = s in
            let orNode () = map (fun (c, n) -> And (Nil, c, n, [])) l' in
            Or (Nil, cost, Lazy.of_thunk orNode) in
          let c' = sum_desc_costs desc in
          let l' = if is_empty l then desc else l in
          let flag = if not (is_empty l) then `Intact
                     else if is_empty l && is_empty desc
                     then `Term else `Changed in
          let m' = if is_empty l' then Solved else m in
          let paths =
            let+ i, (cost, _) = mapi (fun i n -> C.I.of_int i, n) s in
            P.And (c', n, Some (P.Or (cost, i, None))) in
          flag, [paths], And (m', c', n, l')
      (* We need to call aux_or on every one of And node's descendants. *)
      | P.And (pc, _, Some pl), And (m, c, n, l) ->
          let rec target_map_or (j, l) (P.Or (_, i, _) as p) = function
              [] -> `Intact, [], List.rev l
            | hd::tl when C.I.(of_int j <> i) -> target_map_or (j + 1, hd::l) p tl
            | hd::tl -> let flag, ps, hd' = aux_or p hd in
                        flag, ps, List.rev_append l (hd'::tl) in
          let flag, ps, desc = target_map_or (0, []) pl l in
          let c' = sum_desc_costs desc in
          let m' = if List.for_all is_solved_or desc then Solved else m in
          let paths =
            let open List in
            let+ branches = ps in
            let+ orNode = branches in
            P.And (c', n, Some orNode) in
          flag, paths, And (m', c', n, desc)
    and aux_or chosenPath tree =
      match chosenPath, tree with
      (* Don't continue solving if the tree is marked as solved *)
        p, (Or (Solved, _, _) as a) -> `Skip, [[p]], a
      (* If it is an Or node with computed descendants, find the child that
         matches the current node on the supplied path and expand. *)
      | P.Or (_, i, Some (And (_, pn, _) as p)), Or (m, _, lazyl) ->
          let rec target_map_and l = function
              [] -> `Intact, [], List.rev l
            | (And (_, _, n, _) as hd)::tl when N.equal pn n ->
                let flag, ps, hd' = aux_and p hd in
                flag, ps, List.rev_append l (hd'::tl)
            | hd::tl -> target_map_and (hd::l) tl in
          let l = Lazy.force lazyl in
          let flag, ps, branches = target_map_and [] l in
          let c = 1 + (List.map and_cost branches |> List.min) in
          let m' = if List.exists is_solved_and branches then Solved else m in
          let paths =
            let open List in
            let+ p = ps in
            let+ andNode = p in
            P.Or (c, i, Some andNode) in
          flag, paths, Or (m', c, Lazy.pure branches)
      | P.Or (_, i, None), Or (m, c, lazyl) ->
          let flag = if Lazy.is_forced lazyl then `Intact else `Changed in
          let l = Lazy.force lazyl in
          let c' = 1 + (List.map and_cost l |> List.min) in
          let paths =
            let open List in
            let+ n = l in
            [P.Or (c', i, Some (P.And (and_cost n, and_elt n, None)))] in
          flag, paths, Or (m, c', Lazy.pure l)
    in
    C.while_do_result
       (is_solved_or % function _, _, _, _, t -> t)
       (function gMajor, gMinor, s, q, t ->
         let open Result in
         let _ = Format.printf "\n\n####################\nNew Iter:\nCurrent State:\n%a\n\n"
           pp_ (gMajor, gMinor, s, q, t) in
         (* let _ = read_line () in *)
         let inc_if g = function `Term | `Intact | `Skip -> g | `Changed -> C.G.inc g in
         (* ps is a list of terminal paths that have already been marked  *)
         let* gMajor', gMinor', ps, sf = Q.find_min q |> of_opt in
         let ps', sf' =
           if C.G.(gMajor' < gMajor)
           (* maybe update costs here as well *)
           then
             let _ = Printf.printf "Insert paths\n" in
             let _ = Format.print_flush () in
             [], List.fold_left SF.insert sf ps
           else ps, sf in
         if C.G.(gMinor' < gMinor) then
           let _ = Printf.printf "Update costs\n" in
           let _ = Format.print_flush () in
           let sf'' = update_costs (SF.to_list sf') t |> SF.of_list in
           return (gMajor, gMinor, s, Q.insert (Q.delete_min q) (gMajor, gMinor, ps', sf''), t)
         else
           let+ p = SF.find_min sf' |> of_opt in
           let _ = Format.printf "Chosen path: %a\n" P.or_pp p in
           let flag, paths, t' = aux_or p t in
           let sf'' = SF.delete_min sf' in
           match flag with
             `Term when SF.is_empty sf'' ->
               (* the sf is the solution *)
               let _ = Format.printf "Term: SF EMPTY!\n" in
               gMajor, gMinor, s, Q.delete_min q, t'
           | `Skip when SF.is_empty sf'' ->
               let _ = Format.printf "Skip: SF EMPTY!\n" in
               gMajor, gMinor, (gMajor, gMinor, p::ps', sf'')::s, Q.delete_min q, t'
           | `Term | `Skip ->
               let q'' = Q.insert (Q.delete_min q) (gMajor, gMinor, p::ps', sf'') in
               gMajor, gMinor, s, q'', t'
           | _ ->
               let _ = Format.printf "New paths:\n" in
               let sf's =
                 let open List in
                 let+ path = paths in
                 let _ = List.iter (fun p -> Format.printf "%a\n" P.or_pp p) path in
                 gMajor, inc_if gMinor flag, ps', List.fold_left SF.insert sf'' path in
               let _ = Format.printf "New SF's:\n" in
               let _ = List.iter (fun sf -> Format.printf "%a\n" SF.pp sf) sf's in
               let _ = print_newline () in
               let q' = List.fold_left Q.insert (Q.delete_min q) sf's in
               gMajor, inc_if gMinor flag, s, q', t')
       (Ok (gMajor, gMinor, s, q, t))
    |> Result.map_err (const C.SolutionNotFound)

  (* Repeatedly calls try_solve until a solution validates *)
  let run =
    try_solve
    %> C.while_do_result
       (fun ((_, _, _, q, t) as s) ->
         Format.printf "Validating:\n%a\n\n" or_pp t;
         N.validate @@ extract s)
       (function gMajor, gMinor, s, q, t ->
         (* let _ = read_line () in *)
         let _ = Format.printf "\n\n#####################\nValidation Failed.\nPrevious State:\n%a\n\n"
           pp_ (gMajor, gMinor, s, q, t) in
         let q' = List.fold_left Q.insert q s in
         try_solve (C.G.inc gMajor, gMinor, [], q', unmark_or t))
    %> Result.map extract
end
