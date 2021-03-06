open Containers
open Fun

module List = struct
  include List
  let min_by cmp = function
      [] -> raise (Invalid_argument "List.min_by")
    | hd::tl -> fold_left (fun a b -> if cmp a b > 0 then b else a) hd tl
end

module AStar = struct
  type t = (int * Grammar.t) list
  let rec compare a b =
    match a, b with
      [], [] -> 0 | [], _ -> -1 | _, [] -> 1
    | (i1, _)::t1, (i2, _)::t2 when i1 = i2 -> compare t1 t2
    | (i1, _)::_, (i2, _)::_ -> Int.compare i1 i2
end

module FAStar (E : Search.ENV) = struct
  type t = (int * Grammar.t) list
  let rec compare a b =
    (* This happens when s is a hole. We look for the available non-terminals
       that fills this hole and find the min cost *)
    let aux s =
      Grammar.Map.get s E.successorsMap
      |> Option.get_exn
      |> List.map (fun a -> Grammar.Map.get a E.pcfg |> Option.get_exn)
      |> List.min_by Float.compare in
    let term_cost s = Option.fold (fun _ _ -> aux s) 0. (Grammar.Map.get s E.pcfg) in
    let cost = List.map (term_cost % snd) %> List.fold_left (+.) 0. in
    Float.compare (cost b) (cost a)
end
