open Containers
open Fun

module AStar = struct
  type t = float * (int * Grammar.t) list
  let compare (_, a) (_, b) =
    let rec aux a b =
    match a, b with
      [], [] -> 0 | [], _ -> -1 | _, [] -> 1
    | (i1, _)::t1, (i2, _)::t2 when i1 = i2 -> aux t1 t2
    | (i1, _)::_, (i2, _)::_ -> Int.compare i1 i2 in
    aux a b
end

module FAStar (E : Search.ENV) = struct
  type t = float * (int * Grammar.t) list
  let rec compare (a, _) (b, _) = Float.compare a b
end
