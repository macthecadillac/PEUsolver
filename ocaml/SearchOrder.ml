open Containers
open Fun

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
    let cost l =
      let f s =
        if Grammar.is_hole E.succMap s then 0.  (* holes have zero cost *)
        else
          let ast =
            match List.rev l with
              [] -> raise (Invalid_argument "Nill path")
            | (_, n)::tl ->
                List.fold_left
                (fun acc (_, r) -> Tree.Node (r, [acc]))
                (Tree.Node (n, []))
                tl in
          E.ast_cost ast in
      List.map (f % snd) l |> List.fold_left (+.) 0. in
    let c = Float.compare (cost b) (cost a) in
    (* Format.printf "%i\t" c; *)
    c
end
