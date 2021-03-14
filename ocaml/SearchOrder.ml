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
        if Grammar.is_hole E.successorsMap s
        then 0.  (* holes have zero cost *)
        else
          match E.prob with
            `PCFG pcfg -> PCFG.rule_cost pcfg s
          | `PHOG phog ->  (* FIXME: not sure if this is correct *)
              let ast =
                match List.rev l with
                  [] -> raise (Invalid_argument "Nill path")
                | (_, n)::tl ->
                    List.fold_left
                    (fun acc (_, r) -> Tree.Node (r, [acc]))
                    (Tree.Node (n, []))
                    tl in
              let loc = List.replicate (List.length l - 1) TCOND.(M DownFirst) in
              let _, context = TCOND.apply loc ast E.tcond_program in
              PHOG.ast_cost phog context ast in
      List.map (f % snd) l |> List.fold_left (+.) 0. in
    Float.compare (cost b) (cost a)
end
