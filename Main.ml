open Containers
open Fun

module Test = struct
  open AOStar
  module T = Tree

  type t = int

  let testTree = T.Node (Or (Nil, 0), [
                   T.Node (And (Nil, 1), [
                     T.Node (Or (Nil, 3), [
                       T.Node (Or (Nil, 15), []);
                       T.Node (Or (Nil, 16), [])
                     ]);
                     T.Node (Or (Nil, 4), [
                       T.Node (Or (Nil, 17), []);
                       T.Node (Or (Nil, 18), [])
                     ])
                   ]);
                   T.Node (Or (Nil, 2), [
                     T.Node (Or (Nil, 5), [
                       T.Node (Or (Nil, 8), []);
                       T.Node (Or (Nil, 9), [])
                     ]);
                     T.Node (And (Nil, 6), [
                       T.Node (Or (Nil, 10), []);
                       T.Node (Or (Nil, 11), [])
                     ]);
                     T.Node (And (Nil, 7), [
                       T.Node (Or (Nil, 12), []);
                       T.Node (Or (Nil, 13), []);
                       T.Node (Or (Nil, 14), [])
                     ])
                   ])
                 ])

  let equal = Int.equal

  let rec remove_desc = function
      T.Node (Or (a, b), l) -> T.Node (Or (a, b), [])
    | T.Node (And (a, b), l) -> T.Node (And (a, b), [])

  let successors i =
    let rec aux i = function
        T.Node (Or (_, j), l) | T.Node (And (_, j), l) when i = j ->
          List.map (remove_desc % T.map (function
              Or (m, a) -> Or (m, (a, a))
            | And (m, a) -> And (m, (a, a)))) l
      | T.Node (Or _, l) | T.Node (And _, l) -> List.map (aux i) l |> List.concat
    in
    aux i testTree

  let est_cost = id

  let validate t =
    Format.printf "%a\n" (T.pp Int.pp) t;
    false

  let pp = Int.pp
end

module A = AOStar.Make (Test)

let pp =
  let pp_sep fmt () = Format.fprintf fmt ", " in
  let pp_start fmt () = Format.fprintf fmt "(" in
  let pp_stop fmt () = Format.fprintf fmt ")" in
  let pair_pp = Pair.pp ~pp_sep ~pp_start ~pp_stop Int.pp Int.pp in
  Tree.pp (AOStar.node_pp pair_pp)

let tree_list_pp =
  let pp_sep fmt () = Format.fprintf fmt ", " in
  let pp_start fmt () = Format.fprintf fmt "(" in
  let pp_stop fmt () = Format.fprintf fmt ")" in
  let pair_pp = Pair.pp ~pp_sep ~pp_start ~pp_stop Int.pp Int.pp in
  let pp_sep fmt () = Format.fprintf fmt ";\n\t" in
  let pp_start fmt () = Format.fprintf fmt "[" in
  let pp_stop fmt () = Format.fprintf fmt "]" in
  List.pp ~pp_sep ~pp_start ~pp_stop pair_pp

let () =
  (* this will error out. The point is to have it print out all the generated
     solutions *)
  let solved = A.run @@ A.init 0 in
  Format.printf "%a" (Tree.pp Int.pp) solved
  (* Format.printf "%a\n" (List.pp pp) (Test.successors 7); *)
