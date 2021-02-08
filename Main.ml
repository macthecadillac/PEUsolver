open Containers
open Fun

module A = AOStar.Make (struct
  module T = Tree
  open AOStar
  include Int

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
end)

let () =
  (* this will error out. The point is to have it print out all the generated
     solutions *)
  let solved = A.run @@ A.init 0 in
  let pp = Result.pp' (Tree.pp Int.pp) AOStar.error_pp in
  Format.printf "%a\n" pp solved
