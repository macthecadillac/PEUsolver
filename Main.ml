open Containers
open Fun

module T = struct
  open AOStar

  exception ImpossibleBranch

  type nonterm = Plus | Mul
  type term = One | Two
  type t = N of nonterm | T of term

  let equal = curry (function
      N Plus, N Plus | N Mul, N Mul -> true
    | T One, T One | T Two, T Two -> true
    | _ -> false)

  let possible_terms = [(1, T One); (1, T Two); (2, N Plus); (2, N Mul)]

  let initItems = [(1, T One); (1, T Two); (2, N Plus); (2, N Mul)]

  let successors = function
      N Plus -> [[(1, T One); (1, T Two); (2, N Plus); (2, N Mul)];
                 [(1, T One); (1, T Two); (2, N Plus); (2, N Mul)]]
    | N Mul -> [[(1000000, T One); (1, T Two); (2, N Plus); (2, N Mul)];
                [(1000000, T One); (1, T Two); (2, N Plus); (2, N Mul)]]
    | T One | T Two -> []

  let rec eval = function
      Tree.Node (N Plus, [a; b]) -> (eval a) + (eval b)
    | Tree.Node (N Mul, [a; b]) -> (eval a) * (eval b)
    | Tree.Node (T One, []) -> 1
    | Tree.Node (T Two, []) -> 2
    | _ -> raise ImpossibleBranch

  let pp fmt = function
      N Plus -> Format.fprintf fmt "N Plus"
    | N Mul -> Format.fprintf fmt "N Mul"
    | T One -> Format.fprintf fmt "T One"
    | T Two -> Format.fprintf fmt "T Two"

  let validate t =
    Format.printf "%a\n\n" (Tree.pp pp) t;
    (* Format.printf "%a\n\nBREAK\n\n" (Tree.pp pp) t; *)
    Format.print_flush ();
    (* let _ = read_line () in *)
    eval t = 10
end

module M = AOStar.Make (T)

let () =
  let solved = M.run @@ M.init T.initItems in
  let pp = Result.pp' (Tree.pp T.pp) AOStar.error_pp in
  Format.printf "%a\n" pp solved
