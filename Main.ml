open Containers
open Fun

module T = struct
  open AOStar

  exception ImpossibleBranch

  type nonterm = Plus | Mul | Pow
  type term = One | Two | Three | Five
  type t = N of nonterm | T of term

  let equal = curry (function
      N Plus, N Plus | N Mul, N Mul | N Pow, N Pow
    | T One, T One | T Two, T Two | T Three, T Three | T Five, T Five -> true
    | _ -> false)

  let successors = function
      N Plus -> [1, [(1, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Plus); (20, N Mul); (20, N Pow)];
                 1, [(1, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Plus); (20, N Mul); (20, N Pow)]]
    | N Mul | N Pow ->
        [1, [(1000000, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Plus); (20, N Mul); (20, N Pow)];
         1, [(1000000, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Plus); (20, N Mul); (20, N Pow)]]
    | T One | T Two | T Three | T Five -> []

  let est_cost = function
      N Plus | N Mul | N Pow -> 20
    | T One | T Two | T Three | T Five -> 1

  let rec eval = function
      Tree.Node (N Plus, [a; b]) -> (eval a) + (eval b)
    | Tree.Node (N Mul, [a; b]) -> (eval a) * (eval b)
    | Tree.Node (N Pow, [a; b]) -> (try Int.pow (eval a) (eval b) with Invalid_argument _ -> 200)
    | Tree.Node (T One, []) -> 1
    | Tree.Node (T Two, []) -> 2
    | Tree.Node (T Three, []) -> 3
    | Tree.Node (T Five, []) -> 5
    | _ -> raise ImpossibleBranch

  let pp fmt = function
      N Plus -> Format.fprintf fmt "N Plus"
    | N Mul -> Format.fprintf fmt "N Mul"
    | N Pow -> Format.fprintf fmt "N Pow"
    | T One -> Format.fprintf fmt "T One"
    | T Two -> Format.fprintf fmt "T Two"
    | T Three -> Format.fprintf fmt "T Three"
    | T Five -> Format.fprintf fmt "T Five"

  let validate t =
    Format.printf "%a\n\n" (Tree.pp pp) t;
    (* Format.printf "%a\n\nBREAK\n\n" (Tree.pp pp) t; *)
    Format.print_flush ();
    (* let _ = read_line () in *)
    eval t = 19
end

module M = AOStar.Make (T)

let () =
  let solved = M.run @@ M.init T.[20, N Plus; 20, N Mul; 20, N Pow] in
  let pp = Result.pp' (Tree.pp T.pp) AOStar.error_pp in
  Format.printf "%a\n" pp solved
