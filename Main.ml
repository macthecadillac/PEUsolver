open Containers
open Fun

module T = struct
  open AOStar

  exception ImpossibleBranch

  type nonterm = Add | Mul | Pow
  type term = One | Two | Three | Five
  type t = N of nonterm | T of term

  let equal = curry (function
      N Add, N Add | N Mul, N Mul | N Pow, N Pow -> true
    | T One, T One | T Two, T Two | T Three, T Three | T Five, T Five -> true
    | _ -> false)

  let successors = function
      N Add -> [[(2, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Add); (20, N Mul); (20, N Pow)];
                [(2, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Add); (20, N Mul); (20, N Pow)]]
    | N Mul -> [[(1000000, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Add); (20, N Mul); (20, N Pow)];
                [(1000000, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Add); (20, N Mul); (20, N Pow)]]
    | N Pow -> [[(1000000, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Add); (20, N Mul); (20, N Pow)];
                [(1000000, T One); (1, T Two); (1, T Three); (1, T Five); (20, N Add); (20, N Mul); (20, N Pow)]]
    | T One | T Two | T Three | T Five -> []

  let est_cost = function
      N Add | N Mul | N Pow -> 20
    | T One | T Two | T Three | T Five -> 1

  let rec eval = function
      Tree.Node (N Add, [a; b]) -> (eval a) + (eval b)
    | Tree.Node (N Mul, [a; b]) -> (eval a) * (eval b)
    | Tree.Node (N Pow, [a; b]) -> (try Int.pow (eval a) (eval b) with Invalid_argument _ -> 200)
    | Tree.Node (T One, []) -> 1
    | Tree.Node (T Two, []) -> 2
    | Tree.Node (T Three, []) -> 3
    | Tree.Node (T Five, []) -> 5
    | _ -> raise ImpossibleBranch

  let pp fmt = function
      N Add -> Format.fprintf fmt "N Add"
    | N Mul -> Format.fprintf fmt "N Mul"
    | N Pow -> Format.fprintf fmt "N Pow"
    | T One -> Format.fprintf fmt "T One"
    | T Two -> Format.fprintf fmt "T Two"
    | T Three -> Format.fprintf fmt "T Three"
    | T Five -> Format.fprintf fmt "T Five"

  let validate t =
    Format.printf "VALIDATE:\n%a\n\n" (Tree.pp pp) t;
    (* Format.printf "%a\n\nBREAK\n\n" (Tree.pp pp) t; *)
    Format.print_flush ();
    (* let _ = read_line () in *)
    eval t = 100000
end

module M = AOStar.Make (T)

let () =
  let solved = M.run @@ M.init T.[N Add; N Mul; N Pow] in
  Format.printf "%a\n" (Tree.pp T.pp) solved
