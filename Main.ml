open Containers
open Fun

module T = struct
  open AOStar

  exception ImpossibleBranch

  type nonterm = Plus | Mul
  type term = Two
  type t = N of nonterm | T of term

  let equal = curry (function
      N Plus, N Plus | N Mul, N Mul -> true
    | T Two, T Two -> true
    | _ -> false)

  let possible_terms = [(1, T Two); (2, N Plus); (2, N Mul)]

  let successors = function
      N Plus | N Mul -> [possible_terms; possible_terms]
    | T Two -> []

  let est_cost = function
      N Plus | N Mul -> 2
    | T Two -> 1

  let rec eval = function
      Tree.Node (N Plus, [a; b]) -> (eval a) + (eval b)
    | Tree.Node (N Mul, [a; b]) -> (eval a) * (eval b)
    | Tree.Node (T Two, []) -> 2
    | _ -> raise ImpossibleBranch

  let pp fmt = function
      N Plus -> Format.fprintf fmt "N Plus"
    | N Mul -> Format.fprintf fmt "N Mul"
    | T Two -> Format.fprintf fmt "T Two"

  let validate t =
    Format.printf "%a\n" (Tree.pp pp) t;
    eval t = 10
end

module M = AOStar.Make (T)

let () =
  let solved = M.run @@ M.init T.(N Plus) in
  let pp = Result.pp' (Tree.pp T.pp) AOStar.error_pp in
  Format.printf "%a\n" pp solved
