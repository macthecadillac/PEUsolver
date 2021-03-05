open Containers
open Fun

module TestGrammar = struct
  type t = L | N | List | Sort | Singleton | Slice | Concat | Zero | Find

  let successors = function
      List | Zero -> []
    | L -> [List; Sort; Singleton; Slice; Concat]
    | N -> [Zero; Find]
    | Sort -> [L]
    | Singleton -> [N]
    | Slice -> [L; N; N]
    | Concat -> [L; L]
    | Find -> [L; N]

  let init = [L]

  let is_hole = function L | N -> true | _ -> false

  let ast_cost =
    let aux = function
        L, _ | N, _ | Zero, _ | List, _ -> 0   (* terminals *)
      | _, l -> 1 + List.fold_left (+) 0 l in  (* productions *)
    Tree.fold (curry aux)

  let ast_to_string =
    let aux = function
        Zero, [] -> "0"
      | List, [] -> "x"
      | Sort, [a] -> Printf.sprintf "sort(%s)" a
      | Singleton, [a] -> Printf.sprintf "[%s]" a
      | Slice, [a; b; c] -> Printf.sprintf "%s[%s..%s]" a b c
      | Concat, [a; b] -> Printf.sprintf "%s + %s" a b
      | Find, [a; b] -> Printf.sprintf "find(%s, %s)" a b
      | L, [] -> "L"
      | N, [] -> "N"
      | _ -> raise (Invalid_argument "Inconsistent arity")
    in Tree.fold (curry aux)
end

module EnumerativeSearch = AStar.AS (TestGrammar)

let () =
  Seq.take 100 EnumerativeSearch.sequence
  |> Seq.iter (fun s -> Printf.printf "%s\n" (TestGrammar.ast_to_string s))
