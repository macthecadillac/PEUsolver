open Containers
open Fun
open Common

module T = struct
  open AOStar

  exception ImpossibleBranch

  type nonterm = Sort | Singleton | Slice | Concat | Find
  type term = List | Zero
  type t = N of nonterm | T of term

  let equal = curry (function
      N Sort, N Sort
    | N Singleton, N Singleton
    | N Slice, N Slice
    | N Concat, N Concat
    | N Find, N Find
    | T List, T List
    | T Zero, T Zero -> true
    | _ -> false)

  let list_desc = [0, T List; 0, N Sort; 0, N Slice; 0, N Concat; 0, N Singleton]
  let int_desc = [0, T Zero; 0, N Find]

  let successors = function
      N Sort -> [0, [0, T List; 0, N Slice; 0, N Concat]]
    | N Singleton -> [0, int_desc]
    | N Slice -> [0, [0, T List; 0, N Sort; 0, N Concat]; 0, int_desc; 0, int_desc]
    | N Concat -> [0, [0, T List; 0, N Sort; 0, N Slice; 0, N Singleton]; 0, list_desc]
    | N Find -> [0, list_desc; 0, int_desc]
    | T List | T Zero -> []

  type ast_type = I of int | L of int list

  let eval l =
    let aux = function
        T Zero, [] -> I 0
      | T List, [] -> L l
      | N Sort, [L l'] -> L (List.sort Int.compare l')
      | N Singleton, [I i] -> L [i]
      | N Slice, [L l'; I i; I j] -> L (List.take j l' |> List.drop i)
      | N Concat, [L l1; L l2] -> L (l1 @ l2)
      | N Find, [L l'; I i] -> I (List.find_idx ((=) i) l' |> Option.get_exn |> fst)
      | _ -> raise ImpossibleBranch
    in Tree.fold (curry aux)

  let pp fmt = function
      N Sort -> Format.fprintf fmt "N Sort"
    | N Singleton -> Format.fprintf fmt "N Singleton"
    | N Slice -> Format.fprintf fmt "N Slice"
    | N Concat -> Format.fprintf fmt "N Concat"
    | N Find -> Format.fprintf fmt "N Find"
    | T List -> Format.fprintf fmt "T List"
    | T Zero -> Format.fprintf fmt "T Zero"

  let to_string =
    let aux = function
        T Zero, [] -> "0"
      | T List, [] -> "x"
      | N Sort, [a] -> Printf.sprintf "sort(%s)" a
      | N Singleton, [a] -> Printf.sprintf "[%s]" a
      | N Slice, [a; b; c] -> Printf.sprintf "%s[%s..%s]" a b c
      | N Concat, [a; b] -> Printf.sprintf "(%s + %s)" a b
      | N Find, [a; b] -> Printf.sprintf "find(%s, %s)" a b
      | _ -> raise ImpossibleBranch
    in Tree.fold (curry aux)

  let validate t =
    Format.printf "%s\n" (to_string t);
    Format.print_flush ();
    (* let _ = read_line () in *)
    try
      let solution = eval [1; 4; 0; 6] t in
      match solution with
        L l -> List.equal Int.equal l [1; 4]
      | I i -> Printf.printf "%i\n" i; raise ImpossibleBranch
    with
      Invalid_argument _ -> false
end

module AOS = AOStar.Make (T)
(* module AS = AStar.Make (T) *)

let () =
  let solved = AOS.run @@ AOS.init T.[0, T List; 0, N Singleton; 0, N Sort; 0, N Slice; 0, N Concat] in
  match solved with
    Error e -> Format.printf "Error %a\n" error_pp e
  | Ok t -> Format.printf "Ok %s\n" (T.to_string t)
