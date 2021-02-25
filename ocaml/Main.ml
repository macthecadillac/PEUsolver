open Containers
open Fun

module T = struct
  open AOStar

  exception ImpossibleBranch
  exception NotANumber
  exception NotAList

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

  let list_desc = [1, T List; 20, N Sort; 20, N Slice; 20, N Concat; 10, N Singleton]
  let int_desc = [1, T Zero; 20, N Find]

  let successors = function
      N Sort -> [1, list_desc]
    | N Singleton -> [1, int_desc]
    | N Slice -> [1, list_desc; 1, int_desc; 1, int_desc]
    | N Concat -> [1, list_desc; 1, list_desc]
    | N Find -> [1, list_desc; 1, int_desc]
    | T List | T Zero -> []

  type ast_type = I of int | L of int list

  let rec eval l = function
      Tree.Node (T Zero, []) -> I 0
    | Tree.Node (T List, []) -> L l
    | Tree.Node (N Sort, [a]) ->
        begin
          match eval l a with
            L l' -> L (List.sort Int.compare l')
          | I _ -> raise ImpossibleBranch
        end
    | Tree.Node (N Singleton, [a]) ->
        begin
          match eval l a with
            I i -> L [i]
          | L l' -> raise ImpossibleBranch
        end
    | Tree.Node (N Slice, [a; b; c]) ->
        begin
          match eval l a, eval l b, eval l c with
            L l', I i, I j -> L (List.take j l' |> List.drop i)
          | _ -> raise ImpossibleBranch
        end
    | Tree.Node (N Concat, [a; b]) ->
        begin
          match eval l a, eval l b with
            L l1, L l2 -> L (l1 @ l2)
          | _ -> raise ImpossibleBranch
        end
    | Tree.Node (N Find, [a; b]) ->
        begin
          match eval l a, eval l b with
            L l', I i -> I (List.find_idx ((=) i) l' |> Option.get_exn |> fst)
          | _ -> raise ImpossibleBranch
        end
    | _ -> raise ImpossibleBranch

  let pp fmt = function
      N Sort -> Format.fprintf fmt "N Sort"
    | N Singleton -> Format.fprintf fmt "N Singleton"
    | N Slice -> Format.fprintf fmt "N Slice"
    | N Concat -> Format.fprintf fmt "N Concat"
    | N Find -> Format.fprintf fmt "N Find"
    | T List -> Format.fprintf fmt "T List"
    | T Zero -> Format.fprintf fmt "T Zero"

  let rec to_string = function
      Tree.Node (T Zero, []) -> "0"
    | Tree.Node (T List, []) -> "x"
    | Tree.Node (N Sort, [a]) -> Printf.sprintf "sort(%s)" (to_string a)
    | Tree.Node (N Singleton, [a]) -> Printf.sprintf "[%s]" (to_string a)
    | Tree.Node (N Slice, [a; b; c]) ->
        Printf.sprintf "%s[%s..%s]" (to_string a) (to_string b) (to_string c)
    | Tree.Node (N Concat, [a; b]) ->
        Printf.sprintf "(%s + %s)" (to_string a) (to_string b)
    | Tree.Node (N Find, [a; b]) ->
        Printf.sprintf "find(%s, %s)" (to_string a) (to_string b)
    | _ -> raise ImpossibleBranch

  let validate t =
    Format.printf "%s\n" (to_string t);
    Format.print_flush ();
    (* let _ = read_line () in *)
    match eval [1; 4; 0; 6] t with
      L l -> List.equal Int.equal l [1; 4]
    | I i -> Printf.printf "%i\n" i; raise ImpossibleBranch
end

module M = AOStar.Make (T)

let () =
  let solved = M.run @@ M.init T.[1, T List; 10, N Singleton; 20, N Sort; 20, N Slice; 20, N Concat] in
  match solved with
    Error e -> Format.printf "Error %a\n" AOStar.error_pp e
  | Ok t -> Format.printf "Ok %s\n" (T.to_string t)
