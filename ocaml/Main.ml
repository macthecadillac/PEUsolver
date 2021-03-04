open Containers
open Fun
open Common

module List = struct
  include List
  let sum = function [] -> 0 | hd::tl -> List.fold_left (+) hd tl
  let fold_first f = function
      [] -> raise (Invalid_argument "Empty list")
    | hd::tl -> fold_left f hd tl
  let min = fold_first min
end

module T : sig
  include AStar.S
  val to_string : t -> string
end = struct
  type nonterm = Sort | Singleton | Slice | Concat | Find
  type term = List | Zero
  type terms = N of nonterm | T of term
  type t = terms option Tree.t

  let init =
    List.map (Tree.return % Option.return)
      [N Sort; N Singleton; N Slice; N Concat; N Find; T List; T Zero]

  let weight _ _ = 1

  let cost t =
    let aux a = List.map (fun b -> Option.(get_or (weight <$> a <*> b) ~default:0))
      %> List.sum
      %> Option.pure in
    Tree.fold aux t |> Option.get_or ~default: 0

  let is_ground =
    Tree.fold (fun _ blist -> List.for_all id blist)
    % Tree.map (function Some (N _) | None -> false | Some (T _) -> true)

  let list_type = [T List; N Sort; N Singleton; N Slice; N Concat]
  let int_type = [T Zero; N Find]

  let successors = function
      T _ -> []
    | N Sort -> [list_type]
    | N Singleton -> [int_type]
    | N Slice -> [list_type; int_type; int_type]
    | N Concat -> [list_type; list_type]
    | N Find -> [list_type; int_type]

  let choose a bll =
    let weight_or = List.map (weight a) %> List.min in
    let alt (w, i) (w', j) = if w > w' then w', j else w, i in
    let minIdx = List.mapi (fun i a -> weight_or a, i) bll |> List.fold_first alt |> snd in
    List.mapi (fun i n -> if i = minIdx then Some n else None) bll

  (* let unroll t = Tree.fold (fun a blist -> Tree.unfold *) 
  let unroll =
    let choose a l =
      let weight_or = List.map (function `Node (a, _) -> a) %> List.map (weight a) %> List.min in
      let alt (w, i) (w', j) = if w > w' then w', j else w, i in
      let minIdx = List.mapi (fun i a -> weight_or a, i) l |> List.fold_first alt |> snd in
      List.mapi (fun i n ->
        if i = minIdx
        then Lazy.pure n
        else Lazy.of_thunk (fun () -> n)) l in
    let aux = function
        a, [] when Lazy.is_forced a -> `Node (a, [])
      | a, [] -> `Node (Lazy.force a)
      | a, l -> () in
    ()

  let to_string =
    let aux = function
        T Zero, [] -> "0"
      | T List, [] -> "x"
      | N Sort, [a] -> Printf.sprintf "sort(%s)" a
      | N Singleton, [a] -> Printf.sprintf "[%s]" a
      | N Slice, [a; b; c] -> Printf.sprintf "%s[%s..%s]" a b c
      | N Concat, [a; b] -> Printf.sprintf "(%s + %s)" a b
      | N Find, [a; b] -> Printf.sprintf "find(%s, %s)" a b
      | _ -> raise (Invalid_argument "Check code")
    in Tree.fold (curry aux)
end

module M = AStar.AS (T)

let () =
  Seq.take 20 M.sequence
      |> Seq.iter (fun s -> print_endline (T.to_string s))
