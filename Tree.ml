open Containers

type 'a t = Node of 'a * 'a t list

type 'a printer = Format.formatter -> 'a -> unit

let rec map f (Node (a, l)) = Node (f a, List.map (map f) l)

(* preorder *)
let rec fold f acc (Node (a, ts)) = List.fold_left (fold f) (f acc a) ts

(* postorder *)
let rec fold_list f (Node (a, ts)) = List.map (fold_list f) ts |> f a

(* preorder *)
let flatten t = fold (Fun.flip List.cons) [] t

let rec pp pp_item fmt = function
    Node (a, []) -> Format.fprintf fmt "Leaf %a" pp_item a;
  | Node (a, l) ->
      let pp_start = fun fmt () -> Format.fprintf fmt "[" in
      let pp_stop = fun fmt () -> Format.fprintf fmt "]" in
      let pp_sep = fun fmt () -> Format.fprintf fmt "; " in
      let pp_list = List.pp ~pp_start ~pp_stop ~pp_sep (pp pp_item) in
      Format.fprintf fmt "Node %a %a" pp_item a pp_list l
