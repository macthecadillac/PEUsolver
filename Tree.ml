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

let pp pp_item fmt t =
  let rec aux n fmt = function
      Node (a, []) -> Format.fprintf fmt "\n%sLeaf %a" (String.repeat " " n) pp_item a;
    | Node (a, l) ->
        let s = String.repeat " " n in
        let pp_start = fun fmt () -> Format.fprintf fmt "[" in
        let pp_stop = fun fmt () -> Format.fprintf fmt "]" in
        let pp_sep = fun fmt () -> Format.fprintf fmt ";%s" s in
        let pp_list = List.pp ~pp_start ~pp_stop ~pp_sep (aux @@ n + 2) in
        Format.fprintf fmt "\n%sNode %a %a" s pp_item a pp_list l
  in aux 0 fmt t
