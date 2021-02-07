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
  let rec list_pp s fmt = function
      [] -> ()
    | [a] -> Format.fprintf fmt "\n%s└── %a" s (aux ("    " ^ s)) a
    | hd::tl ->
        Format.fprintf fmt "\n%s├── %a" s (aux ("    │" ^ (String.drop 1 s))) hd;
        list_pp s fmt tl;
  and aux s fmt = function
      Node (a, []) -> Format.fprintf fmt "%a" pp_item a;
    | Node (a, l) ->
        Format.fprintf fmt "%a" pp_item a;
        Format.fprintf fmt "%a" (list_pp s) l;
  in aux "" fmt t
