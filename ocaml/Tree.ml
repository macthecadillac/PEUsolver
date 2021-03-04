open Containers

type 'a t = Node of 'a * 'a t list

type 'a printer = Format.formatter -> 'a -> unit

let rec map f (Node (a, l)) = Node (f a, List.map (map f) l)

(* postorder *)
let rec fold f (Node (a, ts)) = List.map (fold f) ts |> f a

(* preorder *)
let rec fold' f acc (Node (a, ts)) = List.fold_left (fold' f) (f acc a) ts

(* preorder *)
let flatten t = fold' (Fun.flip List.cons) [] t

let return a = Node (a, [])

let rec unfold f seed =
  let a, l = f seed in
  Node (a, (List.map (unfold f) l))

let rec pp' pp_item fmt = function
    Node (a, []) -> Format.fprintf fmt "Leaf %a" pp_item a
  | Node (a, l) ->
      let list_pp =
        List.pp
          ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
          ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
          (pp' pp_item)
      in Format.fprintf fmt "Node %a %a" pp_item a list_pp l


let pp pp_item =
  let rec list_pp s fmt = function
      [] -> ()
    | [a] -> Format.fprintf fmt "\n%s└── %a" s (node_pp (s ^ "    ")) a
    | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (node_pp (s ^ "│   ")) hd;
                list_pp s fmt tl;
  and node_pp s fmt = function
      Node (a, []) -> Format.fprintf fmt "%a" pp_item a;
    | Node (a, l) ->
        Format.fprintf fmt "%a%a" pp_item a (list_pp s) l;
  in node_pp ""
