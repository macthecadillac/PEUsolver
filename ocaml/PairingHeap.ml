(* A simple implementation of the pairing heap with some of my own modifications
 * to accomodate multiple elements sharing the same priority. Pairing heaps have
 * much better amortized time complexity than a Leftist heap which is what comes
 * with Containers. *)
open Containers
open Fun

type 'a printer = Format.formatter -> 'a -> unit

module type S = sig
  type elt
  type t
  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val find_min : t -> elt option
  val find_min_exn : t -> elt
  val meld : t -> t -> t
  val insert : t -> elt -> t
  val delete_min : t -> t
  val to_list : t -> elt list
  val of_list : elt list -> t
  val pp : elt printer -> t printer
  val pp' : elt printer -> t printer
end

module type ORDERING = sig
  type t
  val compare : t -> t -> int
end

module Make (E : ORDERING) : S
  with type elt = E.t = struct
  type elt = E.t

  type t = 
      Empty
    | Heap of elt * t list

  exception EmptyHeap

  let empty = Empty

  let singleton a = Heap (a, [])

  let is_empty = function Empty -> true | _ -> false

  let find_min = function
      Empty -> None
    | Heap (elt, _) -> Some elt

  let find_min_exn = function
      Empty -> raise EmptyHeap
    | Heap (elt, _) -> elt

  let meld heap1 heap2 =
    match heap1, heap2 with
      Empty, a | a, Empty -> a
    | Heap (elt1, t1), Heap (elt2, t2) ->
        if E.compare elt1 elt2 <= 0 then Heap (elt1, heap2 :: t1)
        else Heap (elt2, heap1 :: t2)

  let insert heap elt = meld (Heap (elt, [])) heap

  let rec _merge_pairs acc = function
      []               -> acc
    | t :: []          -> meld acc t
    | fst :: snd :: tl -> _merge_pairs (meld acc @@ meld fst snd) tl

  let delete_min = function
      Empty -> Empty
    | Heap (_, subheaps) -> _merge_pairs empty subheaps

  let to_list h =
    let rec aux acc h =
      match find_min h with
        None -> List.rev acc
      | Some a -> aux (a::acc) (delete_min h)
    in aux [] h

  let of_list = List.fold_left insert empty

  let pp pp_item =
    let rec list_pp s fmt = function
        [] -> ()
      | [a] -> Format.fprintf fmt "\n%s└── %a" s (node_pp (s ^ "    ")) a
      | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (node_pp (s ^ "│   ")) hd;
                  list_pp s fmt tl;
    and node_pp s fmt = function
        Empty -> ()
      | Heap (a, l) -> Format.fprintf fmt "%a%a" pp_item a (list_pp s) l;
    in node_pp ""

  let rec pp' pp_item fmt = function
      Empty -> Format.fprintf fmt "Empty"
    | Heap (a, l) ->
        let list_pp =
          List.pp
            ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
            ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
            (pp' pp_item)
        in Format.fprintf fmt "Heap %a %a" pp_item a list_pp l
end
