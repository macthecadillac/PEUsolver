type 'a t = Node of 'a * 'a t list

type 'a printer = Format.formatter -> 'a -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold f acc t] is the good ol' fashioned fold *)

val fold_list : ('a -> 'b list -> 'b) -> 'a t -> 'b
(** [fold_list f t] folds [t] using one of the elements in [t] as the initial
    accumulator. Identical in behavior to the [fold] function in Data.Tree of
    Haskell *)

val flatten : 'a t -> 'a list

val pp : 'a printer -> 'a t printer
