type 'a t = Node of 'a * 'a t list

type 'a printer = Format.formatter -> 'a -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

(** [fold f acc t] is the good ol' fashioned fold *)
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** [fold_list f t] folds [t] using one of the elements in [t] as the initial
    accumulator. Identical in behavior to the [fold] function in Data.Tree of
    Haskell *)
val fold_list : ('a -> 'b list -> 'b) -> 'a t -> 'b

val flatten : 'a t -> 'a list

val debug : 'a printer -> 'a t printer

val pp : 'a printer -> 'a t printer
