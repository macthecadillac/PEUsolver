(** The tree type *)
type 'a t = Node of 'a * 'a t list

(** The printer type *)
type 'a printer = Format.formatter -> 'a -> unit

(** [map f t] maps function [f] over a tree structure [t] *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [fold_list f t] folds [t] using one of the elements in [t] as the initial
    accumulator in postorder. Identical in behavior to the [fold] function in
    [Data.Tree] of Haskell *)
val fold : ('a -> 'b list -> 'b) -> 'a t -> 'b

(** [fold f acc t] is the good ol' fashioned fold. In preorder. *)
val fold' : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** [flatten t] flattens a tree into a list. In preorder. *)
val flatten : 'a t -> 'a list

(** [unfold f seed] builds a tree with the provided function and the seed value.
    Analogous to the [unfold] function in Haskell's [Data.Tree] module in
    [containers] *)
val unfold : ('a -> 'b * 'a list) -> 'a -> 'b t

val return : 'a -> 'a t

(** A pretty printer that formats everything in one line. *)
val pp' : 'a printer -> 'a t printer

(** A pretty printer that tries to draw a tree on screen. *)
val pp : 'a printer -> 'a t printer
