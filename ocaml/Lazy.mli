(** A monadic implementation of lazy values with caching *)

(** A lazy value type *)
type 'a t

val is_forced : 'a t -> bool

(** Convert a function that takes a unit value as the first argument into the
    lazy type *)
val of_thunk : (unit -> 'a) -> 'a t

(** Force a value *)
val force : 'a t -> 'a

(** A lazy type is a functor *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Alias to [map] *)
val (<$>) : ('a -> 'b) -> 'a t -> 'b t

(** A lazy type is an applicative functor *)
val pure : 'a -> 'a t

(** Alias to [pure] *)
val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

(** Syntactic sugar for the applicative functor *)
val (let+) : 'a t -> ('a -> 'b) -> 'b t

val (and+) : 'a t -> 'b t -> ('a * 'b) t

(** A lazy type is a monad *)
val return : 'a -> 'a t

(** Join two monadic layers *)
val join : 'a t t -> 'a t

(** Monadic bind *)
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

(** Syntactic sugar for monadic bind *)
val (let*) : 'a t -> ('a -> 'b t) -> 'b t

val (and*) : 'a t -> 'b t -> ('a * 'b) t
