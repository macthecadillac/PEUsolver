type move =
    Up
  | Left
  | Right
  | DownFirst
  | DownLast
  | PrevDFS

type write = WriteValue

type tcond = W of write | M of move

type p = tcond list

type 'a printer = Format.formatter -> 'a -> unit

(** [apply loc ast p] applies TCOND program [p] to [ast] starting at location
    [loc] and return the starting node and the context. [loc] describes the
    location on the AST using TCOND operators starting at the root of the tree. *)
val apply : tcond list -> Grammar.t Tree.t -> p -> Grammar.t * Grammar.t list

(** [mutate p r] mutates the TCOND program [p] *)
val mutate : p -> Random.State.t -> p

(** Pretty printer for TCOND operators *)
val pp : tcond printer

(** Pretty printer for TCOND programs *)
val pp' : p printer
