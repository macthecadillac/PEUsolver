type t = Grammar.t Tree.t

type 'a printer = Format.formatter -> 'a -> unit

module Value : sig
  type t = Str of string | Int of int | Bool of bool

  val equal : t -> t -> bool

  (** Pretty printer for values *)
  val pp : t printer
end

(** [equal ast1 ast2] tests for structural equality between ast1 and ast2 *)
val equal : t -> t -> bool

(** [equal ast1 ast2] test for observational equality between ast1 and ast2 *)
val equal' : t -> t -> bool

(** Evaluates an AST *)
val eval : ?arg0:Value.t -> ?arg1:Value.t -> ?arg2:Value.t -> ?arg3:Value.t -> t -> Value.t

(** Pretty printer for ASTs *)
val pp : t printer
