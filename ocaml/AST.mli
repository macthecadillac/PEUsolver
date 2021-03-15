type t = Grammar.t Tree.t

type 'a printer = Format.formatter -> 'a -> unit

type value = Str of string | Int of int | Bool of bool

(** [equal ast1 ast2] tests for structural equality between ast1 and ast2 *)
val equal : t -> t -> bool

(** Evaluates an AST *)
val eval : ?arg0:value -> ?arg1:value -> ?arg2:value -> ?arg3:value -> t -> value

(** Pretty printer for ASTs *)
val pp : t printer

(** Pretty printer for values *)
val value_pp : value printer
