type t

val of_json : Yojson.Basic.t -> t

(** [parse path] parses the JSON files at [path] *)
val parse : Fpath.t -> (t, [`Msg of string]) result

(** [save json path] saves the JSON data to [path] *)
val save : t -> Fpath.t -> (unit, [`Msg of string]) result

(** [read_int t] attempts to read [t] as a JSON int *)
val read_int : t -> (int, [`Msg of string]) result

(** [read_assoc t] attempts to read [t] as a JSON map *)
val read_assoc : t -> ((string * t) list, [`Msg of string]) result

(** [of_int i] converts [i] into a JSON int *)
val of_int : int -> t

(** [of_assoc l] converts [l] into a JSON map *)
val of_assoc : (string * t) list -> t

(** [of_string s] converts [s] into a JSON string *)
val of_string : string -> t
