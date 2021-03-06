type t

val parse_pcfg_config : string -> (t, [`Msg of string]) result

val build_cost_map : t -> (float Grammar.Map.t, [`Msg of string]) result
