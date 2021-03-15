open Containers

type t = Str of string | Int of int | Bool of bool

type 'a printer = Format.formatter -> 'a -> unit

let pp fmt = function
    Str s -> Format.fprintf fmt "%s" s
  | Int i -> Format.fprintf fmt "%i" i
  | Bool b -> Format.fprintf fmt "%b" b

let equal a b =
  match a, b with
    Str a, Str b -> String.equal a b
  | Int a, Int b -> Int.equal a b
  | Bool a, Bool b -> Bool.equal a b
  | _ -> false

let of_string t s =
  match t with
    `String -> Ok (Str s)
  | `Int ->
      let open Result.Infix in
      let err = `Msg "Failed at Value.of_string" in
      let+ i = Option.to_result err (Int.of_string s) in
      Int i
  | `Bool ->
      match s with
        "true" -> Ok (Bool true)
      | "false" -> Ok (Bool false)
      | _ -> Error (`Msg "Failed at Value.of_string")
