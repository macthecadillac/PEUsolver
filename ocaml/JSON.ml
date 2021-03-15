open Containers
open Bos.OS

type t = Yojson.Basic.t

let of_json a = a

let parse fpath =
  let open Result in
  let+ str = File.read fpath in
  Yojson.Basic.from_string str

let read_int = function
    `Int i -> Ok i
  | _ -> Error (`Msg "JSON.read_int: malformed config")

let read_assoc = function
    `Assoc l -> Ok l
  | _ -> Error (`Msg "JSON.read_assoc: malformed config")

let of_int i = `Int i

let of_assoc l = `Assoc l

let of_string s = `String s

let save t fpath =
  let open Result.Infix in
  let str = Yojson.Basic.pretty_to_string t in
  File.write fpath str
