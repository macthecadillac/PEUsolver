open Containers
open Bos.OS

type t = Yojson.Basic.t

let of_json a = a

let parse s =
  let open Result in
  let* fpath = Fpath.of_string s in
  let+ str = File.read fpath in
  Yojson.Basic.from_string str

let read_int = function
    `Int i -> Ok i
  | _ -> Error (`Msg "Malformed PCFG config")

let read_assoc = function
    `Assoc l -> Ok l
  | _ -> Error (`Msg "Malformed PCFG config")

let of_int i = `Int i

let of_assoc l = `Assoc l

let of_string s = `String s

let save (t : t) path =
  let open Result.Infix in
  let str = Yojson.Basic.to_string t in
  let* fpath = Fpath.of_string path in
  File.write fpath str
