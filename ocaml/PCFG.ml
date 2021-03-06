open Containers
open Fun
open Bos.OS

type t = Yojson.Basic.t

let parse_pcfg_config s =
  let open Result in
  let* fpath = Fpath.of_string s in
  let+ str = File.read fpath in
  Yojson.Basic.from_string str

let read_json_float = function
    `Float f -> Ok f
  | _ -> Error (`Msg "Malformed PCFG config")

let read_json_assoc = function
    `Assoc l -> Ok l
  | _ -> Error (`Msg "Malformed PCFG config")

let build_cost_map config =
  let open Result in
  let* l = read_json_assoc config in
  let* termTypes = List.assoc_opt ~eq:String.equal "pcfg" l
    |> Option.to_result (`Msg "Malformed PCFG config") in
  let* assoc = read_json_assoc termTypes in
  let* maps = List.map snd assoc |> List.map read_json_assoc |> flatten_l in
  let+ ruleCostPair = List.flatten maps
    |> List.map (fun (a, b) -> let+ c = read_json_float b in Grammar.of_string a, c)
    |> flatten_l in
  Grammar.Map.of_list ruleCostPair
