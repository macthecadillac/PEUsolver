open Containers
open Fun
open Bos.OS

type t = string

let pp = String.pp

let equal = String.equal

let compare = String.compare

let of_string a = a

let init = "Start"

let parse_spec s =
  let open Result in
  let* fpath = Fpath.of_string s in
  let* str = File.read fpath in
  Sexp.parse_string_list str |> map_err (fun s -> `Msg s)

let sexp_atom = function
    `Atom a -> Ok a
  | `List l -> Error (`Msg "Invalid argument")

let sexp_list = function
    `List l -> Ok l
  | `Atom a -> Error (`Msg "Invalid argument")

module Map = CCMap.Make (String)

let build_succession_map a tm =
  let open Result in
  let aux_nonterms l =
    let* l = List.map sexp_atom l |> flatten_l in
    match l with
      name::rest -> Ok (name, rest)
    | _ -> Error (`Msg "Malformed spec") in
  let* l = sexp_list a in
  match l with
    [`Atom ntType; `Atom _; `List terms] ->
      let tm' =
        let+ desc =
          List.map
          (function
            `Atom s -> Ok s
          | `List (`Atom s::_) -> Ok s
          | _ -> Error (`Msg "Malformed spec"))
          terms
          |> flatten_l in
        Map.add ntType desc tm in
      List.fold_left
      (fun acc -> function
          `Atom s -> Map.add s [] <$> acc
        | `List l ->
            let* m = acc in
            let+ k, v = aux_nonterms l in
            Map.add k v m)
      tm'
      terms
  | _ -> Error (`Msg "Malformed spec")
                          
let filter_grammar_spec (spec : Sexp.t list) =
  let open Result in
  let filter_fun_def = function `List (`Atom "synth-fun"::_) -> true | _ -> false in
  let* l = List.filter filter_fun_def spec |> List.hd |> sexp_list in
  (* drop synth-fun, f, args, return *)
  List.drop 4 l |> List.hd |> sexp_list

let is_hole prodMap term =
  not @@ List.is_empty @@ Option.get_exn @@ Map.get term prodMap

let str_pp s fmt () = Format.fprintf fmt "%s" s

let ast_cost pcfg t =
  let aux = function
      s, [] -> Option.get_or ~default:0. (Map.get s pcfg)     (* terminals *)
    | s, l -> List.fold_left (+.) (Option.get_exn (Map.get s pcfg)) l in
  Tree.fold (curry aux) t

let ast_to_string =
  let list_pp =
    List.pp ~pp_sep:(str_pp ", ")
            ~pp_start:(str_pp "")
            ~pp_stop:(str_pp "")
            (fun fmt s -> Format.fprintf fmt "%s" s) in
  let to_string = function
      s, [] -> Format.sprintf "%a" String.pp s
    | s, l -> Format.sprintf "%s(%a)" s list_pp l
  in Tree.fold (curry to_string)
