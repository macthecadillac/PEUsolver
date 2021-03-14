open Containers
open Fun
open Bos.OS

type t = string

type 'a printer = Format.formatter -> 'a -> unit

let pp = String.pp

let equal = String.equal

let compare = String.compare

let of_string a = a

let to_string a = a

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

module Map = struct
  include CCMap.Make (String)
  let pp = 
    let f s fmt () = Format.fprintf fmt "%s" s in
    fun pp1 pp2 ->
      pp ~pp_sep:(f "\n")
         ~pp_arrow:(f " -> ")
         ~pp_start:(f "")
         ~pp_stop:(f "")
         pp1
         pp2
end

let filter_grammar_spec (spec : Sexp.t list) : (Sexp.t list, [`Msg of string]) result =
  let open Result in
  let filter_fun_def = function `List (`Atom "synth-fun"::_) -> true | _ -> false in
  let* l = List.filter filter_fun_def spec |> List.hd |> sexp_list in
  (* drop synth-fun, f, args, return *)
  List.drop 4 l |> List.hd |> sexp_list

let succession_map specs =
  let open Result in
  let build_map (a : Sexp.t) =
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
          Map.add ntType desc Map.empty in
        List.fold_left
        (fun acc -> function
            `Atom s -> Map.add s [] <$> acc
          | `List l ->
              let* m = acc in
              let+ k, v = aux_nonterms l in
              Map.add k v m)
        tm'
        terms
    | _ -> Error (`Msg "Malformed spec") in
  let merge_m acc smap =
    let* map1 = acc in
    let+ map2 = smap in
    let f _ = function
        `Left a | `Right a -> Some a
      | `Both (a, b) -> Some List.(sort_uniq String.compare (rev_append a b)) in
    Map.merge_safe ~f map1 map2 in
  let* specs' = filter_grammar_spec specs in
  List.fold_left (fun acc spec -> merge_m acc @@ build_map spec) (Ok Map.empty) specs'

let rule_nttype_map (specs : Sexp.t list) =
  let open Result in
  let* smap = succession_map specs in
  let* specs' = filter_grammar_spec specs in
  let* l = List.map sexp_list specs' |> flatten_l in
  let+ assoc =
    flatten_l @@
    List.map
    (function
      `Atom ntType::_ ->
        let open List in
        let l' =
          let+ rule = Option.get_exn @@ Map.get ntType smap in
          Ok (rule, ntType) in
        flatten_l l'
    | _ -> Error (`Msg "Malformed spec"))
    l in
  Map.of_list
  @@ List.filter (fun (a, b) -> not @@ String.equal b init)
  @@ List.flatten assoc

let merge_rule_nttype_maps map1 map2 =
  let flag = ref false in
  let conflict = ref "" in
  let f s = function
      `Left a | `Right a -> Some a
    | `Both (a, b) ->
        if equal a b then Some a
        else begin
          flag := true;
          conflict := Printf.sprintf "Key: %s, Left: %s, Right: %s" s a b;
          None
        end
  in
  let m = Map.merge_safe ~f map1 map2 in
  if !flag then
    let msg = Printf.sprintf "Conflicting key-val pair:\n  %s" !conflict in
    Error (`Msg msg)
  else Ok m

let build_solution_ast (spec : Sexp.t list) =
  let rec traverse = function
      `List (`Atom hd::tl) -> Tree.Node (hd, List.map traverse tl)
    | `List [] | `List (`List _::_) -> raise (Invalid_argument "Check code")
    | `Atom a -> Tree.Node (a, []) in
  let open Result in
  let filter_fun_def = function `List (`Atom "define-fun"::_) -> true | _ -> false in
  let+ l = List.filter filter_fun_def spec |> List.hd |> sexp_list in
  (* drop define-fun, f, args, return *)
  List.drop 4 l |> List.hd |> traverse

let is_hole prodMap term =
  not @@ List.is_empty @@ Option.get_exn @@ Map.get term prodMap

let str_pp s fmt () = Format.fprintf fmt "%s" s

let ast_pp fmt ast =
  let list_pp =
    List.pp ~pp_sep:(str_pp ", ")
            ~pp_start:(str_pp "")
            ~pp_stop:(str_pp "")
            (fun fmt s -> Format.fprintf fmt "%s" s) in
  let to_string = function
      s, [] -> Format.sprintf "%a" String.pp s
    | s, l -> Format.sprintf "%s(%a)" s list_pp l in
  Format.fprintf fmt "%s" @@ Tree.fold (curry to_string) ast
