open Containers
open Fun
open Bos.OS

type t = string

type 'a printer = Format.formatter -> 'a -> unit

let pp fmt s = Format.fprintf fmt "%s" s

let equal = String.equal

let compare = String.compare

let of_string a = a

let to_string a = a

let init = "Start"

let parse_spec fpath =
  let open Result.Infix in
  let* str = File.read fpath in
  Sexp.parse_string_list str |> Result.map_err (fun s -> `Msg s)

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
  let open Result.Infix in
  let filter_fun_def = function `List (`Atom "synth-fun"::_) -> true | _ -> false in
  let* l = List.filter filter_fun_def spec |> List.hd |> sexp_list in
  (* drop synth-fun, f, args, return *)
  List.drop 4 l |> List.hd |> sexp_list

let succession_map specs =
  let open Result.Infix in
  let build_map (a : Sexp.t) =
    let aux_nonterms l =
      let* l = List.map sexp_atom l |> Result.flatten_l in
      match l with
        name::rest -> Ok (name, rest)
      | _ -> Error (`Msg "Malformed spec") in
    let* l = sexp_list a in
    match l with
      [`Atom ntType; `Atom t; `List terms] ->
        let args_f s =
          let pred = String.equal "_arg_" @@ String.take 5 s in
          match t with
            (* add quotes for string literals. Prepend type for arguments *)
            "String" ->
              if pred then "str." ^ s else
                if String.equal init ntType then s  (* starting symbol is never literal *)
              else Printf.sprintf "\"%s\"" s
          | "Int" -> if pred then "int." ^ s else s
          | "Bool" -> if pred then "bool." ^ s else s
          | _ -> s in
        let tm' =
          let+ desc =
            List.map
            (function
              `Atom s -> Ok (args_f s)
            | `List (`Atom s::_) -> Ok s
            | _ -> Error (`Msg "Malformed spec"))
            terms
            |> Result.flatten_l in
          Map.add ntType desc Map.empty in
        List.fold_left
        (fun acc -> function
            `Atom s -> Map.add (args_f s) [] <$> acc
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
  let open Result.Infix in
  let* smap = succession_map specs in
  let* specs' = filter_grammar_spec specs in
  let* l = List.map sexp_list specs' |> Result.flatten_l in
  let+ assoc =
    Result.flatten_l @@
    List.map
    (function
      `Atom ntType::_ ->
        let open List in
        let l' =
          let+ rule = Option.get_exn @@ Map.get ntType smap in
          Ok (rule, ntType) in
        Result.flatten_l l'
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

let build_solution_ast ntMap (spec : Sexp.t list) =
  (* FIXME: hack to get around quirk about quotes in sexp parsing *)
  let add_quote s =
    if Option.is_none @@ Map.get s ntMap && not String.(equal "_arg_" @@ take 5 s)
    then "\"" ^ s ^ "\"" else s in
  let rec traverse = function
      `List (`Atom hd::tl) ->
        Tree.Node (add_quote hd, List.map traverse tl)
    | `List [] | `List (`List _::_) -> raise (Invalid_argument "Check code")
    | `Atom a -> Tree.Node (add_quote a, []) in
  let open Result.Infix in
  let filter_fun_def = function `List (`Atom "define-fun"::_) -> true | _ -> false in
  let* l = List.filter filter_fun_def spec |> List.hd |> sexp_list in
  (* get arg types *)
  let* args =
    match List.nth l 2 with
      `List l -> Ok l
    | _ -> Error (`Msg "Malformed spec") in
  (* drop define-fun, f, args, return *)
  let ast = List.drop 4 l |> List.hd |> traverse in
  (* prefix all args *)
  List.fold_left
  (fun acc -> function
      `List [`Atom arg; `Atom "String"] ->
        let+ t = acc in
        Tree.map (fun s -> if String.equal s arg then "str." ^ arg else s) t
    | `List [`Atom arg; `Atom "Int"] ->
        let+ t = acc in
        Tree.map (fun s -> if String.equal s arg then "int." ^ arg else s) t
    | `List [`Atom arg; `Atom "Bool"] ->
        let+ t = acc in
        Tree.map (fun s -> if String.equal s arg then "bool." ^ arg else s) t
    | _ -> Error (`Msg "Malformed spec"))
  (Ok ast) args

let parse_constraints (spec : Sexp.t list) =
  let open Result.Infix in
  let filter_synth = function `List (`Atom "synth-fun"::_) -> true | _ -> false in
  let filter_constraints = function `List (`Atom "constraint"::_) -> true | _ -> false in
  let* constraints = List.filter filter_constraints spec
    |> List.map sexp_list |> Result.flatten_l in
  let parse_args fargs =
    let* args = sexp_list fargs in
    match args with
      `Atom "f"::s -> List.map sexp_atom s |> Result.flatten_l
    | _ -> Error (`Msg "Grammar.parse_constraints: malformed constraint") in
  let parse = function
      [`Atom "constraint"; `List [`Atom "="; fargs; `Atom res]] ->
        let+ args = parse_args fargs in
        args, res
    | _ -> Error (`Msg "Grammar.parse_constraints: malformed constraint") in
  let parse_type = function
      "String" -> Ok `String
    | "Int" -> Ok `Int
    | "Bool" -> Ok `Bool
    | s -> Error (`Msg ("Grammar.parse_constraints: unrecognized type: " ^ s)) in
  let parse_arg_type = function
      `List [`Atom _; `Atom s] -> parse_type s
    | _ -> Error (`Msg "Grammar.parse_constraints: malformed arg list") in
  let* spec = List.filter filter_synth spec |> List.hd |> sexp_list in
  let* argList = List.nth spec 2 |> sexp_list in
  let* out = List.nth spec 3 |> sexp_atom in
  let* outputType = parse_type out in
  let* argTypes = List.map parse_arg_type argList |> Result.flatten_l in
  let* constraints = List.map parse constraints |> Result.flatten_l in
  Result.flatten_l
  @@ List.map
     (fun (args, res) ->
       let* resVal = Value.of_string outputType res in
       let+ argVals = Result.flatten_l
         @@ List.map (fun (arg, t) -> Value.of_string t arg)
         @@ List.combine args argTypes in
       argVals, resVal)
     constraints

let is_hole prodMap term =
  not @@ List.is_empty @@ Option.get_exn @@ Map.get term prodMap
