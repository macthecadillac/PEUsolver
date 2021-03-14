open Containers
open Fun
open Bos.OS

type 'a printer = Format.formatter -> 'a -> unit

module ContextMap = Map.Make (struct
  type t = Grammar.t list
  let compare = List.compare Grammar.compare
end)

module Context = struct
  type t = Grammar.t list

  let pp =
    let f s fmt () = Format.fprintf fmt "%s" s in
    List.pp ~pp_sep:(f "; ") ~pp_start:(f "[") ~pp_stop:(f "]") Grammar.pp

  let to_string a =
    let esc = Printf.sprintf "%c" '\t' in
    String.concat esc (List.map Grammar.to_string a)

  let of_string s = List.map Grammar.of_string @@ String.split_on_char '\t' s
end

type t = PCFG.t ContextMap.t

let pp fmt cmap =
  ContextMap.iter
  (fun key map ->
    Format.fprintf fmt "Context: %a\nPCFG:\n%a\n\n" Context.pp key PCFG.pp map)
  cmap

type raw_t = (Context.t * PCFG.raw_t) list

module T = TCOND
module M = Grammar.Map

(* enumerate all the nodes of the AST using the move operators *)
let loc_list =
  let rec aux = function
    Tree.Node (_, []) -> [[]]
  | Tree.Node (_, ns) ->
      let open List in
      let locs =
        let* i, node = mapi Pair.make ns in
        let+ loc = aux node in
        T.(M DownFirst::rev_append (replicate i (M Right)) loc) in
      []::locs
  in List.rev % aux

(* enumerate all the context + production rule pairs of the training AST set *)
let enumerate_hog p =
  let open List in
  fold_left
  (fun acc ast ->
    let contexts =
      let+ loc = rev @@ loc_list ast in
      let h, l = T.apply loc ast p in
      h, l in
    rev_append contexts acc)
  []

let train ntMap p asts =
  let pairs = enumerate_hog p asts in
  let f acc (rule, context) =
    ContextMap.update context
    (function
       None ->
         let ntType = Option.get_exn @@ M.get rule ntMap in
         Some (M.singleton ntType (M.singleton rule 1))
     | Some m ->
         let open Option in
         let ntType = get_exn @@ M.get rule ntMap in
         let inc_opt = (fold (fun acc a -> (+) a <$> acc) (pure 1)) in
         let update_w_default = fold (fun _ a -> Some (M.update rule inc_opt a))
           @@ Some (M.singleton rule 1) in
         Some (M.update ntType update_w_default m))
    acc in
  let open List.Infix in
  let+ context, inner = List.fold_left f ContextMap.empty pairs
    |> ContextMap.to_list in
  let assocs =
    let+ ntType, inner' = M.to_list inner in
    ntType, M.to_list inner' in
  context, assocs

let compile ntMap raw =
  let open List.Infix in
  let l =
    let+ context, pcfg_raw = raw in
    context, PCFG.compile ntMap pcfg_raw in
  ContextMap.of_list l

let encode raw =
  let assoc =
    let open List.Infix in
    let+ context, pcfg_raw = raw in
    let contextString = String.concat " " (Grammar.to_string <$> context) in
    contextString, PCFG.encode pcfg_raw in
  let encodedPHOG = JSON.of_assoc assoc in
  let jsonAssoc = ["contextual-word-count", encodedPHOG] in
  JSON.of_assoc jsonAssoc

let decode t =
  let open Result.Infix in
  let* jsonAssoc = JSON.read_assoc t in
  let contextAssoc =
    List.map
    (function context, json ->
      let+ pcfg_raw = PCFG.decode json in
      Context.of_string context, pcfg_raw)
    jsonAssoc in
  Result.flatten_l contextAssoc

(* FIXME: default should fall back to a fallback PCFG *)
let rule_cost phog context rule =
  let open Option.Infix in
  let costOpt =
    let+ pcfg = ContextMap.get context phog in
    PCFG.rule_cost pcfg rule in
  Option.get_or ~default:0. costOpt

let ast_cost phog context t =
  let aux = function
      s, [] -> rule_cost phog context s (* terminals *)
    | s, l -> List.fold_left (+.) (rule_cost phog context s) l in
  Tree.fold (curry aux) t
