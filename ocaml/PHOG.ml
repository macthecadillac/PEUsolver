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

  let to_string a = String.concat "\t" (List.map Grammar.to_string a)

  let of_string s = List.map Grammar.of_string @@ String.split_on_char '\t' s
end

type t = PCFG.t * PCFG.t ContextMap.t

let pp fmt (pcfg, cmap) =
  Format.fprintf fmt "Base PCFG:\n%a\n" PCFG.pp pcfg;
  ContextMap.iter
  (fun key map ->
    Format.fprintf fmt "Context: %a\nPCFG:\n%a\n\n" Context.pp key PCFG.pp map)
  cmap

type raw_t = PCFG.raw_t * (Context.t * PCFG.raw_t) list

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
      l, h in
    rev_append contexts acc)
  []

let train ntMap p asts =
  let open List in
  let pairs = enumerate_hog p asts in
  let aux acc (context, rule) =
    let f = function None -> Some [rule] | Some l -> Some (rule::l) in
    ContextMap.update context f acc in
  let l =
    let* context, rules = fold_left aux ContextMap.empty pairs
      |> ContextMap.to_list in
    let* () = mguard @@ not @@ is_empty context in (* discard empty context *)
    [context, PCFG.count ntMap rules] in
  PCFG.train ntMap [] asts, l

let compile ntMap (p_raw, l) =
  let open List.Infix in
  let l =
    let+ context, pcfg_raw = l in
    context, PCFG.compile ntMap pcfg_raw in
  PCFG.compile ntMap p_raw, ContextMap.of_list l

let encode (p_raw, l) =
  let assoc =
    let open List.Infix in
    let+ context, pcfg_raw = l in
    let contextString = Context.to_string context in
    contextString, PCFG.encode pcfg_raw in
  let encodedPHOG = JSON.of_assoc assoc in
  let basePCFG = PCFG.encode p_raw in
  let jsonAssoc = ["context", encodedPHOG; "base", basePCFG] in
  JSON.of_assoc jsonAssoc

let decode t =
  let open Result.Infix in
  let* jsonAssoc = JSON.read_assoc t in
  let* contextAssoc = List.assoc_opt ~eq:String.equal "context" jsonAssoc
    |> Option.to_result (`Msg "PHOG.decode: malformed config") in
  let* pcfgJSON = List.assoc_opt ~eq:String.equal "base" jsonAssoc
    |> Option.to_result (`Msg "PHOG.decode: malformed config") in
  let* basePCFG = PCFG.decode pcfgJSON in
  let* assoc = JSON.read_assoc contextAssoc in
  let+ raw =
    List.map
    (function context, json ->
      let+ pcfg_raw = PCFG.decode json in
      Context.of_string context, pcfg_raw)
    assoc
    |> Result.flatten_l in
  basePCFG, raw

let rule_cost (basePCFG, m) context rule =
  let open Option.Infix in
  let costOpt =
    let* pcfg = ContextMap.get context m in
    try Some (PCFG.rule_cost pcfg rule) with
      Invalid_argument _ -> None in  (* PCFG doesn't contain ntType *)
  Option.get_or ~default:(PCFG.rule_cost basePCFG rule) costOpt

let ast_cost phog p t =
  let pairs = enumerate_hog p [t] in
  List.map (uncurry (rule_cost phog)) pairs
  |> List.fold_left (+.) 0.


