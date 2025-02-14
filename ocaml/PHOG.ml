open Containers
open Fun
open Bos.OS

module ContextMap = TCOND.ContextMap

module Context = TCOND.Context

type 'a printer = Format.formatter -> 'a -> unit

type t = PCFG.raw_t * PCFG.t * PCFG.t ContextMap.t

let logBase2 = log 2.
let log2 a = log a  /. logBase2

let pp fmt (_, pcfg, cmap) =
  Format.fprintf fmt "Base PCFG:\n%a\n" PCFG.pp pcfg;
  ContextMap.iter
  (fun key map ->
    Format.fprintf fmt "Context: %a\nPCFG:\n%a\n\n" Context.pp key PCFG.pp map)
  cmap

type raw_t = PCFG.raw_t * (Context.t * PCFG.raw_t) list

module T = TCOND
module M = Grammar.Map

let train ntMap p asts =
  let open List in
  let pairs = T.enumerate_context_rule_pairs p asts in
  let aux acc (context, rule) =
    let f = function None -> Some [rule] | Some l -> Some (rule::l) in
    ContextMap.update context f acc in
  let l =
    let* context, rules = fold_left aux ContextMap.empty pairs
      |> ContextMap.to_list in
    let* () = mguard @@ not @@ is_empty context in (* discard empty context *)
    [context, PCFG.count ntMap rules] in
  PCFG.train ntMap asts, l

let compile ntMap (p_raw, l) =
  let open List.Infix in
  let l' =
    let+ context, pcfg_raw = l in
    context, PCFG.compile ntMap pcfg_raw in
  p_raw, PCFG.compile ntMap p_raw, ContextMap.of_list l'

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

let rule_cost (_, basePCFG, m) context rule =
  let open Option.Infix in
  let costOpt =
    let* pcfg = ContextMap.get context m in
    try Some (PCFG.rule_cost pcfg rule) with
      Invalid_argument _ -> None in  (* PCFG doesn't contain ntType *)
  Option.get_or ~default:(PCFG.rule_cost basePCFG rule) costOpt

let ast_cost phog p t =
  let pairs = T.enumerate_context_rule_pairs p [t] in
  List.map (uncurry (rule_cost phog)) pairs
  |> List.fold_left (+.) 0.

let compute_heuristic succMap ((pcfgRaw, _, contextMap) : t) =
  let rec aux map =
    let next_iter nt pNT =
      let get_pmax rhs =
        let phogItem = TCOND.ContextMap.to_list contextMap in
        let probs = List.map (fun (_, pcfg) -> PCFG.rule_prob pcfg rhs) phogItem in
        let pInit = List.fold_left Float.max 0. probs in
        let product acc nt =
          let pNT' = Option.get_exn @@ Grammar.Map.get nt map in
          acc *. pNT' in
        let nextNTs = Grammar.Map.get_or ~default:[] rhs succMap in
        List.fold_left product pInit nextNTs in
      let pRHS = List.map get_pmax @@ Grammar.Map.get_or ~default:[] nt succMap in
      List.fold_left Float.max pNT pRHS in
    let map' = Grammar.Map.mapi next_iter map in
    if Grammar.Map.equal (fun a b -> abs_float (a -. b) <. 0.0001) map map'
    then Grammar.Map.map (fun v -> -. (log2 v)) map'
    else aux map' in
  List.map (function nt, _ -> nt, 0.) pcfgRaw
  |> Grammar.Map.of_list
  |> aux

let compute_heuristic_with_context succMap (basePCFGRaw, _, contextPCFG) heuristicMap =
  let open List in
  let ntContextMap =
    let* nt, _ = basePCFGRaw in
    let+ context, pcfg = ContextMap.to_list contextPCFG in
    let h_min acc rule =
      let init = PCFG.rule_cost pcfg rule in
      let nt's = Grammar.Map.get_or ~default:[] rule succMap in
      let sum_h acc nt' =
        let hNT' = Grammar.Map.get_or ~default:0.001 nt heuristicMap in
        hNT' +. acc in
      let hRule = List.fold_left sum_h init nt's in
      Float.min acc hRule in
    let rules = Option.get_exn @@ Grammar.Map.get nt succMap in
    let hMin = List.fold_left h_min Float.nan rules in
    (context, nt), hMin in
  TCOND.ContextRuleMap.of_list ntContextMap
