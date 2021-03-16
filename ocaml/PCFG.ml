open Containers
open Fun
open Bos.OS

module M = Grammar.Map

type 'a printer = Format.formatter -> 'a -> unit

type count = { wordCount : int; vocab : int }

let count_pp fmt c =
  Format.fprintf fmt "{ wordCount : %i; vocab : %i }" c.wordCount c.vocab

type raw_t = (Grammar.t * (Grammar.t * int) list) list

type t = { counts : count M.t;
           raw : raw_t;
           (* map the rules to the non-terminal type they belong *)
           ntMap : Grammar.t M.t;
           probMap : float M.t }

let logBase2 = log 2.
let log2 a = log a  /. logBase2

let pp fmt t =
  Format.fprintf fmt "Counts: %a\nprobMap: %a\nntMap: %a"
  (M.pp Grammar.pp count_pp) t.counts
  (M.pp Grammar.pp Float.pp) t.probMap
  (M.pp Grammar.pp Grammar.pp) t.ntMap

let count ntMap rules =
  let open List.Infix in
  let aux acc rule =
    let ntType = Option.get_exn @@ M.get rule ntMap in
    let open Option in
    let inc_opt = (fold (fun acc a -> (+) a <$> acc) (pure 1)) in
    let update_w_default = fold (fun _ a -> Some (M.update rule inc_opt a))
      @@ Some (M.singleton rule 1) in
    M.update ntType update_w_default acc in
  let open List.Infix in
  let+ ntType, inner = List.fold_left aux M.empty rules |> M.to_list in
  let assocs =
    let+ rule, count = M.to_list inner in
    rule, count in
  ntType, assocs

let train ntMap _ asts =
  let open List.Infix in
  let rules =
    let* ast = asts in
    Tree.flatten ast in
  count ntMap rules

let decode json =
  let open Result.Infix in
  let aux f g l =
    let open Result in
    List.map (fun (s, l') -> let+ r = g l' in f s, r) l
    |> flatten_l in
  let* l = JSON.read_assoc json in
  let* termTypes = List.assoc_opt ~eq:String.equal "word-count" l
    |> Option.to_result (`Msg "PCFG.decode: malformed config") in
  let* assoc = JSON.read_assoc termTypes in
  aux Grammar.of_string JSON.read_assoc assoc
  >>= aux id (aux Grammar.of_string JSON.read_int)

let encode raw =
  let open List.Infix in
  let ntAssoc =
    let+ nt, l = raw in
    let ruleAssoc =
      let+ rule, count = l in
      Grammar.to_string rule, JSON.of_int count in
    Grammar.to_string nt, JSON.of_assoc ruleAssoc in
  let wordCount = JSON.of_assoc ntAssoc in
  let jsonAssoc = ["word-count", wordCount] in
  JSON.of_assoc jsonAssoc

let compile ntMap assocs =
  let count_inner f a =
    let open List.Infix in
    let+ nonterm, innerAssoc = a in
    let cnt = f innerAssoc in
    nonterm, cnt + 1 in  (* FIXME: temp fix *)
  let ntWordCount = count_inner List.(fold_left (+) 0 % map snd) assocs in
  let ntVocab = count_inner List.length assocs in
  let probMap =
    let open List.Infix in
    let assoc' =
      let* (_, v), ((_, l), (_, totCnt)) =
        List.combine ntVocab @@ List.combine assocs ntWordCount in
      let+ rule, cnt = l in
      rule, Float.((1. +. of_int cnt) /. (of_int v +. of_int totCnt)) in  (* +1 smoothing *)
      (* rule, if totCnt = 0 then 0. else Float.(of_int cnt /. of_int totCnt) in *)
    M.of_list assoc' in
  let counts = List.combine ntWordCount ntVocab
    |> List.map (function (s, wordCount), (_, vocab) -> s, { wordCount; vocab })
    |> M.of_list in
  { raw = assocs; counts; probMap; ntMap }

let rule_prob pcfg rule =
  let open Option.Infix in
  let costOpt =
    let+ ntType = M.get rule pcfg.ntMap in
    let default = { wordCount = 5000; vocab = 5000 } in
    let count = Option.get_or ~default @@ M.get ntType pcfg.counts in
    let default = Float.(1. /. (of_int count.vocab +. of_int count.wordCount)) in
    (* let default = 0.001 in *)
    Option.get_or ~default (M.get rule pcfg.probMap) in
  (* if a "rule" isn't found in ntMap, it is a hole which should have a cost of 0 *)
  Option.get_or ~default:0.0001 costOpt

let rule_cost pcfg rule = -. log2 (rule_prob pcfg rule)

let ast_cost pcfg _ t =
  let aux = function
      s, [] -> rule_cost pcfg s (* terminals *)
    | s, l -> List.fold_left (+.) (rule_cost pcfg s) l in
  Tree.fold (curry aux) t

let compute_heuristic _ _ = Grammar.Map.empty

let compute_heuristic_with_context _ _ _ = TCOND.ContextRuleMap.empty
