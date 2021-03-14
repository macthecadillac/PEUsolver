open Containers
open Fun
open Bos.OS

type 'a printer = Format.formatter -> 'a -> unit

type count = { wordCount : int; vocab : int }

let count_pp fmt c =
  Format.fprintf fmt "{ wordCount : %i; vocab : %i }" c.wordCount c.vocab

type t = { counts : count Grammar.Map.t;
           (* map the rules to the non-terminal type they belong *)
           ntMap : Grammar.t Grammar.Map.t;
           probMap : float Grammar.Map.t }

let pp fmt t =
  Format.fprintf fmt "Counts: %a\nprobMap: %a\nntMap: %a"
  (Grammar.Map.pp Grammar.pp count_pp) t.counts
  (Grammar.Map.pp Grammar.pp Float.pp) t.probMap
  (Grammar.Map.pp Grammar.pp Grammar.pp) t.ntMap

type raw_t = (Grammar.t * (Grammar.t * int) list) list

let decode json =
  let open Result.Infix in
  let aux f g l =
    let open Result in
    List.map (fun (s, l') -> let+ r = g l' in f s, r) l
    |> flatten_l in
  let* l = JSON.read_assoc json in
  let* termTypes = List.assoc_opt ~eq:String.equal "word-count" l
    |> Option.to_result (`Msg "Malformed PCFG config") in
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
    nonterm, cnt in
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
    Grammar.Map.of_list assoc' in
  let counts = List.combine ntWordCount ntVocab
    |> List.map (function (s, wordCount), (_, vocab) -> s, { wordCount; vocab })
    |> Grammar.Map.of_list in
  { counts; probMap; ntMap }

let rule_cost pcfg rule =
  let open Option.Infix in
  let costOpt =
    let+ ntType = Grammar.Map.get rule pcfg.ntMap in
    (* the non-terminal type must exist in the map *)
    let count = Option.get_exn @@ Grammar.Map.get ntType pcfg.counts in
    let default = Float.(1. /. (of_int count.vocab +. of_int count.wordCount)) in
    (* let default = 0.001 in *)
    Option.get_or ~default (Grammar.Map.get rule pcfg.probMap) in
  (* if a "rule" isn't found in ntMap, it is a hole which should have a cost of 0 *)
  Option.get_or ~default:0. costOpt

let ast_cost pcfg t =
  let aux = function
      s, [] -> rule_cost pcfg s (* terminals *)
    | s, l -> List.fold_left (+.) (rule_cost pcfg s) l in
  Tree.fold (curry aux) t
