open Containers
open Fun

let error_pp fmt (`Msg s) = Format.fprintf fmt "%s" s

let print_result =
  let pp fmt = Result.iter_err (Format.fprintf fmt "%a\n" error_pp) in
  Format.printf "%a\n" pp

let make_struct successorsMap pcfg = (module struct
  let successorsMap = successorsMap
  let pcfg = pcfg
end : Search.ENV)

let () =
  let open Result in
  let result =
    let* fullSpec = Grammar.parse_spec
      "/home/mac/Documents/code/cse291/euphony/benchmarks/string/train/dr-name.sl" in
    let* pcfg =
      PCFG.parse_pcfg_config "/home/mac/Documents/code/cse291/benchmark/pcfg.json"
      >>= PCFG.build_cost_map in
    let* grammarSpec = Grammar.filter_grammar_spec fullSpec in
    let+ successorsMap =
      List.fold_left
        (fun acc s -> acc >>= Grammar.build_succession_map s)
        (Ok Grammar.Map.empty)
        grammarSpec in
    let (module E) = (make_struct successorsMap pcfg) in
    let (module O) = (module SearchOrder.FAStar (E) : Search.PATHORDER) in
    let (module S) = (module Search.Make (E) (O) : Search.S) in
    S.sequence
    |> Seq.map (fun s -> Grammar.ast_cost pcfg s, Grammar.ast_to_string s)
    |> Seq.take 200
    |> Seq.iter (fun (a, b) -> Printf.printf "%f\t%s\n" a b) in
  print_result result
