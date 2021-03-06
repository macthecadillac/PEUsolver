open Containers
open Fun

let error_pp fmt (`Msg s) = Format.fprintf fmt "%s" s

let print_result =
  let pp fmt = Result.iter_err (Format.fprintf fmt "%a\n" error_pp) in
  Format.printf "%a\n" pp

let make_struct grammar pcfg = (module struct
  let grammar = grammar
  let pcfg = pcfg
end : AStar.ENV)

let () =
  let open Result in
  let result =
    let* spec = Grammar.parse_spec "/home/mac/Documents/code/cse291/dr-name.sl" in
    let* pcfg =
      PCFG.parse_pcfg_config "/home/mac/Documents/code/cse291/benchmark/pcfg.json"
      >>= PCFG.build_cost_map in
    let* def = Grammar.grammar spec in
    let+ grammar =
      List.fold_left
        (fun acc s -> acc >>= Grammar.build_succession_map s)
        (Ok Grammar.Map.empty)
        def in
    let (module H) = (module AStar.Make (val make_struct grammar pcfg) : AStar.S) in
    H.sequence
    |> Seq.map Grammar.ast_to_string
    |> Seq.take 200
    |> Seq.iter print_endline in
  print_result result
