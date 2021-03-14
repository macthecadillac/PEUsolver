open Containers
open Fun
open Cmdliner

let baseDir = "/home/mac/Documents/code/cse291/"

type search_order = AS | FAS
type cmd = RunPCFG | TrainPHOG | RunPHOG | PrintHelp

let error_pp fmt (`Msg s) = Format.fprintf fmt "%s" s

let print_result =
  let pp fmt = Result.iter_err (Format.fprintf fmt "%a\n" error_pp) in
  Format.printf "%a\n" pp

let make_struct successorsMap tcond_program prob = (module struct
  let successorsMap = successorsMap
  let tcond_program = tcond_program
  let prob = prob
end : Search.ENV)

let run_pcfg () =
  let open Result in
  let result =
    let specDir = baseDir ^ "euphony/benchmarks/string/train/dr-name.sl" in
    let* fullSpec = Grammar.parse_spec specDir in
    let* ntMap = Grammar.rule_nttype_map fullSpec in
    let* json = JSON.parse @@ baseDir ^ "benchmark/pcfg.json" in
    let* pcfg_raw = PCFG.decode json in
    let pcfg = PCFG.compile ntMap pcfg_raw in
    let+ successorsMap = Grammar.succession_map fullSpec in
    let (module E) = make_struct successorsMap [] @@ `PCFG pcfg in
    let (module O) = (module SearchOrder.FAStar (E) : Search.PATHORDER) in
    let (module S) = (module Search.Make (E) (SearchOrder.AStar) : Search.S) in
    S.sequence
    |> Seq.map (fun s -> PCFG.ast_cost pcfg s, s)
    |> Seq.take 200
    |> Seq.iter (fun (a, b) -> Format.printf "%f\t%a\n" a Grammar.ast_pp b) in
  print_result result

let train_phog () =
  let open Result in
  let p = TCOND.[M Left; W WriteValue; M Up; W WriteValue] in
  let result =
    let* path = Fpath.of_string @@ baseDir ^ "euphony/benchmarks/string/train" in
    let* rawSpecs = List.map Fpath.to_string <$> Bos.OS.Dir.contents path in
    let* fullSpecs = List.map Grammar.parse_spec rawSpecs |> flatten_l in
    let* ntMaps = List.map Grammar.rule_nttype_map fullSpecs |> flatten_l in
    let* ntMap =
      List.fold_left
      (fun acc m -> acc >>= Grammar.merge_rule_nttype_maps m)
      (Ok Grammar.Map.empty) ntMaps in
    let* asts = List.map Grammar.build_solution_ast fullSpecs |> flatten_l in
    Format.printf "ntMap:\n%a\n" (Grammar.Map.pp Grammar.pp Grammar.pp) ntMap;
    let phog_raw = PHOG.train ntMap p asts in
    let phog_json = PHOG.encode phog_raw in
    let+ _ = JSON.save phog_json @@ baseDir ^ "benchmark/phog.json" in
    let phog = PHOG.compile ntMap phog_raw in
    Format.printf "%a\n" PHOG.pp phog;
    in
  print_result result

let exe = function
    RunPCFG -> run_pcfg ()
  | TrainPHOG -> train_phog ()
  | PrintHelp ->
      let msg = "A required argument is missing. "
        ^ "See the help page for more information." in
      print_endline msg
  | _ -> ()

let man = [
  `S Manpage.s_arguments;
  `Pre "train-phog\tTrain PHOG using the given TCOND program."; `Noblank;
  `Pre "run-pcfg\t\tRun the enumerator with the default PCFG."; `Noblank;
  `Pre "run-phog\t\tRun the enumerator with the trained PHOG.";
]

let info =
  let doc = "A proof-of-concept implementation of A* and FA* with PCFG and PHOG." in
  Term.info "search" ~doc ~man

let run =
  let cmds = ["train-phog", TrainPHOG; "run-pcfg", RunPCFG; "run-phog", RunPHOG] in
  Arg.(value @@ pos 0 (enum cmds) PrintHelp @@ info [])

let () =
  let main = Term.(const exe $ run) in
  Term.exit @@ Term.eval (main, info)
