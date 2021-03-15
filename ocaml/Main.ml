open Containers
open Fun
open Cmdliner

let baseDir = "/home/mac/Documents/code/cse291/"

type cmd = EnumPCFG | TrainPHOG | TrainPCFG | EnumPHOG | PrintHelp
type mode = PCFG | PHOG
type order = AS | FAS

let error_pp fmt (`Msg s) = Format.fprintf fmt "%s" s

let print_result =
  let pp fmt = Result.iter_err (Format.fprintf fmt "%a\n" error_pp) in
  Format.printf "%a\n" pp

module type E = sig
  val name : string
  val tcondP : TCOND.p
end

module type R = sig val run : int -> order -> unit end

module Enumerate (P : Sig.P) (Env : E) : R = struct
  let make_struct succMap ast_cost = (module struct
    let succMap = succMap
    let ast_cost = ast_cost
  end : Search.ENV)

  let run n order =
    let open Result in
    let result =
      let specDir = baseDir ^ "benchmark/string/train/dr-name.sl" in
      let* fullSpec = Grammar.parse_spec specDir in
      let* succMap = Grammar.succession_map fullSpec in
      let* ntMap = Grammar.rule_nttype_map fullSpec in
      let* json = JSON.parse @@ Printf.sprintf "%sbenchmark/%s.json" baseDir Env.name in
      let+ p_raw = P.decode json in
      let p = P.compile ntMap p_raw in
      let ast_cost = P.ast_cost p Env.tcondP in
      let env = make_struct succMap ast_cost in
      let orderM =
        match order with
          AS -> (module SearchOrder.AStar : Search.PATHORDER)
        | FAS -> (module SearchOrder.FAStar (val env) : Search.PATHORDER) in
      let (module S) = (module Search.Make (val env) (val orderM) : Search.S) in
      S.sequence
      |> Seq.map (fun s -> ast_cost s, s)
      |> Seq.take n
      |> Seq.iter (fun (a, b) ->
          Format.printf "%f\t%a\n" a AST.pp b;
          Format.print_flush ()) in
    print_result result
end

module type T = sig val run : unit -> unit end

module Train (P : Sig.P) (Env: E) : T = struct
  let run () =
    let open Result in
    let result =
      let* path = Fpath.of_string @@ baseDir ^ "benchmark/string/train" in
      let* rawSpecs = List.map Fpath.to_string <$> Bos.OS.Dir.contents path in
      let* specs = List.map Grammar.parse_spec rawSpecs |> flatten_l in
      let* ntMaps = List.map Grammar.rule_nttype_map specs |> flatten_l in
      let* ntMap =
        List.fold_left
        (fun acc m -> acc >>= Grammar.merge_rule_nttype_maps m)
        (Ok Grammar.Map.empty) ntMaps in
      let* asts = List.map (Grammar.build_solution_ast ntMap) specs |> flatten_l in
      let raw = P.train ntMap Env.tcondP asts in
      let json = P.encode raw in
      JSON.save json @@ Printf.sprintf "%sbenchmark/%s.json" baseDir Env.name in
    print_result result
end

let exe n order = function
    EnumPCFG ->
      let env = (module struct let tcondP = [] let name = "pcfg" end : E) in
      let (module E) = (module Enumerate (PCFG) (val env) : R) in
      E.run n order
  | EnumPHOG ->
      let p = TCOND.[M Right; W WriteValue; M Up; W WriteValue] in
      let env = (module struct let tcondP = p let name = "phog" end : E) in
      let (module E) = (module Enumerate (PHOG) (val env) : R) in
      E.run n order
  | TrainPCFG ->
      let env = (module struct let tcondP = [] let name = "pcfg" end : E) in
      let (module T) = (module Train (PCFG) (val env) : T) in
      T.run ()
  | TrainPHOG ->
      let p = TCOND.[M Right; W WriteValue; M Up; W WriteValue] in
      let env = (module struct let tcondP = p let name = "phog" end : E) in
      let (module T) = (module Train (PHOG) (val env) : T) in
      T.run ()
  | PrintHelp ->
      let msg = "A required argument is missing. "
        ^ "See the help page for more information." in
      print_endline msg

let man = [
  `S Manpage.s_arguments;
  `Pre "train-phog\tTrain PHOG using the given TCOND program."; `Noblank;
  `Pre "run-pcfg\t\tRun the enumerator with the default PCFG."; `Noblank;
  `Pre "run-phog\t\tRun the enumerator with the trained PHOG.";
]

let info =
  let doc = "A proof-of-concept implementation of A* and FA* with PCFG and PHOG." in
  Term.info "search" ~doc ~man

let nEnum =
  let doc = "How many sentential forms should the program enumerate." in
  Arg.(value @@ opt int 20 @@ info ["n"; "enum"] ~doc)

let order =
  let doc = "Choose between `astar' and `fastar'." in
  let var = ["astar", AS; "fastar", FAS] in
  Arg.(value @@ opt (enum var) AS @@ info ["o"; "order"] ~doc)

let runMode =
  let cmds = ["train-phog", TrainPHOG; "train-pcfg", TrainPCFG;
              "enum-pcfg", EnumPCFG; "enum-phog", EnumPHOG] in
  Arg.(value @@ pos 0 (enum cmds) PrintHelp @@ info [])

let () =
  let main = Term.(const exe $ nEnum $ order $ runMode) in
  Term.exit @@ Term.eval (main, info)
