open Containers
open Fun
open Cmdliner

let baseDir = "/home/mac/Documents/code/cse291/"

type cmd = EnumPCFG | TrainPHOG | TrainPCFG | EnumPHOG | RunPCFG | RunPHOG | PrintHelp
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

module Synthesize (P : Sig.P) (Env : E) : R = struct
  let make_struct succMap ast_cost = (module struct
    let succMap = succMap
    let ast_cost = ast_cost
  end : Search.ENV)

  module V = AST.Value

  let eval ast = function
      [arg0] -> AST.eval ~arg0 ast
    | [arg0; arg1] -> AST.eval ~arg0 ~arg1 ast
    | [arg0; arg1; arg2] -> AST.eval ~arg0 ~arg1 ~arg2 ast
    | [arg0; arg1; arg2; arg3] -> AST.eval ~arg0 ~arg1 ~arg2 ~arg3 ast
    | _ -> raise (Invalid_argument "Not a supported arity")

  let verify ast (args, res) = V.equal (eval ast args) res

  let run n order =
    let open Result in
    let result =
      let specDir = baseDir ^ "benchmark/string/train/dr-name.sl" in
      let* fullSpec = Grammar.parse_spec specDir in
      let* constraints = Grammar.parse_constraints fullSpec in
      let cs =
        let open List.Infix in
        let+ args, res = constraints in
        List.map (fun arg -> V.Str arg) args, V.Str res in
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
      |> Seq.take_while (fun s -> not @@ List.for_all (verify s) cs)
      |> Seq.iteri (fun i ast ->
          Format.printf "%i: %a\n" (i + 1) AST.pp ast;
          List.iter (fun (args, res) ->
            let res' = eval ast args in
            let list_pp fmt = List.iter (fun s -> Format.fprintf fmt "%a " V.pp s) in
            Format.printf "Args: %a\t\nEval: %a\t\nExpected: %a\n\n" list_pp args V.pp res' V.pp res)
          cs;
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
  | RunPCFG ->
      let env = (module struct let tcondP = [] let name = "pcfg" end : E) in
      let (module S) = (module Synthesize (PCFG) (val env) : R) in
      S.run n order
  | RunPHOG ->
      let p = TCOND.[M Right; W WriteValue; M Up; W WriteValue] in
      let env = (module struct let tcondP = p let name = "phog" end : E) in
      let (module S) = (module Synthesize (PHOG) (val env) : R) in
      S.run n order
  | PrintHelp ->
      let msg = "A required argument is missing. "
        ^ "See the help page for more information." in
      print_endline msg

let man = [
  `S Manpage.s_arguments;
  `Pre "train-pcfg\tTrain PCFG from the data."; `Noblank;
  `Pre "train-phog\tTrain PHOG using the given TCOND program."; `Noblank;
  `Pre "enum-pcfg\t\tEnumerate candidates with the trained PCFG model."; `Noblank;
  `Pre "enum-phog\t\tEnumerate candidates with the trained PHOG model."; `Noblank;
  `Pre "run-pcfg\t\tRun a simple PBE synthesizer on a given spec with the trained PCFG model."; `Noblank;
  `Pre "run-phog\t\tRun a simple PBE synthesizer on a given spec with the trained PHOG model.";
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
              "enum-pcfg", EnumPCFG; "enum-phog", EnumPHOG;
              "run-pcfg", RunPCFG; "run-phog", RunPHOG] in
  Arg.(value @@ pos 0 (enum cmds) PrintHelp @@ info [])

let () =
  let main = Term.(const exe $ nEnum $ order $ runMode) in
  Term.exit @@ Term.eval (main, info)
