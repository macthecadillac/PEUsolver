open Containers
open Fun
open Cmdliner
open Bos

module List = struct
  include List
  let pp pp_item =
    let f s fmt () = Format.fprintf fmt "%s" s in
    pp ~pp_sep:(f "; ") ~pp_start:(f "[") ~pp_stop:(f "]") pp_item
end

let log2 a = log a /. log 2.

let baseDir = Result.(Fpath.parent <$> OS.Dir.current ())

type cmd = TrainPHOG | TrainTCOND | Enumerate | Run | PrintHelp

let error_pp fmt (`Msg s) = Format.fprintf fmt "%s" s

let print_result =
  let pp fmt = Result.iter_err (Format.fprintf fmt "%a\n" error_pp) in
  Format.printf "%a\n" pp

type order = [`HMax | `HMin | `LeftFirst | `RightFirst]

let make_struct order succMap ast_cost heuristics heuristics_w_context = (module struct
  let order = order
  let succMap = succMap
  let ast_cost = ast_cost
  let heuristics = heuristics
  let heuristics_w_context = heuristics_w_context
end : Search.ENV)

let eval ast = function
    [arg0] -> AST.eval ~arg0 ast
  | [arg0; arg1] -> AST.eval ~arg0 ~arg1 ast
  | [arg0; arg1; arg2] -> AST.eval ~arg0 ~arg1 ~arg2 ast
  | [arg0; arg1; arg2; arg3] -> AST.eval ~arg0 ~arg1 ~arg2 ~arg3 ast
  | _ -> raise (Invalid_argument "Not a supported arity")

let verify ast (args, res) = Value.equal (eval ast args) res

let constraint_pp fmt (args, res) =
  Format.fprintf fmt "%a -> %a" (List.pp Value.pp) args Value.pp res

let sequence order specFileName tcondP =
  let open Result.Infix in
  if String.equal specFileName "" then Error (`Msg "file name cannot be blank")
  else
    let* projBase = baseDir in
    let specPath = Fpath.(projBase / "benchmark" / "string" / "test" / specFileName) in
    let* fullSpec = Grammar.parse_spec specPath in
    let* constraints = Grammar.parse_constraints fullSpec in
    let* succMap = Grammar.succession_map fullSpec in
    let* ntMap = Grammar.rule_nttype_map fullSpec in
    let jsonPath = Fpath.(projBase / "benchmark" / "phog.json") in
    let* json = JSON.parse jsonPath in
    let+ p_raw = PHOG.decode json in
    let p = PHOG.compile ntMap p_raw in
    let ast_cost = PHOG.ast_cost p tcondP in
    let heuristicsMap = PHOG.compute_heuristic succMap p in
    let heuristicsWCtxt = PHOG.compute_heuristic_with_context succMap p heuristicsMap in
    let heuristics nt = Grammar.Map.get_or ~default:(-. log2 0.001) nt heuristicsMap in
    let heuristics_w_context loc ast =
      let pair = TCOND.apply loc ast tcondP in
      TCOND.ContextRuleMap.get_or ~default:0.001 (Pair.swap pair) heuristicsWCtxt in
    let env = make_struct order succMap ast_cost heuristics heuristics_w_context in
    let (module S) = (module Search.Make (val env) : Search.S) in
    Seq.map (fun s -> ast_cost, constraints, s) S.sequence |> Seq.mapi Pair.make

let run order specFileName tcondP =
  let open Result.Infix in
  let result =
    let+ seq = sequence order specFileName tcondP in
    Seq.take_while (fun (i, (_, constraints, ast)) ->
        let err, pred =
          try false, List.for_all (verify ast) constraints with
            AST.EvalError s ->
              Format.printf "\t%a\n\t%s\n" AST.pp ast s;
              true, false
        in
        if pred then begin
          Format.printf "\nSolution found: %a\n" AST.pp ast;
          Format.printf "  Constraints: %a\n" (List.pp constraint_pp) constraints;
          Format.printf "     AST eval: %a\n"
            (List.pp constraint_pp)
            (List.map (fun (args, res) -> args, eval ast args) constraints);
          not err && false
        end
        else not err && true) seq
    |> Seq.iteri (fun i ast ->
        Format.printf "\rIteration: %i" (i + 1);
        Format.print_flush ()) in
  print_result result

let enumerate n order specFileName tcondP =
  let open Result.Infix in
  let result =
    let+ seq = sequence order specFileName tcondP in
    Seq.take n seq
    |> Seq.iter (fun (i, (f, _, a)) ->
        let cost = f a in
        Format.printf "%i: %f\t%a\n" i cost AST.pp a;
        Format.print_flush ()) in
  print_result result

let train_phog tcondP =
  print_endline "Training PHOG...";
  let open Result in
  let result =
    let* projBase = baseDir in
    let path = Fpath.(projBase / "benchmark" / "string" / "train") in
    let* rawSpecs = Bos.OS.Dir.contents path in
    let* specs = List.map Grammar.parse_spec rawSpecs |> flatten_l in
    let* ntMaps = List.map Grammar.rule_nttype_map specs |> flatten_l in
    let* ntMap =
      List.fold_left
      (fun acc m -> acc >>= Grammar.merge_rule_nttype_maps m)
      (Ok Grammar.Map.empty) ntMaps in
    let* asts = List.map (Grammar.build_solution_ast ntMap) specs |> flatten_l in
    let raw = PHOG.train ntMap tcondP asts in
    let json = PHOG.encode raw in
    let jsonPath = Fpath.(projBase / "benchmark" / "phog.json") in
    let+ () = JSON.save json jsonPath in
    print_endline "Done." in
  print_result result

let train_tcond maxIter =
  let open Result in
  let result =
    let* projBase = baseDir in
    let path = Fpath.(projBase / "benchmark" / "string" / "train") in
    let* rawSpecs = Bos.OS.Dir.contents path in
    let* specs = List.map Grammar.parse_spec rawSpecs |> flatten_l in
    let* ntMaps = List.map Grammar.rule_nttype_map specs |> flatten_l in
    let* ntMap =
      List.fold_left
      (fun acc m -> acc >>= Grammar.merge_rule_nttype_maps m)
      (Ok Grammar.Map.empty) ntMaps in
    let* asts = List.map (Grammar.build_solution_ast ntMap) specs |> flatten_l in
    let tcondP = TCOND.train asts 0.18 maxIter in  (** hard code lambda *)
    let path = Fpath.(projBase / "benchmark" / "TCOND") in
    Format.printf "Done.";
    TCOND.save tcondP path in
  print_result result

let exe n order spec maxIter operation =
  let open Result.Infix in
  let result =
    let+ tcondP =
      let open Result.Infix in
      let baseP = Result.get_exn baseDir in
      let path = Fpath.(baseP / "benchmark" / "TCOND") in
      TCOND.load path in
    match operation with
      TrainPHOG -> train_phog tcondP
    | TrainTCOND -> train_tcond maxIter
    | Enumerate -> enumerate n order spec tcondP
    | Run -> run order spec tcondP
    | PrintHelp ->
        let msg = "A required argument is missing. "
          ^ "See the help page for more information." in
        print_endline msg in
  print_result result

let man = [
  `S Manpage.s_arguments;
  `Pre "train-phog\tTrain PHOG using the given TCOND program."; `Noblank;
  `Pre "enumerate\t\tEnumerate candidates with the trained PHOG model."; `Noblank;
  `Pre "run\t\tRun a simple PBE synthesizer on a given spec with the trained PHOG model."; `Noblank;
  `Pre "train-tcond\t\tTrain the TCOND program.";
]

let info =
  let doc = "A proof-of-concept implementation of A* and FA* with PHOG." in
  Term.info "search" ~doc ~man

let nEnum =
  let doc = "How many sentential forms should the program enumerate." in
  Arg.(value @@ opt int 20 @@ info ["n"; "enum"] ~doc)

let order =
  let doc = "Choose between `astar' and `fastar'." in
  let var = ["left-first", `LeftFirst; "right-first", `RightFirst;
             "h-max", `HMax; "h-min", `HMin] in
  Arg.(value @@ opt (enum var) `LeftFirst @@ info ["o"; "order"] ~doc)

let spec =
  let doc = "The name of the spec file." in
  Arg.(value @@ opt string "" @@ info ["f"; "spec"] ~doc)

let maxIter =
  let doc = "The maximum number of iterations during TCOND training." in
  Arg.(value @@ opt int 20000 @@ info ["i"; "maxiter"] ~doc)

let runMode =
  let cmds = ["train-phog", TrainPHOG; "enumerate", Enumerate;
              "run", Run; "train-tcond", TrainTCOND] in
  Arg.(value @@ pos 0 (enum cmds) PrintHelp @@ info [])

let () =
  let main = Term.(const exe $ nEnum $ order $ spec $ maxIter $ runMode) in
  Term.exit @@ Term.eval (main, info)
