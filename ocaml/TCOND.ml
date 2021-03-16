open Containers
open Fun

module R = Random

module List = struct
  include List
  let map_nth_opt f n l =
    let rec aux m acc = function
        [] -> rev acc
      | hd::tl when m = n -> Option.fold (flip cons) tl (f hd)
      | hd::tl -> aux (m + 1) (hd::acc) tl in
    aux 0 [] l
end

module Context = struct
  type t = Grammar.t list

  let compare = List.compare Grammar.compare

  let pp =
    let f s fmt () = Format.fprintf fmt "%s" s in
    List.pp ~pp_sep:(f "; ") ~pp_start:(f "[") ~pp_stop:(f "]") Grammar.pp

  let to_string a = String.concat "\t" (List.map Grammar.to_string a)

  let of_string s = List.map Grammar.of_string @@ String.split_on_char '\t' s
end

module ContextRule = struct
  type t = Context.t * Grammar.t
  let compare = Pair.compare Context.compare Grammar.compare
end

module ContextMap = Map.Make (Context)
module ContextSet = Set.Make (Context)
module ContextRuleMap = Map.Make (ContextRule)
module ContextRuleSet = Set.Make (ContextRule)

type move =
    Up
  | Left
  | Right
  | DownFirst
  | DownLast
  | PrevDFS

type write = WriteValue

type tcond = W of write | M of move

type p = tcond list

let tcond_to_string = function
    W WriteValue -> "WriteValue"
  | M Up -> "Up"
  | M Left -> "Left"
  | M Right -> "Right"
  | M DownFirst -> "DownFirst"
  | M DownLast -> "DownLast"
  | M PrevDFS -> "PrevDFS"

let tcond_of_string = function
    "WriteValue" -> W WriteValue
  | "Up" -> M Up
  | "Left" -> M Left
  | "Right" -> M Right
  | "DownFirst" -> M DownFirst
  | "DownLast" -> M DownLast
  | "PrevDFS" -> M PrevDFS
  | _ -> raise (Invalid_argument "TCOND.tcond_of_string: unrecognized operator")

type 'a printer = Format.formatter -> 'a -> unit

let pp fmt tcond = Format.fprintf fmt "%s" @@ tcond_to_string tcond

let pp' =
  let f s fmt () = Format.fprintf fmt "%s" s in
  List.pp ~pp_sep:(f "; ") ~pp_start:(f "[") ~pp_stop:(f "]") pp

let p_to_string =
  let f s fmt () = Format.fprintf fmt "%s" s in
  let pp = List.pp ~pp_sep:(f " ") ~pp_start:(f "") ~pp_stop:(f "") pp in
  Format.sprintf "%a" pp

let p_of_string = String.trim %> String.split_on_char ' ' %> List.map tcond_of_string

let save tcondP fpath = Bos.OS.File.write fpath (p_to_string tcondP)

let load fpath =
  let open Result in
  let+ str = Bos.OS.File.read fpath in
  p_of_string str

let is_write = function W _ -> true | M _ -> false

let moveOps = [M Up; M Left; M Right; M DownFirst; M DownLast; M PrevDFS]

let writeOps = [W WriteValue]

let firstOp = [M Up; M Left; M Right; M PrevDFS]

let allOps = moveOps @ writeOps

let compact p =
  let rec aux dummyCtxt acc p =
    match acc, p with
      [], l -> dummyCtxt, l
    | _, [] -> dummyCtxt, []
    | l, W _::ps -> aux (Grammar.of_string "\\0"::dummyCtxt) l ps
    | M Left::xs, ((M Up::_) as ps)
    | M Right::xs, ((M Up::_) as ps)
    | M Left::xs, M Right::ps
    | M Right::xs, M Left::ps
    | M DownFirst::xs, M Up::ps
    | M DownLast::xs, M Up::ps -> aux dummyCtxt xs ps
    | xs, M PrevDFS::ps -> aux dummyCtxt xs (M Up::ps)
    | l, p::ps -> aux dummyCtxt (p::l) ps
  in match p with
    [] -> [], []
  | op::rest -> aux [] [op] rest

let apply loc ast p : Grammar.t * Context.t =
  let rec aux acc i (Tree.Node (_, l) as node) child p dir =
    match move acc i child p `None with
      `Done _ as c -> c
    | `Continue (`Up, acc', p') ->
        move acc' i node p' `None
    | `Continue (`Left, acc', p') ->
        if i <= 0
        then
          let skipped, p'' = compact (M Left::p') in
          aux (List.rev_append skipped acc') i node (List.nth l i) p'' `None
        else begin
          aux acc' (i - 1) node (List.nth l (i - 1)) p' `None
        end
    | `Continue (`Right, acc', p') ->
        if i >= List.length l - 1
        then
          let skipped, p'' = compact (M Right::p') in
          aux (List.rev_append skipped acc') i node (List.nth l i) p'' `None
        else aux acc' (i + 1) node (List.nth l (i + 1)) p' `None
    | `Continue (`PrevDFS, acc', p') ->
        if i <= 0
        then `Continue (`Up, acc', p')
        else aux acc' (i - 1) node (List.nth l (i - 1)) p' `Child
  and move acc i (Tree.Node (a, l) as node) p dir =
    match p, dir with
      [], `None -> `Done acc
    | M Up::tl, `None -> `Continue (`Up, acc, tl)
    | M Left::tl, `None -> `Continue (`Left, acc, tl)
    | M Right::tl, `None -> `Continue (`Right, acc, tl)
    | M PrevDFS::tl, `None -> `Continue (`PrevDFS, acc, tl)
    | W WriteValue::tl, `None -> move (a::acc) i node tl `None
    | M DownFirst::tl, `None -> begin
        match List.head_opt l with
          Some h -> aux acc 0 node h tl `None
        | None ->
            let skipped, p = compact tl in
            move (List.rev_append skipped acc) i node p `None
      end
    | M DownLast::tl, `None -> begin
        match List.last_opt l with
          Some h -> aux acc (List.length l - 1) node h tl `None
        | None ->
            let skipped, p = compact tl in
            move (List.rev_append skipped acc) i node p `None
      end
    | _, `Child -> begin
        match List.last_opt l with
          None -> move acc i node p `None
        | Some last -> move acc (List.length l - 1) last p `Child
      end
  in
  let rec main l i ast p dir =
    let finalize acc =
      let rev'd = List.rev acc in
      let h = List.hd rev'd in
      h, List.rev @@ List.tl rev'd in
    match move l i ast p dir with
      `Continue (`Up, acc, tl) | `Continue (`PrevDFS, acc, tl) -> begin
        let skipped, p' = compact tl in
        match p' with
          M DownLast::tl' | M DownFirst::tl' -> 
            main (List.rev_append skipped acc) 0 ast tl' `None
        | _ -> finalize acc
      end
    | `Done (_::_ as acc) | `Continue (_, acc, _) -> finalize acc
    | `Done [] -> raise (Invalid_argument "Check code") in
  (* this is sort of a hack--the first item is the "origin" of the set of operations *)
  main [] 0 ast (loc @ [W WriteValue] @ p) `None

let delete = function
    [] | [_] | [_; _] -> None
  | chunk -> let n = R.run (R.int_range 1 (List.length chunk - 2)) in
             Some (List.remove_at_idx n chunk)

let modify = function
    [] | [_] as a -> Some a
  | _::([_] as w) -> Some (R.run (R.pick_list firstOp)::w)
  | chunk ->
      let len = List.length chunk in
      let op = R.run (R.pick_list moveOps) in
      if len <= 2 then
        Some (List.map_nth_opt (const (Some op)) 1 chunk)
      else
        let n = R.run (R.int_range 1 (List.length chunk - 2)) in
        Some (List.map_nth_opt (const (Some op)) n chunk)

let add chunk =
  let len = List.length chunk in
  let op = R.run (R.pick_list moveOps) in
  if len <= 2 then
    Some (List.map_nth_opt (const (Some op)) 1 chunk)
  else
    let n = R.run (R.int_range 1 (List.length chunk - 2)) in
    Some (List.map_nth_opt (const (Some op)) n chunk)

let append =
  let op = R.pick_list firstOp in
  let write = R.pick_list writeOps in
  R.list_seq [op; write]

let mutate p =
  let open Random in
  let partition =
    let rec aux acc1 acc2 = function
        [] -> acc2
      | (W _ as w)::tl -> aux [] (List.rev (w::acc1)::acc2) tl
      | hd::tl -> aux (hd::acc1) acc2 tl in
    aux [] [] in
  let chunks = partition p in
  let ops =
    if List.length p <= 2 then [`I add; `A append]
    else if List.length chunks <= 1 then [`I modify; `I add; `A append]
    else [`I delete; `I modify; `I add; `A append] in
  let* op = pick_list ops in
  match op with
    `A l -> map (List.append p) l
  | `I f ->
      let len = List.length chunks in
      if len <= 1 then return (List.flatten chunks)
      else
        let+ n = int len in
        List.map_nth_opt f n chunks |> List.flatten

let initPool =
  let aux l : p * p list =
    let p = R.run (R.pick_list l) in
    let p' = R.run (mutate p) in
    p', p'::l in
  Seq.unfold (Option.pure % aux) [firstOp]

(* enumerate all the nodes of the AST using the move operators *)
let enumerate_nodes_with_move_ops =
  let rec aux = function
    Tree.Node (_, []) -> [[]]
  | Tree.Node (_, ns) ->
      let open List in
      let locs =
        let* i, node = mapi Pair.make ns in
        let+ loc = aux node in
        (M DownFirst::rev_append (replicate i (M Right)) loc) in
      []::locs
  in List.rev % aux

let enumerate_context_rule_pairs tcondP =
  let open List in
  fold_left
  (fun acc ast ->
    let contexts =
      let+ loc = rev @@ enumerate_nodes_with_move_ops ast in
      let h, l = apply loc ast tcondP in
      l, h in
    rev_append contexts acc)
  []

let generate_probability_map tcondP asts =
  let pairs = enumerate_context_rule_pairs tcondP asts in
  let inc_opt = Option.(fold (fun acc a -> (+) a <$> acc) (pure 1)) in
  let context_count =
    let open ContextMap in
    List.fold_left (fun acc (context, _) -> update context inc_opt acc) empty pairs in
  let context_rule_count =
    let open ContextRuleMap in
    List.fold_left (fun acc pair -> update pair inc_opt acc) empty pairs in
  let context_set = ContextSet.of_list @@ List.map fst pairs in
  let context_rule_set = ContextRuleSet.of_list pairs in
  let assoc =
    let open List.Infix in
    let* context = ContextSet.to_list context_set in
    let+ pair = ContextRuleSet.to_list context_rule_set in
    let denominator = Option.get_exn @@ ContextMap.get context context_count in
    let numerator = Option.get_exn @@ ContextRuleMap.get pair context_rule_count in
    pair, Float.(of_int numerator /. of_int denominator) in
  ContextRuleMap.of_list assoc

let cost asts tcondP lambda =
  let probMap = generate_probability_map tcondP asts in
  let score ast =
    let pairs = enumerate_context_rule_pairs tcondP [ast] in
    List.fold_left (fun acc pair ->
      acc +. (Option.get_exn @@ ContextRuleMap.get pair probMap)) 0. pairs in
  let tcondPTrainingSetScore =
    List.fold_left (fun acc ast -> score ast +. acc) 0. asts in
  let nWrites = Float.of_int @@ List.length @@ List.filter is_write tcondP in
  let nAST = Float.of_int @@ List.length asts in
  tcondPTrainingSetScore /. nAST +. lambda *. nWrites

module H = PairingHeap.Make (struct
  type t = float * p
  let compare a b = Float.compare (fst a) (fst b)
end)

let cost_tcond_pp fmt (f, tcondP) =
  Format.fprintf fmt "(%a, %a)" Float.pp f pp' tcondP

let train trainingSet lambda maxIter =
  let rec aux h iter =
    if iter >= maxIter then h
    else begin
      Format.printf "TCOND training iteration %i\r" iter;
      Format.print_flush ();
      let _, p = H.find_min_exn h in
      (* Format.printf "%a\n" (H.pp cost_tcond_pp) h; *)
      (* Format.print_flush (); *)
      (* read_line (); *)
      let p' = Random.run (mutate p) in
      if List.is_empty p' || List.length p' <= 1 then aux h iter
      else
        let h' = H.insert h (cost trainingSet p' lambda, p') in
        aux h' (iter + 1)
    end
  in
  let startP = [M Up; W WriteValue] in
  let startCost = cost trainingSet startP lambda in
  let startH = H.singleton (startCost, startP) in
  let endH = aux startH 0 in
  snd (H.find_min_exn endH)
