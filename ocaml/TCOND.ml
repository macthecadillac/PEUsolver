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

type 'a printer = Format.formatter -> 'a -> unit

let pp fmt tcond =
  let to_string = function
      W WriteValue -> "WriteValue"
    | M Up -> "Up"
    | M Left -> "Left"
    | M Right -> "Right"
    | M DownFirst -> "DownFirst"
    | M DownLast -> "DownLast"
    | M PrevDFS -> "PrevDFS" in
  Format.fprintf fmt "%s" @@ to_string tcond

let pp' =
  let f s fmt () = Format.fprintf fmt "%s" s in
  List.pp ~pp_sep:(f "; ") ~pp_start:(f "[") ~pp_stop:(f "]") pp

let moveOps = [M Up; M Left; M Right; M DownFirst; M DownLast; M PrevDFS]

let writeOps = [W WriteValue]

let firstOp = [M Up; M Left; M Right; M PrevDFS]

let allOps = moveOps @ writeOps

let compact p =
  let rec aux acc p =
    match acc, p with
      [], l -> l
    | _, [] -> []
    | l, W _::ps -> aux l ps
    | M Left::xs, ((M Up::_) as ps)
    | M Right::xs, ((M Up::_) as ps)
    | M Left::xs, M Right::ps
    | M Right::xs, M Left::ps
    | M DownFirst::xs, M Up::ps
    | M DownLast::xs, M Up::ps -> aux xs ps
    | xs, M PrevDFS::ps -> aux xs (M Up::ps)
    | l, p::ps -> aux (p::l) ps
  in match p with
    [] -> []
  | op::rest -> aux [op] rest

let apply loc ast p =
  let rec aux acc i (Tree.Node (_, l) as node) child p dir =
    match move acc i child p `None with
      `Done _ as c -> c
    | `Continue (`Up, acc', p') ->
        move acc' i node p' `None
    | `Continue (`Left, acc', p') ->
        if i <= 0
        then aux acc' i node (List.nth l i) (compact (M Left::p')) `None
        else begin
          aux acc' (i - 1) node (List.nth l (i - 1)) p' `None
        end
    | `Continue (`Right, acc', p') ->
        if i >= List.length l - 1
        then aux acc' i node (List.nth l i) (compact (M Right::p')) `None
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
          None -> move acc i node (compact tl) `None
        | Some h -> aux acc 0 node h tl `None
      end
    | M DownLast::tl, `None -> begin
        match List.last_opt l with
          None -> move acc i node (compact tl) `None
        | Some h -> aux acc (List.length l - 1) node h tl `None
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
        match compact tl with
          M DownLast::tl' | M DownFirst::tl' -> main acc 0 ast tl' `None
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
  | chunk -> let n = R.run (R.int_range 1 (List.length chunk - 2)) in
             let op = R.run (R.pick_list moveOps) in
             Some (List.map_nth_opt (const (Some op)) n chunk)

let add chunk =
  let n = R.run (R.int_range 1 (List.length chunk - 2)) in
  let op = R.run (R.pick_list moveOps) in
  Some (List.map_nth_opt (const (Some op)) n chunk)

let append =
  let op = R.pick_list firstOp in
  let write = R.pick_list writeOps in
  R.list_seq [op; write]

let mutate p =
  let open Random in
  let partition =
    let rec aux acc1 acc2 = function
        [] -> List.rev @@ List.rev acc1::acc2
      | (W _ as w)::tl -> aux [] (List.rev (w::acc1)::acc2) tl
      | hd::tl -> aux (hd::acc1) acc2 tl in
    aux [] [] in
  let* op = pick_list [`I delete; `I modify; `I add; `A append] in
  match op with
    `A l -> map (List.append p) l
  | `I f ->
      let chunks = partition p in
      let len = List.length chunks in
      let+ n = int len in
      List.map_nth_opt f n chunks |> List.flatten

let initPool =
  let aux l : p * p list =
    let p = R.run (R.pick_list l) in
    let p' = R.run (mutate p) in
    p', p'::l in
  Seq.unfold (Option.pure % aux) [firstOp]

(* let cost trainingSet p lambda = *)
(*   let penalty = lambda *. Float.of_int (List.length p) in *)
(*   () *)
