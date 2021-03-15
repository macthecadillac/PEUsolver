open Containers
open Fun

type 'a printer = Format.formatter -> 'a -> unit

type t = Grammar.t Tree.t

let str_pp s fmt () = Format.fprintf fmt "%s" s

let pp fmt ast =
  let list_pp =
    List.pp ~pp_sep:(str_pp ", ")
            ~pp_start:(str_pp "")
            ~pp_stop:(str_pp "")
            (fun fmt s -> Format.fprintf fmt "%s" s) in
  let to_string = function
      s, [] -> Format.sprintf "%a" Grammar.pp s
    | s, l -> Format.sprintf "%a(%a)" Grammar.pp s list_pp l in
  Format.fprintf fmt "%s" @@ Tree.fold (curry to_string) ast
exception EvalError of string

module Value = struct
  type t = Str of string | Int of int | Bool of bool

  let pp fmt = function
      Str s -> Format.fprintf fmt "%s" s
    | Int i -> Format.fprintf fmt "%i" i
    | Bool b -> Format.fprintf fmt "%b" b

  let equal a b =
    match a, b with
      Str a, Str b -> String.equal a b
    | Int a, Int b -> Int.equal a b
    | Bool a, Bool b -> Bool.equal a b
    | _ -> false
end

let equal = Tree.equal Grammar.equal

let eval ?arg0 ?arg1 ?arg2 ?arg3 ast =
  let open Value in
  let try_unwrap_str = function
      Some (Str s) -> Str s
    | Some _ -> raise (EvalError "arg0 type error")
    | None -> raise (EvalError "arg0 not provided") in
  let try_unwrap_int = function
      Some (Int i) -> Int i
    | Some _ -> raise (EvalError "arg0 type error")
    | None -> raise (EvalError "arg0 not provided") in
  let aux = function
      "str.++", [Str a; Str b] -> Str (a ^ b)
    | "str.at", [Str a; Int b] ->
        if String.length a > b && b >= 0
        then Str (String.get a b |> String.of_char)
        else Str ""
    | "str.substr", [Str s; Int a; Int b] ->
        if a >= 0 && b >= 0
        then Str (String.drop a s |> String.take b)
        else Str ""
    | "str.indexof", [Str a; Str b; Int i] -> 
        if i < 0 then Int (-1)
        else
          let n = String.drop i a
            |> String.split ~by:b
            |> List.head_opt
            |> Option.fold (fun a b -> String.length b + a) (-1) in
          Int n
    | "str.replace", [Str a; Str b; Str c] ->
        if String.equal b ""
        then Str (c ^ a)
        else Str (String.replace ~which:`Left ~sub:b ~by:c a)
    | "int.to.str", [Int i] -> Str (Printf.sprintf "%i" i)
    | "str.ite", [Bool p; Str a; Str b] -> Str (if p then a else b)
    | "str.len", [Str s] -> Int (String.length s)
    | "str.to.int", [Str s] -> Int (Option.get_or ~default:(-1) @@ Int.of_string s)
    | "int.ite", [Bool p; Int a; Int b] -> Int (if p then a else b)
    | "str.contains", [Str a; Str b] -> Bool (String.mem ~sub:b a)
    | "str.prefixof", [Str a; Str b] -> Bool (String.prefix ~pre:a b)
    | "str.suffixof", [Str a; Str b] -> Bool (String.suffix ~suf:a b)
    | "+", [Int a; Int b] -> Int (a + b)
    | "-", [Int a; Int b] -> Int (a - b)
    | "=", [a; b] -> Bool (equal a b)
    | "str._arg_0", _ -> try_unwrap_str arg0
    | "str._arg_1", _ -> try_unwrap_str arg1
    | "str._arg_2", _ -> try_unwrap_str arg2
    | "str._arg_3", _ -> try_unwrap_str arg3
    | "int._arg_0", _ -> try_unwrap_int arg0
    | "int._arg_1", _ -> try_unwrap_int arg1
    | "int._arg_2", _ -> try_unwrap_int arg2
    | "int._arg_3", _ -> try_unwrap_int arg3
    | "true", _ -> Bool true
    | "false", _ -> Bool false
    | s, [] ->  (* for literals *)
        if String.prefix ~pre:"\"" s && String.suffix ~suf:"\"" s
        then
          let len = String.length s in
          Str (String.take (len - 1) s |> String.drop 1)
        else begin
          match Int.of_string s with
            None -> raise @@ EvalError (Printf.sprintf "Trying to eval %s as an int" s)
          | Some i -> Int i
        end
    | s, _ -> raise @@ EvalError (Printf.sprintf "Not implemented: %s" s)
  in Tree.fold (curry aux) @@ Tree.map Grammar.to_string ast

let equal' t1 t2 = Value.equal (eval t1) (eval t2)
