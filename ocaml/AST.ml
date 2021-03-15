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

let equal = Tree.equal Grammar.equal

type value = Str of string | Int of int | Bool of bool

let value_pp fmt = function
    Str s -> Format.fprintf fmt "%s" s
  | Int i -> Format.fprintf fmt "%i" i
  | Bool b -> Format.fprintf fmt "%b" b

let eval ?arg0 ?arg1 ?arg2 ?arg3 ast =
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
    | "str.at", [Str a; Int b] -> Str (String.get a b |> String.of_char)
    | "str.substr", [Str s; Int a; Int b] -> Str (String.drop a s |> String.take b)
    | "str.indexof", [Str a; Str b; Int i] -> 
        let n = String.drop i a
          |> String.split ~by:b
          |> List.head_opt
          |> Option.fold (fun a b -> String.length b + a) (-1) in
        Int n
    | "str.replace", [Str a; Str b; Str c] ->
        Str (String.replace ~which:`Left ~sub:b ~by:c a)
    | "int.to.str", [Int i] -> Str (Printf.sprintf "%i" i)
    | "str.ite", [Bool p; Str a; Str b] -> Str (if p then a else b)
    | "str.len", [Str s] -> Int (String.length s)
    | "str.to.int", [Str s] -> Int (Option.get_or ~default:(-1) @@ Int.of_string s)
    | "int.ite", [Bool p; Int a; Int b] -> Int (if p then a else b)
    | "str.contains", [Str a; Str b] -> Bool (String.mem ~sub:b a)
    | "str.prefixof", [Str a; Str b] -> Bool (String.prefix ~pre:a b)
    | "str.suffixof", [Str a; Str b] -> Bool (String.suffix ~suf:a b)
    | "str._arg_0", _ -> try_unwrap_str arg0
    | "str._arg_1", _ -> try_unwrap_str arg1
    | "str._arg_2", _ -> try_unwrap_str arg2
    | "str._arg_3", _ -> try_unwrap_str arg3
    | "int._arg_0", _ -> try_unwrap_int arg0
    | "int._arg_1", _ -> try_unwrap_int arg1
    | "int._arg_2", _ -> try_unwrap_int arg2
    | "int._arg_3", _ -> try_unwrap_int arg3
    | s, _ -> raise @@ EvalError (Printf.sprintf "Not implemented: %s" s)
  in Tree.fold (curry aux) @@ Tree.map Grammar.to_string ast
