open Containers

type 'a printer = Format.formatter -> 'a -> unit

type error = SolutionNotFound

let error_pp fmt _ = Format.fprintf fmt "SolutionNotFound"

let rec list_pp s pp fmt = function
    [] -> ()
  | [a] -> Format.fprintf fmt "\n%s└── %a" s (pp (s ^ "    ")) a
  | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (pp (s ^ "│   ")) hd;
              list_pp s pp fmt tl

let str_pp s fmt () = Format.fprintf fmt s

(* Helper function that helps with loops *)
let rec while_do_result pred f x =
  let open Result in
  let* y = x in
  if pred y then x
  else while_do_result pred f @@ f y
