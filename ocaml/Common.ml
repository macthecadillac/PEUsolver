type 'a printer = Format.formatter -> 'a -> unit

type error = SolutionNotFound

let error_pp fmt _ = Format.fprintf fmt "SolutionNotFound"

let rec list_pp s pp fmt = function
    [] -> ()
  | [a] -> Format.fprintf fmt "\n%s└── %a" s (pp (s ^ "    ")) a
  | hd::tl -> Format.fprintf fmt "\n%s├── %a" s (pp (s ^ "│   ")) hd;
              list_pp s pp fmt tl

let str_pp s fmt () = Format.fprintf fmt s
