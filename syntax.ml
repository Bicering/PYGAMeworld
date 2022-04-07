type location = int * int
let no_location = -1,-1

type long_ident =
  | Lident of string
  | Ldot of long_ident * string

type constant =
  | Const_char of char
  | Const_int of int
  | Const_float of float
  | Const_string of string

(* tag *)

type constr_tag =
  | Constr_tag_regular of int * int
  | Constr_tag_extensible of long_ident * int

let new_exttag_stamp =
  let stamp = ref 0 in
  fun () ->
    l