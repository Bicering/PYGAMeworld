
open Prettiest
open Syntax

let pri_in = 5
let pri_semi = 10
let pri_function = 30
let pri_with = 30
let pri_and = 35
let pri_then = 37
let pri_else = 38
let pri_as = 42
let pri_bar = 43
let pri_comma = 45
let pri_minusgreater = 47
let pri_barbar = 50
let pri_amperamper = 52
let pri_equal = 55
let pri_infix0 = 55
let pri_infix1 = 60
let pri_coloncolon = 65
let pri_infix2 = 70
let pri_infix3 = 80
let pri_infix4 = 90
let pri_unary_minus = 95
let pri_app = 110
let pri_dot = 120
let pri_prefix = 130

let paren flag doc =
  if flag then
    lparen <.> align doc <.> rparen
  else
    doc

let pretty_constant pri = function
  | Const_char x ->
      char '\'' <.> text (Char.escaped x) <.> char '\''
  | Const_int x ->
      let d = text @@ string_of_int x in
      if x < 0 then
        paren (pri>=pri_unary_minus) d
      else
        d
  | Const_float x ->
      let d = text @@ string_of_float x in
      if x < 0. then
        paren (pri>=pri_unary_minus) d
      else
        d
  | Const_string x ->
      char '"' <.> text (String.escaped x) <.> char '"'

let pretty_longid id = text @@ string_of_long_ident id
