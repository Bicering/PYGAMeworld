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
    let r = !stamp in
    incr stamp;
    r

(* primitive *)

type prim =
  | Paddint
  | Pandint
  | Parraylength
  | Pasrint
  | Pccall of int * string
  | Pdecr
  | Pdivint
  | Pdummy of int
  | Pfield of int
  | Pfloat of float_prim
  | Pgetarrayitem
  | Pgetstringitem
  | Pgetglobal of long_ident
  | Pidentity
  | Pincr
  | Plslint
  | Plsrint
  | Pmakearray of bool
  | Pmakeblock of constr_tag
  | Pmakestring
  | Pmodint
  | Pmulint
  | Pnegint
  | Pnot
  | Porint
  | Praise
  | Psequand
  | Psequor
  | Psetarrayitem
  | Psetstringitem
  | Psetfield of int
  | Psetglobal of long_ident
  | Pstringlength
  | Psubint
  | Ptest of bool_test
  | Pupdate
  | Pxorint

and float_prim =
  | Paddfloat
  | Pnegfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat

and bool_test =
  | Ptest_eq
  | Ptest_neq
  | Ptest_int of int test_prim
  | Ptest_float of float test_prim
  | Ptest_string of string test_prim
  | Ptest_noteqtag of constr_tag

and 'a test_prim =
  | Peq
  | Pneq
  | Pneqimm of 'a
  | Plt
  | Ple
  | Pgt
  | Pge

(* global *)

type 'a global = { qualid: long_ident; info: 'a }

(* type *)

let generic = -1 and notgeneric = 0

type typ = { typ_desc: typ_desc; mutable typ_level: int }
and typ_desc =
  | Tarrow of typ * typ
  | Tconstr of type_constr global * typ list
  | Tproduct of typ list
  | Tvar of typ_link ref
and typ_link =
  | Tnolink
  | Tlink of typ
and type_constr = { ty_stamp: int; mutable ty_abbr: type_abbrev }
and type_abbrev =
  | Tnotabbrev
  | Tabbrev of typ list * typ

(* type constructur descriptions
 * *)

(* e.g