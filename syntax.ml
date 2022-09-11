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

(* e.g. unit, int, list, option *)
type type_desc =
  { ty_constr: type_constr global; ty_arity: int; mutable ty_desc: type_components }
and type_components =
  | Abstract_type
  | Variant_type of constr_desc global list
  | Abbrev_type of typ list * typ

(* e.g. false, None *)
and constr_desc =
  { cs_arg: typ; cs_res: typ; cs_tag: constr_tag; cs_kind: constr_kind }

and constr_kind =
  | Constr_constant
  | Constr_regular
  | Constr_superfluous of int

type expression = { e_desc: expression_desc; e_loc: location }
and expression_desc =
  | Pexpr_apply of expression * expression list
  | Pexpr_array of expression list
  | Pexpr_constant of constant
  | Pexpr_constraint of expression * type_expression
  | Pexpr_constr of long_ident * expression option
  | Pexpr_for of string * expression * expression * bool * expression
  | Pexpr_function of (pattern * expression) list
  | Pexpr_ident of long_ident
  | Pexpr_if of expression * expression * expression option
  | Pexpr_let of bool * (pattern * expression) list * expression
  | Pexpr_sequence of expression * expression
  | Pexpr_try of expression * (pattern * expression) list
  | Pexpr_tuple of expression list

and type_expression = { te_desc: type_expression_desc; te_loc: location }
and type_expression_desc =
  | Ptype_var of string
  | Ptype_arrow of type_expression * type_expression
  | Ptype_tuple of type_expression list
  | Ptype_constr of long_ident * type_expression list

and pattern = { p_desc: pattern_desc; p_loc: location }
and pattern_desc =
  | Ppat_alias of pattern * string
  | Ppat_any
  | Ppat_array of pattern list
  | Ppat_constant of constant
  | Ppat_constraint of pattern * type_expression
  | Ppat_constr of long_ident * pattern option
  | Ppat_or of pattern * pattern
  | Ppat_tuple of pattern list
  | Ppat_var of string

(* RHS of type xx = ... *)
type constr_decl = string * type_expression option

type type_decl =
  | Ptd_abstract
  | Ptd_variant of constr_decl list
  | Ptd_alias of type_expression

let rec expr_is_pure expr =
  match expr.e_desc with
  | Pexpr_array(es) -> List.for_all expr_is_pure es
  | Pexpr_constr _
  | Pexpr_constant _
  | Pexpr_function _
  | Pexpr_ident _ -> true
  | _ -> false

type impl_phrase = { im_desc: impl_desc; im_loc: location }
and impl_desc =
  | Pimpl_expr of expression
  | Pimpl_typedef of (string * string list * type_decl) list
  | Pimpl_letdef of bool * (pattern * expression) list
  | Pimpl_excdef of constr_decl

(* global value *)

type value_desc = { v_typ: typ; v_prim: prim_desc }
and prim_desc =
  | Not_prim
  | Prim of int * prim

(* instances *)

let no_type = {typ_desc=Tproduct []; typ_level=0}

(* type stamp *)

let init_stamp = ref 0

let new_type_stamp () =
  let r = !init_stamp in
  incr init_stamp;
  r

(* pattern *)

let rec free_vars_of_pat pat =
  match pat.p_desc with
  | Ppat_alias(p,_)
  | Ppat_constraint(p,_) -> free_vars_of_pat p
  | Ppat_any
  | Ppat_constant _ -> []
  | Ppat_array ps
  | Ppat_tuple ps -> List.map free_vars_of_pat ps |> List.concat
  | Ppat_constr(_,arg) ->
      begin match arg with
      | None -> []
      | Some arg -> free_vars_of_pat arg
      end
  | Ppat_or(p1,p2) -> free_vars_of_pat p1 @ free_vars_of_pat p2
  | Ppat_var v -> [v]

(* dump *)

let rec string_of_long_ident = function
  | Lident id -> id
  | Ldot (l,id) -> string_of_long_ident l ^ "." ^ id

let show_constant = function
  | Const_char c ->
      Printf.sprintf "Const_char %s" (Char.escaped c)
  | Const_int i ->
      Printf.sprintf "Const_int %d" i
  | Const_float f ->
      Printf.sprintf "Const_float %f" f
  | Const_string s ->
      Printf.sprintf "Const_string %s" (String.escaped s)

let dump_constant c =
  show_constant c |> print_endline

let rec dump_pattern d pat =
  let rec go d p =
    Printf.printf "%*s" (2*d) "";
    match p.p_desc with
    | Ppat_alias(p,a) ->
        Printf.printf "Alias %s\n" a;
        go (d+1) p
    | Ppat_any ->
        print_endline "Any"
    | Ppat_array ps ->
        Printf.printf