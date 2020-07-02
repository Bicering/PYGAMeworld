
open Syntax
open Global

(* type_constr global *)

let f id =
  { qualid=Lident id; info={ ty_stamp=new_type_stamp(); ty_abbr=Tnotabbrev } }
let type_constr_unit   = f "unit"
let type_constr_bool   = f "bool"
let type_constr_char   = f "char"
let type_constr_int    = f "int"
let type_constr_float  = f "float"
let type_constr_exn    = f "exn"
let type_constr_string = f "string"
let type_constr_option = f "option"
let type_constr_list   = f "list"
let type_constr_array  = f "array"

(* typ *)

let f desc = { typ_desc=Tconstr(desc, []); typ_level=notgeneric }
let type_unit   = f type_constr_unit
let type_bool   = f type_constr_bool
let type_char   = f type_constr_char
let type_int    = f type_constr_int
let type_float  = f type_constr_float
let type_exn    = f type_constr_exn
let type_string = f type_constr_string
let type_option = f type_constr_option

let f desc t = { typ_desc=Tconstr(desc, [t]); typ_level=notgeneric }
let type_list   = f type_constr_list
let type_array  = f type_constr_array
let gen_type_array ty =
  { typ_desc=Tconstr(type_constr_array, [ty]); typ_level=generic }

let type_arrow t1 t2 =
  { typ_desc=Tarrow(t1,t2); typ_level=notgeneric }
let gen_type_arrow t1 t2 =
  { typ_desc=Tarrow(t1,t2); typ_level=generic }

let type_product ts =
  { typ_desc=Tproduct ts; typ_level=notgeneric }

(* constr_desc global *)

let f id info =
  { qualid=Lident id; info=info }

let constr_void =
  f "()"
  { cs_res={ typ_desc=Tconstr(type_constr_unit, []); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=Constr_tag_regular(1,0)
  ; cs_kind=Constr_constant
  }

let constr_false =
  f "false"
  { cs_res={ typ_desc=Tconstr(type_constr_bool, []); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=Constr_tag_regular(2,0)
  ; cs_kind=Constr_constant
  }

let constr_true =
  f "true"
  { cs_res={ typ_desc=Tconstr(type_constr_bool, []); typ_level=notgeneric }
  ; cs_arg=type_unit
  ; cs_tag=Constr_tag_regular(2,1)
  ; cs_kind=Constr_constant
  }

let match_failure_tag =
  Constr_tag_extensible(Lident "Match_failure",new_exttag_stamp())

let division_by_zero_tag =
  Constr_tag_extensible(Lident "Division_by_zero",new_exttag_stamp())

let constr_match_failure =
  f "Match_failure"
  { cs_res={ typ_desc=Tconstr(type_constr_exn, []); typ_level=notgeneric }
  ; cs_arg=type_product [type_string; type_int; type_int]
  ; cs_tag=match_failure_tag
  ; cs_kind=Constr_regular
  }

let constr_division_by_zero =
  f "Division_by_zero"
  { cs_res={ typ_desc=Tconstr(type_constr_exn, []); typ_level=notgeneric }
  ; cs_arg=type_product []
  ; cs_tag=division_by_zero_tag
  ; cs_kind=Constr_constant
  }

let constr_nil =
  let arg = { typ_desc=Tvar(ref Tnolink); typ_level=generic } in
  f "[]"
  { cs_res={ typ_desc=Tconstr(type_constr_list, [arg]); typ_level=generic }
  ; cs_arg=type_unit
  ; cs_tag=Constr_tag_regular(2,0)