open Syntax

let cur_module = ref "Main"

let all_constrs : (long_ident, constr_desc global) Hashtbl.t = Hashtbl.create 17
let all_types : (long_ident, type_desc global) Hashtbl.t= Hashtbl.create 17
let all_values : (long_ident, value_desc global) Hashtbl.t= Hashtbl.create 17

let find_desc sel name =
  Hashtbl.find sel name

let find_constr_de