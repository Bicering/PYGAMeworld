open Syntax

let cur_module = ref "Main"

let all_constrs : (long_ident, constr_desc global) Hashtbl.t = Hashtbl.create 17
let all_types : (long_ident, type_de