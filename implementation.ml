
open Back
open Builtin
open Emit
open Error
open Front
open Global
open Instruction
open Lambda
open Printer
open Syntax
open Type
open Typing

let stage = ref 4
let verbose = ref false

let typing_impl_expr loc e =
  push_level();
  let ty = typing_expr [] e in
  pop_level();
  if should_generate e then
    gen_type ty;
  ty

let submit_variant ty_res cs =
  let n = List.length cs in
  let rec go i acc = function
    | [] -> List.rev acc
    | (name,arg)::xs ->
        match arg with
        | None ->
            let constr =
              { qualid=Lident name
              ; info={ cs_res=ty_res
                     ; cs_arg=type_unit
                     ; cs_tag=Constr_tag_regular(n,i)
                     ; cs_kind=Constr_constant
                     }
              }
            in
            add_global_constr constr;
            go (i+1) (constr::acc) xs
        | Some arg ->
            let ty_arg = type_of_type_expression true arg in
            (* TODO kind *)
            let constr =
              { qualid=Lident name
              ; info={ cs_res=ty_res
                     ; cs_arg=ty_arg
                     ; cs_tag=Constr_tag_regular(n,i)
                     ; cs_kind=Constr_regular
                     }
              }
            in
            add_global_constr constr;
            go (i+1) (constr::acc) xs
  in
  let cds = go 0 [] cs in
  pop_level();
  gen_type ty_res;
  List.iter (fun cd -> gen_type cd.info.cs_arg) cds;
  Variant_type cds

let typing_impl_typedef loc decl : (typ * type_components) list =
  let submit (name,args,def) =
    let ty_constr =
      { qualid=Lident name
      ; info={ ty_stamp=new_type_stamp(); ty_abbr=Tnotabbrev }
      } in
    let ty_desc =