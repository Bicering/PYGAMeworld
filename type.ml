
open Builtin
open Syntax
open Global

let assoc_mem k kvs = try List.assoc k kvs; true with Not_found -> false

exception Unify
exception Recursive_abbrev

let cur_level = ref 0
let push_level () =
  (*Printf.printf "+ level %d\n" (1+ !cur_level);*)
  incr cur_level
let pop_level () =
  (*Printf.printf "- level %d\n" (!cur_level - 1);*)
  decr cur_level

(* union-find *)

let rec type_repr ty =
  match ty.typ_desc with
  | Tvar link ->
      begin match !link with
      | Tnolink ->
          ty
      | Tlink ty' ->
          let ty'' = type_repr ty' in
          link := Tlink ty'';
          ty''
      end
  | _ -> ty

(* new *)

let new_type_var () =
  { typ_desc=Tvar(ref Tnolink); typ_level= !cur_level }

let new_global_type_var () =
  (* typ_level=1 can be generalized *)
  { typ_desc=Tvar(ref Tnolink); typ_level=1 }

let rec new_type_var_list arity =
  if arity <= 0 then
    []
  else
    { typ_desc=Tvar(ref Tnolink); typ_level= !cur_level } ::
    new_type_var_list (arity-1)

let rec type_var_list level arity =
  if arity <= 0 then
    []
  else
    { typ_desc=Tvar(ref Tnolink); typ_level=level } ::
    type_var_list level (arity-1)

(* generalize *)

let gen_type ty =
  let rec go ty =
    let ty' = type_repr ty in
    begin match ty'.typ_desc with
    | Tarrow(ty1,ty2) ->
        let l1 = go ty1
        and l2 = go ty2 in
        ty'.typ_level <- min l1 l2
    | Tconstr(_,tys) ->
        ty'.typ_level <- gos tys
    | Tproduct tys ->
        ty'.typ_level <- gos tys
    | Tvar _ ->
        if ty'.typ_level > !cur_level then
          ty'.typ_level <- generic
    end;
    ty'.typ_level
  and gos = function
    | [] -> notgeneric
    | ty::tys ->
        let l1 = go ty and l2 = gos tys in
        min l1 l2
  in