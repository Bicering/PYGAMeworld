
open Builtin
open Type
open Error
open Syntax
open Global

let te_vars = ref []
let reset_te_vars () = te_vars := []

let bind_te_vars vars =
  te_vars := [];
  List.map (fun v ->
    if List.mem_assoc v !te_vars then
      failwith "bind_te_vars"
    else (
      let ty = new_global_type_var() in
      te_vars := (v,ty) :: !te_vars;
      ty
    )
  ) vars

let rec should_generate expr =
  let rec go expr =
    match expr.e_desc with
    | Pexpr_array [] -> true
    | Pexpr_constant _ -> true
    | Pexpr_constr(c,arg) ->
        begin match arg with
        | None -> true
        | Some arg -> go arg
        end
    | Pexpr_function _ -> true
    | Pexpr_ident _ -> true
    | Pexpr_if(cond,ifso,ifnot) ->
        begin match ifnot with
        | None -> go ifso
        | Some ifnot -> go ifso && go ifnot
        end
    | Pexpr_let(isrec,binds,body) ->
        List.for_all (fun (_,e) -> should_generate e) binds &&
        should_generate body
    | Pexpr_sequence(e1,e2) -> go e2
    | Pexpr_tuple es -> List.for_all should_generate es
    | _ -> false
  in
  go expr

(* type of *)

let type_of_constant = function
  | Const_char _ -> type_char
  | Const_int _ -> type_int
  | Const_float _ -> type_float
  | Const_string _ -> type_string

let type_of_type_expression strict te =
  let rec go te =
    match te.te_desc with
    | Ptype_arrow(te1,te2) ->
        type_arrow (go te1) (go te2)
    | Ptype_constr(id,params) ->
        let cd =
          try
            find_type_desc id
          with Not_found ->
            unbound_type_constr_err te.te_loc id in
        if List.length params <> cd.info.ty_arity then
          type_arity_err te.te_loc cd params;
        { typ_desc=Tconstr(cd.info.ty_constr, List.map go params)
        ; typ_level=notgeneric }
    | Ptype_tuple tes ->
        type_product (List.map go tes)
    | Ptype_var v ->
        begin try
          List.assoc v !te_vars
        with Not_found ->
          if strict then
            unbound_type_var_err v te
          else (
            let ty = new_global_type_var() in
            te_vars := (v,ty):: !te_vars;
            ty
          )
        end
  in
  go te

(* unify *)

let rec unify ty1 ty2 =
  let ty1' = type_repr ty1
  and ty2' = type_repr ty2 in
  if ty1' == ty2' then
    ()
  else (
    begin match ty1'.typ_desc, ty2'.typ_desc with
    | Tvar link1, Tvar link2 ->
        if ty1'.typ_level < ty2'.typ_level then (
          ty2'.typ_level <- ty1'.typ_level;
          link2 := Tlink ty1'
        ) else (
          ty1'.typ_level <- ty2'.typ_level;
          link1 := Tlink ty2'
        )
    | Tvar link, _ when not (check_occur ty1'.typ_level ty1' ty2') ->
        link := Tlink ty2';
    | _, Tvar link when not (check_occur ty2'.typ_level ty2' ty1') ->
        link := Tlink ty1';
    | Tarrow(t1x,t1y), Tarrow(t2x,t2y) ->
        unify t1x t2x;
        unify t1y t2y
    | Tconstr(c1,tys1), Tconstr(c2,tys2)
      when c1.info.ty_stamp = c2.info.ty_stamp ->
        unify_list tys1 tys2
    | Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params), _ ->
        unify (expand_abbrev body args params) ty2'
    | _, Tconstr({info={ty_abbr=Tabbrev(args,body)}}, params) ->
        unify ty1' (expand_abbrev body args params)
    | Tproduct t1s, Tproduct t2s ->
        unify_list t1s t2s
    | _ ->
        raise Unify
    end;
    type_repr ty1 |> ignore;
    type_repr ty2 |> ignore;
  )

and unify_list t1s t2s =
  match t1s, t2s with
  | [], [] -> ()
  | t1::t1s, t2::t2s ->
      unify t1 t2;
      unify_list t1s t2s
  | _ -> raise Unify

let unify_expr expr expect_ty actual_ty =
  try
    unify expect_ty actual_ty
  with Unify ->
    expr_wrong_type_err expr expect_ty actual_ty

let unify_pat pat expect_ty actual_ty =
  try
    unify expect_ty actual_ty
  with Unify ->
    pat_wrong_type_err pat expect_ty actual_ty

let rec typing_expr env expr =
  match expr.e_desc with
  | Pexpr_apply(e,es) ->
      let ty_f = typing_expr env e in
      List.fold_left (fun ty arg ->
        let ty1, ty2 =
          try
            filter_arrow ty
          with Unify ->
            application_of_non_function_err expr ty_f
        in
        typing_expect env arg ty1;
        ty2
      ) ty_f es
  | Pexpr_array es ->
      let ty = new_type_var() in
      List.iter (fun e -> typing_expect env e ty) es;
      type_array ty
  | Pexpr_constant c ->
      type_of_constant c
  | Pexpr_constr(id,arg) ->
      let cd =
        try find_constr_desc id
        with Not_found -> unbound_constr_err expr.e_loc id
      in
      begin match arg with
      | None ->
        begin match cd.info.cs_kind with
        | Constr_constant ->
            type_instance cd.info.cs_res
        | _ ->
            let ty1, ty2 = type_pair_instance cd.info.cs_arg cd.info.cs_res in
            type_arrow ty1 ty2
        end
      | Some arg ->
        begin match cd.info.cs_kind with
        | Constr_constant ->
            constant_constr_err expr.e_loc cd
        | _ ->
            let ty1, ty2 = type_pair_instance cd.info.cs_arg cd.info.cs_res in
            typing_expect env arg ty1;