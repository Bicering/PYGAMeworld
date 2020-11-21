
open Builtin
open Error
open Global
open Lambda
open Syntax

let search_env env name =
  let rec go i = function
    | [] -> raise Not_found
    | vs::vss ->
        try i, List.assoc name vs
        with Not_found -> go (i+1) vss
  in
  go 0 env

let transl_access env name =
  let i, path = search_env env name in
  List.fold_right (fun j l ->
    Lprim(Pfield j, [l])
  ) path (Lvar i)

let transl_update env name value =
  match search_env env name with
  | i, n::path ->
      Lprim(Psetfield n,
      [List.fold_right (fun j l -> Lprim(Pfield j, [l])) path (Lvar i);
      value])
  | _ -> assert false

let paths_of_pat path p =
  let rec go acc path p =
    match p.p_desc with
    | Ppat_alias(p,a) ->
        go ((a,path)::acc) path p
    | Ppat_constraint(p,_) ->
        go acc path p
    | Ppat_constr(_,p) ->
        begin match p with
        | None -> acc
        | Some p -> go acc (0::path) p
        end
    | Ppat_tuple ps ->
        let rec go2 acc i = function
          | [] -> acc
          | p::ps -> go2 (go acc (i::path) p) (i+1) ps
        in
        go2 acc 0 ps
    | Ppat_var v ->
        (v,path)::acc
    | _ -> acc
  in
  go [] path p

let make_env env ps =
  List.fold_left (fun env p -> paths_of_pat [] p :: env) env ps

(* matching *)

type res = Partial | Total | Dubious

type row = pattern list * lambda
type matching = row list * lambda list

let add_match (rows,paths) row =
  row::rows, paths

(* (vars with leftmost column removed, others) *)
let split_matching (rows,paths) =
  let rec go = function
    | ({p_desc=Ppat_any}::ps,act)::rest
    | ({p_desc=Ppat_var _}::ps,act)::rest ->
        let vars, others = go rest in
        add_match vars (ps,act), others
    | ({p_desc=Ppat_alias(p,_)}::ps,act)::rest
    | ({p_desc=Ppat_constraint(p,_)}::ps,act)::rest ->
        go ((p::ps,act)::rest)
    | rows ->
        ([],List.tl paths), (rows,paths)
  in
  go rows

let make_const_match paths row : matching =
  [row], List.tl paths

let make_constr_match cd paths row : matching =
  match paths with
  | [] -> assert false
  | hd::tl ->
      match cd.info.cs_kind with
      | Constr_constant ->
          [row], tl
      | _ ->
          [row], Lprim(Pfield 0, [hd])::tl

let add_to_division make_match divs key (row : row) =
  try
    let ms = List.assoc key divs in
    ms := add_match !ms row;
    divs
  with Not_found ->
    (key, ref (make_match row)) :: divs

let pat_any =
  { p_desc=Ppat_any; p_loc=no_location }

let simplify_upper_left rows =
  let rec go = function
    | ({p_desc=Ppat_alias(p,_)}::ps,act)::rest
    | ({p_desc=Ppat_constraint(p,_)}::ps,act)::rest ->
        go ((p::ps,act)::rest)
    | ({p_desc=Ppat_or(p1,p2)}::ps,act)::rest ->
        go ((p1::ps,act)::(p2::ps,act)::rest)
    | rows -> rows
  in
  go rows

(* divide *)

let divide_tuple_matching arity (rows,paths) =
  let rec go = function
    | [] ->
        let rec make_path n = function
          | hd::tl ->
              let rec make i =
                if i >= n then tl
                else Lprim(Pfield i, [hd]) :: make (i+1)
              in
              make 0
          | [] -> assert false
        in
        [], make_path arity paths
    | ({p_desc=Ppat_array args}::ps,act)::rest
    | ({p_desc=Ppat_tuple args}::ps,act)::rest ->
        add_match (go rest) (args@ps,act)
    | ({p_desc=Ppat_any|Ppat_var _}::ps,act)::rest ->
        let rec make i =
          if i >= arity then
            ps
          else
            pat_any::make (i-1)
        in
        add_match (go rest) (make arity, act)
    | _ -> assert false
  in
  go (simplify_upper_left rows)

let divide_constant_matching (rows,paths) =
  let rec go = function
    | ({p_desc=Ppat_constant c}::ps,act)::rest ->
        let constants, others = go rest in
        add_to_division (make_const_match paths) constants c (ps,act),
        others
    | rows ->
        [], (rows, paths)
  in
  go (simplify_upper_left rows)

let divide_constr_matching (rows,paths) =
  let rec go = function
    | ({p_desc=Ppat_constr(id,arg)}::ps,act)::rest ->
        let cd = find_constr_desc id in
        let ps =
          begin match arg with
          | None -> ps
          | Some arg ->
              match cd.info.cs_kind with
              | Constr_constant -> ps
              | _ -> arg::ps
          end in
        let constrs, others = go rest in