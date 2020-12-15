
%{
open Syntax

let get_loc () = Parsing.symbol_start (), Parsing.symbol_end ()

let make_expr desc =
  {e_desc=desc; e_loc=get_loc()}

let make_pat desc =
  {p_desc=desc; p_loc=get_loc()}

let make_type_expression desc =
  {te_desc=desc; te_loc=get_loc()}

let make_impl desc =
  {im_desc=desc; im_loc=get_loc()}

let make_uplus name arg =
  match name, arg.e_desc with
  | "+", Pexpr_constant(Const_int x) ->
      make_expr arg.e_desc
  | ("+"|"+."), Pexpr_constant(Const_float x) ->
      make_expr arg.e_desc
  | _ ->
      make_expr(Pexpr_apply(
        make_expr(Pexpr_ident(Lident "~+")),
        [arg]))

let make_uminus name arg =
  match name, arg.e_desc with
  | "-", Pexpr_constant(Const_int x) ->
      make_expr(Pexpr_constant(Const_int (-x)))
  | ("-"|"-."), Pexpr_constant(Const_float x) ->
      make_expr(Pexpr_constant(Const_float (-.x)))
  | _ ->
      make_expr(Pexpr_apply(
        make_expr(Pexpr_ident(Lident "~-")),
        [arg]))

let make_unop op ({e_loc=l1,_} as e1) =
  let l,_ as loc = get_loc() in
  {e_desc=Pexpr_apply({e_desc=Pexpr_ident(Lident op);
                       e_loc=(l,l1) }, [e1]);
   e_loc=loc}

let make_binop op ({e_loc=_,m1} as e1) ({e_loc=l2,_} as e2) =
  Pexpr_apply({e_desc=Pexpr_ident(Lident op);
               e_loc=(m1,l2)}, [e1; e2]) |> make_expr

let make_ternop op ({e_loc=_,m1} as e1) ({e_loc=l2,_} as e2) e3 =
  Pexpr_apply({e_desc=Pexpr_ident(Lident op);
               e_loc=(m1,l2)}, [e1; e2; e3]) |> make_expr

let make_expr_list es =
  List.fold_right (fun e acc ->
    make_expr (Pexpr_constr(Lident "::",
      Some(make_expr(Pexpr_tuple [e; acc]))))
  ) es (make_expr(Pexpr_constr(Lident "[]", None)))

let make_pat_list es =
  List.fold_right (fun p acc ->
    make_pat (Ppat_constr(Lident "::",
      Some(make_pat(Ppat_tuple [p; acc]))))
  ) es (make_pat(Ppat_constr(Lident "[]", None)))

let make_apply e1 e2 =
  match e1.e_desc, e2 with
  | Pexpr_constr(c,None), [e2] ->
      make_expr(Pexpr_constr(c, Some e2))
  | _ ->
      make_expr(Pexpr_apply(e1, e2))
%}

%token <char> CHAR
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> LIDENT
%token <string> UIDENT
%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> INFIX3
%token <string> INFIX4

%token EOF
%token FALSE
%token TRUE