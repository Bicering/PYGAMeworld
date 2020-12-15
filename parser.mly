
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

%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUS          /* "-" */
%token MINUSDOT       /* "-." */
%token MINUSGREATER   /* "->" */
%token DOT            /* "." */
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token COLONEQUAL     /* ":=" */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LESSMINUS      /* "<-" */
%token PLUS           /* "+" */
%token PLUSDOT        /* "+." */
%token RBRACKET       /* "]" */
%token QUOTE          /* "'" */
%token UNDERSCORE     /* "_" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token RBRACE         /* "}" */
%token AMPERSAND      /* "&" */
%token AMPERAMPER     /* "&&" */
%token BARBAR         /* "||" */
/* Keywords */
%token AND            /* "and" */
%token AS             /* "as" */
%token BEGIN          /* "begin" */
%token DO             /* "do" */
%token DONE           /* "done" */
%token DOWNTO         /* "downto" */
%token ELSE           /* "else" */
%token END            /* "end" */
%token EXCEPTION      /* "exception" */
%token FOR            /* "for" */
%token FUN            /* "fun" */
%token FUNCTION       /* "function" */
%token IF             /* "if" */
%token IN             /* "in" */
%token LET            /* "let" */
%token MATCH          /* "match" */
%token MUTABLE        /* "mutable" */
%token OF             /* "of" */
%token OR             /* "or" */
%token REC            /* "rec" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TRY            /* "try" */
%token TYPE           /* "type" */
%token WHILE          /* "while" */
%token WITH           /* "with" */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc FUNCTION WITH
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESSMINUS
%right COLONEQUAL
%nonassoc AS
%right BAR
%nonassoc below_COMMA
%left COMMA
%nonassoc MINUSGREATER
%right OR BARBAR
%right AMPERSAND AMPERAMPER

%nonassoc below_EQUAL
%left INFIX0 EQUAL EQUALEQUAL
%right INFIX1
%right COLONCOLON
%left INFIX2 PLUS PLUSDOT MINUS MINUSDOT
%left INFIX3 STAR
%right INFIX4
%nonassoc prec_unary_minus
%nonassoc prec_constr_app
%nonassoc below_DOT
%left DOT
%right PREFIX
%right LIDENT UIDENT

%start implementation
%type <Syntax.impl_phrase list> implementation

%%

implementation:
  | structure EOF { $1 }

structure:
  | structure_tail { $1 }
  | seq_expr { [make_impl(Pimpl_expr $1)] }
  | seq_expr SEMISEMI structure_tail { make_impl(Pimpl_expr $1)::$3 }

structure_tail:
  | /* emtpy */ { [] }
  | SEMISEMI { [] }
  | SEMISEMI seq_expr structure_tail { make_impl(Pimpl_expr $2)::$3 }
  | SEMISEMI structure_item structure_tail { $2::$3 }
  | structure_item structure_tail { $1::$2 }

structure_item:
  | TYPE type_decl_list { make_impl(Pimpl_typedef $2) }
  | LET rec_flag let_binding_list { make_impl(Pimpl_letdef($2, $3)) }
  | EXCEPTION constr_decl { make_impl(Pimpl_excdef $2) }

/* type */

type_:
  | simple_type { $1 }
  | type_star_list { make_type_expression(Ptype_tuple(List.rev $1)) }
  | simple_type MINUSGREATER type_ { make_type_expression(Ptype_arrow($1, $3)) }

simple_type:
  | type_var { make_type_expression(Ptype_var $1) }
  | simple_type LIDENT { make_type_expression(Ptype_constr(Lident $2, [$1])) }
  | LIDENT { make_type_expression(Ptype_constr(Lident $1, [])) }
  | LPAREN type_ RPAREN { $2 }
  | LPAREN type_ COMMA type_comma_list RPAREN LIDENT { make_type_expression(Ptype_constr(Lident $6, $2::$4)) }

type_comma_list:
  | type_ COMMA type_comma_list { $1::$3 }
  | type_ { [$1] }

type_star_list:
  | type_star_list STAR simple_type { $3::$1 }
  | simple_type STAR simple_type { [$3; $1] }

/* type declaration */

type_decl_list:
  | type_decl { [$1] }
  | type_decl AND type_decl_list { $1::$3 }

type_decl:
  | type_vars LIDENT type_def { $2, $1, $3 }

type_vars:
  | LPAREN type_var_list RPAREN { $2 }
  | type_var { [$1] }
  | /* empty */ { [] }

type_var_list:
  | type_var COMMA type_var_list { $1::$3}
  | type_var { [$1] }

type_var:
  | QUOTE LIDENT { $2 }

type_def:
  | /* empty */ { Ptd_abstract }
  | EQUAL opt_bar constr_decl_list { Ptd_variant $3 }
  | EQUALEQUAL type_ { Ptd_alias $2 }

constr_decl_list:
  | constr_decl BAR constr_decl_list { $1::$3 }
  | constr_decl { [$1] }

constr_decl:
  | UIDENT OF type_ { $1, Some $3 }
  | UIDENT { $1, None }

/* expression */

additive:
  | PLUS { "+" }
  | PLUSDOT { "+." }

subtractive:
  | MINUS { "-" }
  | MINUSDOT { "-." }

opt_bar:
  | /* empty */ { () }
  | BAR { () }

rec_flag:
  | /* empty */ { false }
  | REC { true }

direction_flag:
  | TO { true }
  | DOWNTO { false }

mutable_flag:
  | /* empty */ { false }
  | MUTABLE { true }

seq_expr:
  | expr SEMI seq_expr { make_expr(Pexpr_sequence($1, $3)) }
  | expr SEMI { $1 }
  | expr %prec below_SEMI { $1 }

expr:
  | simple_expr { $1 }
  | simple_expr simple_expr_list { make_apply $1 $2 }
  | expr_comma_list %prec below_COMMA { make_expr(Pexpr_tuple(List.rev $1)) }
  /*| simple_expr expr LESSMINUS expr { make_expr (Pexpr_assign($1, $3)) }*/