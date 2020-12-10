
open Error
open Location
open Parser
open Pretty
open Lexer
open Syntax
open Implementation

let pretty = ref false
let width = ref 72
let compile_only = ref false

let dbg_lexer lexbuf =
  match Lexer.main lexbuf with
  | EOF -> "EOF"

  | CHAR c -> "CHAR " ^ String.escaped (String.make 1 c)
  | INT i -> "INT " ^ string_of_int i
  | FLOAT f -> "FLOAT " ^ string_of_float f
  | STRING s -> "STRING(" ^ String.escaped s ^ ")"
  | LIDENT id -> "LIDENT(" ^ id ^ ")"
  | UIDENT id -> "UIDENT(" ^ id ^ ")"
  | PREFIX op -> "PREFIX " ^ op
  | INFIX0 op -> "INFIX0 " ^ op
  | INFIX1 op -> "INFIX1 " ^ op
  | INFIX2 op -> "INFIX2 " ^ op
  | INFIX3 op -> "INFIX3 " ^ op
  | INFIX4 op -> "INFIX4 " ^ op
  | PLUS -> "+"
  | PLUSDOT -> "+."
  | MINUS -> "-"
  | MINUSDOT -> "-."

  | EQUAL -> "="
  | EQUALEQUAL -> "=="
  | LPAREN -> "("
  | RPAREN -> ")"
  | STAR -> "*"
  | COMMA -> ","
  | MINUSGREATER -> "->"
  | DOT -> "."
  | COLON -> ":"
  | COLONCOLON -> "::"
  | COLONEQUAL -> ":="
  | SEMI -> ";"
  | SEMISEMI -> ";;"
  | LBRACKET -> "["
  | LBRACKETBAR -> "[|"
  | LESSMINUS -> "<-"
  | RBRACKET -> "]"
  | QUOTE -> "'"
  | UNDERSCORE -> "_"
  | BAR -> "|"
  | BARRBRACKET -> "|]"
  | RBRACE -> "}"
  | AMPERSAND -> "&"
  | AMPERAMPER -> "&&"
  | BARBAR -> "||"
  | AND -> "and"
  | AS -> "as"
  | BEGIN -> "begin"
  | DO -> "do"
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | END -> "end"
  | FALSE -> "false"
  | FOR -> "for"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | IF -> "if"
  | IN -> "in"
  | LET -> "let"
  | MATCH -> "match"
  | MUTABLE -> "mutable"
  | OF -> "of"
  | OR -> "or"
  | REC -> "rec"
  | THEN -> "then"
  | TO -> "to"
  | TRUE -> "true"
  | TRY -> "try"
  | TYPE -> "type"
  | WITH -> "with"

let compile_file f =
  if !verbose then
    Printf.printf "Compiling %s\n" f;
  let basename = Filename.chop_suffix f ".ml" in
  let objfile = basename ^ ".zo" in
  let ic = open_in_bin f in
  Location.input_chan := ic;
  let lexbuf = Lexing.from_channel ic in
  begin match !stage with
  | 0 ->
      let rec go () =
        let s = dbg_lexer lexbuf in
        Printf.printf "%s " s;
        if s <> "EOF" then
          go ()
      in
      go ()
  | 1 ->
      let ast = Parser.implementation Lexer.main lexbuf in
      List.iter (Syntax.dump_impl_phrase 0) ast
  | _ ->
      try
        let impls = Parser.implementation Lexer.main lexbuf in
        if !pretty then
          pprint_implementation !width impls
        else
          compile_implementation objfile impls
      with Lexical_error(err, l) ->
        (*Printf.eprintf "character %d-%d" l m;*)
        begin match err with
        | Bad_char_constant ->
            Printf.eprintf "%aIll-formed character literal\n"
            output_location l
        | Illegal_character ch ->
            Printf.eprintf "%aIllegal character %c\n"
            output_location l
            ch
        | Unterminated_comment ->
            Printf.eprintf "%aComment not terminated\n"
            output_location l
        | Unterminated_string ->
            Printf.eprintf "%aString literal not terminated\n"
            output_location l
        end;
        exit 1
      | Failure l ->
          prerr_endline "Syntax error";
          exit 1
  end;
  close_in ic;
  objfile

let () =
  let program_desc = "camlfwc [-c] [-o exefile] *.ml\n\
    Caml Featherweight compiler\n\n\
    camlfwc compiles Caml source files (*.ml) and produces relocatable\n\
    object files (*.zo). If \"-c\" option is not given, camlfwc will\n\
    link those object files to produce a bytecode executable file.\n"
  in
  let files = ref [] in
  let exe = ref "a.out" in
  let opts =
    [ "-c", Arg.Unit(fun () -> compile_only := true), "compile only"