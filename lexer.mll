
{
open Error
open Parser
open Syntax

let keyword_tbl = Hashtbl.create 31;;

List.iter (fun (str,tok) -> Hashtbl.replace keyword_tbl str tok) [
  "and", AND;
  "as", AS;
  "asr", INFIX4 "asr";
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  "exception", EXCEPTION;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  "if", IF;
  "in", IN;
  "land", INFIX3 "land";
  "lor", INFIX3 "lor";
  "lsl", INFIX4 "lsl";
  "lsr", INFIX4 "lsr";
  "lxor", INFIX3 "lxor";
  "let", LET;
  "match", MATCH;
  "mod", INFIX3 "mod";
  "of", OF;
  "or", OR;
  "rec", REC;
  "then", THEN;