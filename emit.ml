open Error
open Exe
open Lambda
open Instruction
open Opcode
open Syntax

(* code buffer *)

let out_buf = ref (Bytes.create 256)
let out_pos = ref 0

let o b =
  let len = Bytes.length !out_buf in
  if !out_pos >= len then (
    let newbuf = Bytes.create (len*2) in
    Bytes.blit !out_buf 0 newbuf 0 len;
    out_buf := newbuf
  );
  Bytes.set !out_buf !out_pos (b land 255 |> char_of_int);
  incr out_pos

let oo w =
  if w < -32768 || 32767 < w then
    displacement_overflow ()
  else (
    o w;
    o (w lsr 8)
  )

let oooo w =
  oo (w land 65535);
  oo (w lsr 16 land 65535)

let out_test_int = function
  | Peq -> opEQ
  | Pneq -> opNEQ
  | Plt -> opLTINT
  | Ple -> opLEINT
  | Pgt -> opGTINT
  | Pge -> opGEINT
  | _ -> assert false

let out_test_int_b = function
  | Peq -> opBRANCHIFEQ
  | Pneq -> opBRANCHIFNEQ
  | Plt -> opBRANCHIFLT
  | Ple -> opBRANCHIFLE
  | Pgt -> opBRANCHIFGT
  | Pge -> opBRANCHIFGE
  | _ -> assert false

let out_test_float = function
  | Peq -> opEQFLOAT
  | Pneq -> opNEQFLOAT
  | Plt -> opLTFLOAT
  | Ple -> opLEFLOAT
  | Pgt -> opGTFLOAT
  | Pge -> opGEFLOAT
  | _ -> assert false

let out_test_string = function
  | Peq -> opEQSTRING
  | Pneq -> opNEQSTRING
  | Plt -> opLTSTRING
  | Ple -> opLESTRING
  | Pgt -> opGTSTRING
  | Pge -> opGESTRING
  | _ -> assert false

(* label *)

type label_def =
  | Label_defined of int
  | Label_undefined of (int * int) list

let label_tbl = ref [||]
let extend_label_tbl l =
  let len = Array.length !label_tbl in
  let newtbl = Array.make ((l/len+1)*len) (Label_undefined []) in
  for i = 0 to len-1 do
    newtbl.(i) <- (!label_tbl).(i)
  done;
  label_tbl := newtbl

let define_label l =
  if l >= Array.length !label_tbl then
    extend_label_tbl l;
  match (!label_tbl).(l) with
  | Label_defined _ ->
      fatal_error "define_label: already defined"
  | Label_undefined ls ->
      let curr_pos = !out_pos in
      (!label_tbl).(l) <- Label_defined curr_pos;
      List.iter (fun (pos,orig) ->
        out_pos := pos;
        oo (curr_pos-orig)
      ) ls;
      out_pos := curr_pos

(* orig must be aligned by 2 *)
let out_label_with_orig orig l =
  if l >= Array.length !label_tbl then
    extend_label_tbl l;
  match (!label_tbl).(l) with
  | Label_defined pos ->
      oo (pos-orig)
  | Label_undefined ls ->
      (!label_tbl).(l) <- Label_undefined ((!out_pos,orig)::ls);
      oo 0

let out_label l =
  if !out_pos mod 2 <> 0 then
    o 0;
  out_label_with_orig !out_pos l

(* relocation *)

type reloc_entry =
  | Reloc_const of constant
  | Reloc_getglobal of long_ident
  | R