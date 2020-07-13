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
  if w < -32768 || 32767 < 