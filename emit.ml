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
    let 