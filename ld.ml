
open Emit
open Error
open Exe
open Opcode
open Syntax

exception Invalid

let global_tbl = Hashtbl.create 257
let global_tbl_used = ref 0
let const_tbl = Hashtbl.create 257
let prim_tbl = Hashtbl.create 257
let tag_tbl = Hashtbl.create 257

let make_slot_for_const c =
  try
    Hashtbl.find const_tbl c
  with Not_found ->
    let s = !global_tbl_used in
    if s >= 65536 then (
      prerr_endline "Used more than 65536 global table slots.";
      exit 1
    );
    incr global_tbl_used;
    Hashtbl.replace const_tbl c s;
    if !Implementation.verbose then
      Printf.printf "%s: slot %d of global table\n" (show_constant c) s;
    s

let make_slot_for_global id =
  try
    Hashtbl.find global_tbl id
  with Not_found ->
    let s = !global_tbl_used in
    if s >= 65536 then (
      prerr_endline "Used more than 65536 global table slots.";
      exit 1
    );
    incr global_tbl_used;
    Hashtbl.replace global_tbl id s;
    if !Implementation.verbose then
      Printf.printf "%s: slot %d of global table\n" (string_of_long_ident id) s;
    s

let make_slot_for_tag (id,stamp as tag) =
  try
    Hashtbl.find tag_tbl tag
  with Not_found ->
    let s = Hashtbl.length tag_tbl in
    if s >= 256 then (
      prerr_endline "Used more than 65536 tag table slots.";
      exit 1
    );
    Hashtbl.replace tag_tbl tag s;
    if !Implementation.verbose then
      Printf.printf "%s,%d: slot %d of tag table\n" (string_of_long_ident id)
      stamp s;
    s

let get_slot_for_global id =
  try
    Hashtbl.find global_tbl id
  with Not_found ->
    Printf.eprintf "The global value \"%s\" is undefined.\n" (string_of_long_ident id);
    exit 1

let get_num_of_prim name =
  try
    Hashtbl.find prim_tbl name
  with Not_found ->
    Printf.eprintf "The C primitive \"%s\" is not available.\n" name;
    exit 1

let dump_data oc =
  let pos = ref 0 in
  let buf = ref (Bytes.create 16) in
  let o u8 =
    let len = Bytes.length !buf in
    if !pos >= len then (
      let newbuf = Bytes.create (2*len) in
      Bytes.blit !buf 0 newbuf 0 !pos;
      buf := newbuf
    );
    Bytes.set !buf !pos (Int32.(logand u8 255l |> to_int) |> char_of_int);
    incr pos
  in
  let o' u8 =
    let len = Bytes.length !buf in
    if !pos >= len then (
      let newbuf = Bytes.create (2*len) in
      Bytes.blit !buf 0 newbuf 0 !pos;
      buf := newbuf