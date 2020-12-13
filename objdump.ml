
open Cprim
open Emit
open Exe
open Opcode

exception Invalid

let jumptbl = Array.make (opXORINT+1) (fun _ _ -> ())
let relocatable = ref false

let init_jumptbl () =
  let i8 ic _ =
    let b = input_byte ic in
    Printf.printf "%d" (if b >= 128 then b-256 else b) in
  let u8 ic _ =
    Printf.printf "%d" (input_byte ic) in
  let prim ic _ =
    if !relocatable then
      Printf.printf "%d" (input_byte ic)
    else
      Printf.printf "%s" name_of_prims.(input_byte ic) in
  let i16 ic pos =
    if pos mod 2 <> 0 then
      input_byte ic |> ignore;
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    let n =
      if n >= 32768 then n-65536
      else n
    in
    Printf.printf "%d" n
  in
  let u16 ic pos =
    if pos mod 2 <> 0 then
      input_byte ic |> ignore;
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    Printf.printf "[%d]" n
  in
  let rel16 ic pos =
    let pos =
      if pos mod 2 <> 0 then (
        input_byte ic |> ignore;
        pos+1
      ) else
        pos
    in
    let b0 = input_byte ic in
    let b1 = input_byte ic in
    let n = b0 + b1 lsl 8 in
    let n =
      if n >= 32768 then n-65536
      else n
    in
    Printf.printf "0x%04x" (pos+n)
  in
  let makearray ic pos =
    let init = input_byte ic in
    if init <> 0 then
      print_string "init"
  in
  let makeblock ic pos =
    if pos mod Config.sizeof_word <> 0 then
      for i = pos mod Config.sizeof_word to Config.sizeof_word-1 do
        input_byte ic |> ignore
      done;
    Printf.printf "0x%08lx" (input_bin_int32 ic)
  in
  let branchifneqtag ic pos =
    Printf.printf "%d " (input_byte ic);
    rel16 ic (pos+1)
  in
  let switch ic pos =
    let nalts = input_byte ic in
    let pos =
      if (pos+1) mod 2 <> 0 then (
        input_byte ic |> ignore;
        pos+2
      ) else
        pos+1
    in
    print_char '[';
    for i = 1 to nalts do
      let b0 = input_byte ic in
      let b1 = input_byte ic in
      let n = b0 + b1 lsl 8 in
      let n =
        if n >= 32768 then n-65536
        else n
      in
      if i > 1 then
        print_string "; ";
      Printf.printf "%04x" (pos+n);
    done;
    print_char ']'
  in
  jumptbl.(opACCESS) <- u8;
  jumptbl.(opATOM) <- u8;
  jumptbl.(opBRANCH) <- rel16;
  jumptbl.(opBRANCHIF) <- rel16;
  jumptbl.(opBRANCHIFEQ) <- rel16;
  jumptbl.(opBRANCHIFGE) <- rel16;
  jumptbl.(opBRANCHIFGT) <- rel16;
  jumptbl.(opBRANCHIFLE) <- rel16;
  jumptbl.(opBRANCHIFLT) <- rel16;
  jumptbl.(opBRANCHIFNEQ) <- rel16;
  jumptbl.(opBRANCHIFNOT) <- rel16;
  jumptbl.(opCCALL1) <- prim;
  jumptbl.(opCCALL2) <- prim;
  jumptbl.(opCCALL3) <- prim;
  jumptbl.(opCCALL4) <- prim;
  jumptbl.(opCONSTINT8) <- i8;
  jumptbl.(opCONSTINT16) <- i16;
  jumptbl.(opCUR) <- rel16;
  jumptbl.(opDUMMY) <- u8;
  jumptbl.(opENDLET) <- u8;
  jumptbl.(opGETFIELD) <- u8;
  jumptbl.(opGETGLOBAL) <- u16;
  jumptbl.(opMAKEARRAY) <- makearray;
  jumptbl.(opMAKEBLOCK) <- makeblock;
  jumptbl.(opBRANCHIFNEQTAG) <- branchifneqtag;
  jumptbl.(opPUSHTRAP) <- rel16;
  jumptbl.(opSETFIELD) <- u8;
  jumptbl.(opSETGLOBAL) <- u16;
  jumptbl.(opSWITCH) <- switch;
  jumptbl.(opUPDATE) <- u8

let print_code ic len =
  let start = pos_in ic in
  let stop = start+len in
  while pos_in ic < stop do
    Printf.printf "%04x: " (pos_in ic);
    let op = input_byte ic in
    Printf.printf "%s " name_of_opcodes.(op);
    jumptbl.(op) ic (pos_in ic);
    print_char '\n'
  done

let print_phr_entry ic phr =
  Printf.printf "\nOffset %d\nLength %d\n" phr.cph_pos phr.cph_len;
  seek_in ic phr.cph_pos;
  print_code ic phr.cph_len

let iiii ic =
  let bs = Array.init 4 (fun _ -> input_byte ic |> Int32.of_int) in
  Array.fold_right (fun b acc -> Int32.(add (mul acc 256l) b)) bs 0l

let iiii' ic =
  let bs = Array.init 8 (fun _ -> input_byte ic |> Int64.of_int) in
  Array.fold_right (fun b acc -> Int64.(add (mul acc 256L) b)) bs 0L
