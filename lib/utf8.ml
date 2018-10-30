(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

(*
  This file is a simplified version of the UTF8 module found in CDUCE:
  http://www.cduce.org/cgi-bin/viewcvs.cgi/cduce/trunk/misc/encodings.ml?revision=1956&view=markup
*)

let store b p =
  (* Adapted from Netstring's netconversion.ml/write_utf8 *)
  if p <= 127 then Buffer.add_char b (Char.chr p)
  else if p <= 0x7ff then (
    Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6))) ;
    Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f))) )
  else if p <= 0xffff then (
    (* Refuse writing surrogate pairs, and fffe, ffff *)
    if (p >= 0xd800 && p < 0xe000) || p >= 0xfffe then
      failwith "Encodings.Utf8.store" ;
    Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12))) ;
    Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f))) ;
    Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f))) )
  else if p <= 0x10ffff then (
    Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18))) ;
    Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f))) ;
    Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f))) ;
    Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f))) )
  else
    (* Higher code points are not possible in XML: *)
    failwith "Encodings.Utf8.store"

let rec mk_latin1_aux buf s i n =
  if i = n then ()
  else (
    store buf (Char.code s.[i]) ;
    mk_latin1_aux buf s (succ i) n )

let of_latin1 s =
  let b = Buffer.create (String.length s) in
  mk_latin1_aux b s 0 (String.length s) ;
  Buffer.contents b

let next s i =
  let fail () = failwith "Malformed UTF-8 buffer" in
  let check i =
    let n = Char.code s.[i] in
    if n lsr 6 <> 0b10 then fail () else n
  in
  try
    match s.[!i] with
    | '\000' .. '\127' as c ->
        let n = Char.code c in
        i := !i + 1 ;
        Uchar.of_int n
    | '\192' .. '\223' as c ->
        let n1 = Char.code c in
        let n2 = check (!i + 1) in
        let n = ((n1 land 0b11111) lsl 6) lor (n2 land 0b111111) in
        i := !i + 2 ;
        Uchar.of_int n
    | '\224' .. '\239' as c ->
        let n1 = Char.code c in
        let n2 = check (!i + 1) in
        let n3 = check (!i + 2) in
        let n =
          ((n1 land 0b1111) lsl 12)
          lor ((n2 land 0b111111) lsl 6)
          lor (n3 land 0b111111)
        in
        i := !i + 3 ;
        Uchar.of_int n
    | '\240' .. '\247' as c ->
        let n1 = Char.code c in
        let n2 = check (!i + 1) in
        let n3 = check (!i + 2) in
        let n4 = check (!i + 3) in
        let n =
          ((n1 land 0b111) lsl 18)
          lor ((n2 land 0b111111) lsl 12)
          lor ((n3 land 0b111111) lsl 6)
          lor (n4 land 0b111111)
        in
        i := !i + 4 ;
        Uchar.of_int n
    | _ -> fail ()
  with Invalid_argument _ -> fail ()

let to_string_aux buf s i n =
  let i = ref i in
  while !i <> n do
    let c = Uchar.to_int (next s i) in
    if c <= 255 then Buffer.add_char buf (Char.chr c)
    else Printf.bprintf buf "\\%i;" c
    (* or fail? *)
  done

let to_latin1 s =
  let l = String.length s in
  let b = Buffer.create l in
  to_string_aux b s 0 l ; Buffer.contents b

let is_valid s =
  let off = ref 0 in
  try
    while !off < String.length s do
      ignore (next s off)
    done ;
    true
  with Failure _ -> false
