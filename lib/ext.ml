(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

module Int = struct
  module Map = Map.Make (struct type t = int

                                let compare = compare end)
end

module List_ = struct
  let pp_list sep pp ppf = function
    | [] -> ()
    | [e] -> pp ppf e
    | e :: es -> (
        pp ppf e ;
        (* Must be efficient, code specialization. *)
        match sep with
        | "" ->
            List.iter
              (fun e ->
                Format.pp_print_space ppf () ;
                pp ppf e )
              es
        | "," ->
            List.iter
              (fun e ->
                Format.pp_print_char ppf ',' ;
                Format.pp_print_space ppf () ;
                pp ppf e )
              es
        | ";" ->
            List.iter
              (fun e ->
                Format.pp_print_char ppf ';' ;
                Format.pp_print_space ppf () ;
                pp ppf e )
              es
        | _ ->
            List.iter
              (fun e ->
                Format.pp_print_string ppf sep ;
                Format.pp_print_space ppf () ;
                pp ppf e )
              es )

  let to_string sep f = function
    | [] -> ""
    | hd :: tl ->
        let seplen = String.length sep in
        let rec aux len = function
          | [] -> Bytes.create len
          | hd :: tl ->
              let s = f hd in
              let slen = String.length s in
              let buf = aux (len + seplen + slen) tl in
              Bytes.blit_string sep 0 buf len seplen ;
              Bytes.blit_string s 0 buf (len + seplen) slen ;
              buf
        in
        let s = f hd in
        let slen = String.length s in
        let buf = aux slen tl in
        Bytes.blit_string s 0 buf 0 slen ;
        Bytes.to_string buf

  let rec fst_n acc n = function
    | [] -> (List.rev acc, [])
    | l when n = 0 -> (List.rev acc, l)
    | x :: l -> fst_n (x :: acc) (n - 1) l

  let fst_n n l = fst_n [] n l

  let copy n x =
    let rec aux accu n = if n = 0 then accu else aux (x :: accu) (n - 1) in
    aux [] n

  let rev_choose f l =
    let rec aux accu = function
      | [] -> accu
      | x :: xs -> (
        match f x with None -> aux accu xs | Some y -> aux (y :: accu) xs )
    in
    aux [] l

  let choose f l = List.rev (rev_choose f l)

  let range n m =
    let rec range acc m =
      if n >= m then acc
      else
        let m = pred m in
        range (m :: acc) m
    in
    range [] m

  let findi prop lst =
    let i = ref 0 in
    let rec f = function
      | [] -> raise Not_found
      | hd :: tl -> if prop hd then !i else ( incr i ; f tl )
    in
    f lst

  let rev_flatten_map f l =
    List.fold_left (fun acc e -> List.rev_append (f e) acc) [] l

  let rev_flatten l = rev_flatten_map (fun x -> x) l
  let flatten_map f l = List.rev (rev_flatten_map f l)
end

module Array_ = struct
  let pp_array sep pp ppf es =
    let open Format in
    let n = Array.length es in
    if n > 0 then (
      let e = Array.unsafe_get es 0 in
      pp ppf e ;
      for i = 1 to n - 1 do
        let e = Array.unsafe_get es i in
        pp_print_char ppf sep ; pp_print_space ppf () ; pp ppf e
      done )

  let map_to_list f t = Array.fold_right (fun el acc -> f el :: acc) t []

  let of_list_rev = function
    | [] -> [||]
    | hd :: _ as l ->
        let i = ref (List.length l) in
        let t = Array.make !i hd in
        List.iter (fun e -> decr i ; t.(!i) <- e) l ;
        t

  let of_list_map f = function
    | [] -> [||]
    | x :: xs ->
        let n = List.length xs in
        let a = Array.make (n + 1) (f x) in
        let rec aux i = function
          | [] -> a
          | x :: xs ->
              Array.unsafe_set a i (f x) ;
              aux (i + 1) xs
        in
        aux 1 xs

  let of_list_mapi f = function
    | [] -> [||]
    | x :: xs ->
        let n = List.length xs in
        let a = Array.make (n + 1) (f 0 x) in
        let rec aux i = function
          | [] -> a
          | x :: xs ->
              Array.unsafe_set a i (f i x) ;
              aux (i + 1) xs
        in
        aux 1 xs
end

module Float = struct
  (* TODO: this float printing is very slow. *)
  
  (* from ocaml/typing/oprint.ml *)
  let valid_float_lexeme s =
    let l = String.length s in
    let rec loop i =
      if i >= l then s ^ "."
      else match s.[i] with '0' .. '9' | '-' -> loop (i + 1) | _ -> s
    in
    loop 0

  let repres f =
    match classify_float f with
    | FP_nan -> "nan"
    | FP_infinite -> if f < 0.0 then "neg_infinity" else "infinity"
    | _ ->
        let float_val =
          let s1 = Printf.sprintf "%.12g" f in
          if f = float_of_string s1 then s1
          else
            let s2 = Printf.sprintf "%.15g" f in
            if f = float_of_string s2 then s2 else Printf.sprintf "%.18g" f
        in
        valid_float_lexeme float_val

  let pp_repres ppf f = Format.pp_print_string ppf (repres f)
end

module String = struct
  let cut_start i s =
    let l = String.length s in
    if l <= i then "" else String.sub s i (l - i)

  let cut_end i s =
    let l = String.length s in
    if l <= i then "" else String.sub s 0 (l - i)

  let rec sub_equal_aux_case_sensitive pat i s j n =
    n = 0
    || String.unsafe_get pat i = String.unsafe_get s j
       && sub_equal_aux_case_sensitive pat (i + 1) s (j + 1) (n - 1)

  let rec sub_equal_aux_case_insensitive pat i s j n =
    n = 0
    || Char.uppercase_ascii (String.unsafe_get pat i)
       = Char.uppercase_ascii (String.unsafe_get s j)
       && sub_equal_aux_case_insensitive pat (i + 1) s (j + 1) (n - 1)

  let sub_equal_aux ?(case_insensitive = false) pat i s j n =
    if case_insensitive then sub_equal_aux_case_insensitive pat i s j n
    else sub_equal_aux_case_sensitive pat i s j n

  let sub_equal ?case_insensitive ~pattern s i =
    0 <= i
    && i + String.length pattern <= String.length s
    && sub_equal_aux ?case_insensitive pattern 0 s i (String.length pattern)

  let string_start ?case_insensitive ~pattern s =
    sub_equal ?case_insensitive ~pattern s 0

  let string_end ?case_insensitive ~pattern s =
    sub_equal ?case_insensitive ~pattern s
      (String.length s - String.length pattern)

  let buffer_add_unicode_escape =
    let conv = "0123456789abcdef" in
    fun b c ->
      Buffer.add_char b '\\' ;
      Buffer.add_char b 'u' ;
      Buffer.add_char b conv.[(c lsr 12) land 0xf] ;
      Buffer.add_char b conv.[(c lsr 8) land 0xf] ;
      Buffer.add_char b conv.[(c lsr 4) land 0xf] ;
      Buffer.add_char b conv.[c land 0xf]

  (* Code taken from js_of_ocaml. *)
  let js_string_escaping ?(utf8 = false) s =
    let l = String.length s in
    let b = Buffer.create (4 * l) in
    let i = ref 0 in
    while !i < l do
      let c = s.[!i] in
      ( match c with
      (*  '\000' when i = l - 1 || s.[i + 1] < '0' || s.[i + 1] > '9' ->
            Buffer.add_string b "\\0" *)
      | '\b' -> Buffer.add_string b "\\b"
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      (* This escape sequence is not supported by IE < 9
            | '\011' ->
                Buffer.add_string b "\\v"
        *)
      | '\012' -> Buffer.add_string b "\\f"
      | '\r' -> Buffer.add_string b "\\r"
      | '\"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\000' .. '\031' | '\'' | '\127' .. '\255' ->
          let c =
            if utf8 then (
              let cp = Utf8.next s i in
              decr i ; Uchar.to_int cp )
            else Char.code c
          in
          if c <= 0xFFFF then buffer_add_unicode_escape b c
          else
            let c = c - 0x1_0000 in
            let high_surrogate = ((c lsr 10) land 0b11_1111_1111) + 0xD800 in
            let low_surrogate = (c land 0b11_1111_1111) + 0xDC00 in
            buffer_add_unicode_escape b high_surrogate ;
            buffer_add_unicode_escape b low_surrogate
      | _ -> Buffer.add_char b c ) ;
      incr i
    done ;
    Buffer.contents b

  let hex_digit = function
    | '0' .. '9' as c -> Char.code c - Char.code '0'
    | 'a' .. 'f' as c -> 10 + (Char.code c - Char.code 'a')
    | 'A' .. 'F' as c -> 10 + (Char.code c - Char.code 'A')
    | _ -> raise (Invalid_argument "of_hex")

  let char_to_hex_const =
    [| "00"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09"; "0a"; "0b"
     ; "0c"; "0d"; "0e"; "0f"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"
     ; "18"; "19"; "1a"; "1b"; "1c"; "1d"; "1e"; "1f"; "20"; "21"; "22"; "23"
     ; "24"; "25"; "26"; "27"; "28"; "29"; "2a"; "2b"; "2c"; "2d"; "2e"; "2f"
     ; "30"; "31"; "32"; "33"; "34"; "35"; "36"; "37"; "38"; "39"; "3a"; "3b"
     ; "3c"; "3d"; "3e"; "3f"; "40"; "41"; "42"; "43"; "44"; "45"; "46"; "47"
     ; "48"; "49"; "4a"; "4b"; "4c"; "4d"; "4e"; "4f"; "50"; "51"; "52"; "53"
     ; "54"; "55"; "56"; "57"; "58"; "59"; "5a"; "5b"; "5c"; "5d"; "5e"; "5f"
     ; "60"; "61"; "62"; "63"; "64"; "65"; "66"; "67"; "68"; "69"; "6a"; "6b"
     ; "6c"; "6d"; "6e"; "6f"; "70"; "71"; "72"; "73"; "74"; "75"; "76"; "77"
     ; "78"; "79"; "7a"; "7b"; "7c"; "7d"; "7e"; "7f"; "80"; "81"; "82"; "83"
     ; "84"; "85"; "86"; "87"; "88"; "89"; "8a"; "8b"; "8c"; "8d"; "8e"; "8f"
     ; "90"; "91"; "92"; "93"; "94"; "95"; "96"; "97"; "98"; "99"; "9a"; "9b"
     ; "9c"; "9d"; "9e"; "9f"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"
     ; "a8"; "a9"; "aa"; "ab"; "ac"; "ad"; "ae"; "af"; "b0"; "b1"; "b2"; "b3"
     ; "b4"; "b5"; "b6"; "b7"; "b8"; "b9"; "ba"; "bb"; "bc"; "bd"; "be"; "bf"
     ; "c0"; "c1"; "c2"; "c3"; "c4"; "c5"; "c6"; "c7"; "c8"; "c9"; "ca"; "cb"
     ; "cc"; "cd"; "ce"; "cf"; "d0"; "d1"; "d2"; "d3"; "d4"; "d5"; "d6"; "d7"
     ; "d8"; "d9"; "da"; "db"; "dc"; "dd"; "de"; "df"; "e0"; "e1"; "e2"; "e3"
     ; "e4"; "e5"; "e6"; "e7"; "e8"; "e9"; "ea"; "eb"; "ec"; "ed"; "ee"; "ef"
     ; "f0"; "f1"; "f2"; "f3"; "f4"; "f5"; "f6"; "f7"; "f8"; "f9"; "fa"; "fb"
     ; "fc"; "fd"; "fe"; "ff" |]

  let char_to_hex c = Array.unsafe_get char_to_hex_const (Char.code c)

  let to_hex s =
    let len = String.length s in
    let res = Bytes.create (len * 2) in
    for i = 0 to len - 1 do
      Bytes.blit_string (char_to_hex s.[i]) 0 res (2 * i) 2
    done ;
    Bytes.unsafe_to_string res

  let hex_in_string s i =
    Char.chr ((hex_digit s.[i] lsl 4) + hex_digit s.[i + 1])

  let of_hex s =
    let len = String.length s in
    if len mod 2 = 1 then raise (Invalid_argument "of_hex") ;
    let len = len / 2 in
    let res = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set res i (hex_in_string s (2 * i))
    done ;
    Bytes.unsafe_to_string res

  let url_encode ?(is_uri_component = false) s =
    let n = String.length s in
    let b = Buffer.create (n * 2) in
    for i = 0 to n - 1 do
      match s.[i] with
      | ( 'A' .. 'Z'
        | 'a' .. 'z'
        | '0' .. '9'
        | '-' | '_' | '.' | '~' (* url_special_chars *) | '!' | '*' | '\''
        | '(' | ')' ) as c
      (* url_reserved_chars allowed in uri components*) ->
          Buffer.add_char b c
      | ( '$' | '&' | '+' | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '#' | '['
        | ']' ) as c
      (* url_reserved_chars encoded in uri_components*)
        when not is_uri_component ->
          Buffer.add_char b c
      | c -> Buffer.add_string b (String.uppercase_ascii ("%" ^ char_to_hex c))
    done ;
    Buffer.contents b

  let url_decode ?(is_query_string_value = false) s =
    let n = String.length s in
    let rec simple_string i =
      if i < n then
        match s.[i] with
        | '%' | '+' ->
            let buf = Buffer.create n in
            Buffer.add_substring buf s 0 i ;
            decode_string buf i
        | _ -> simple_string (i + 1)
      else s
    and decode_string buf i =
      if i < n then (
        match s.[i] with
        | '+' when is_query_string_value ->
            Buffer.add_char buf ' ' ;
            decode_string buf (i + 1)
        | '%' when i + 2 < n -> (
          match hex_in_string s (i + 1) with
          | s ->
              Buffer.add_char buf s ;
              decode_string buf (i + 3)
          | exception Invalid_argument _ ->
              Buffer.add_char buf s.[i] ;
              decode_string buf (i + 1) )
        | c ->
            Buffer.add_char buf c ;
            decode_string buf (i + 1) )
      else Buffer.contents buf
    in
    simple_string 0

  module Tbl = struct
    (* TODO: heuristic to delay building the dispatch table until N lookups occured
       (use simple list search before)? *)
    
    (* A slightly faster, but much less readable, implementation is in public/tests/strtbl.mf. *)

    type tree =
      | Node of {pos: int; first: int; sub: tree array}
      | Leaf of int * string
      | Fail

    (* Decision tree (pos is the index to look up). *)

    type t = tree array

    (* dispatch on the string's length *)
    
    (* Dispatching *)

    let rec eval_tree s = function
      | Fail -> -1
      | Leaf (i, s2) -> if s = s2 then i else -1
      | Node {pos; first; sub} ->
          let c = Char.code (String.unsafe_get s pos) in
          if c < first then -1
          else
            let i = c - first in
            if i >= Array.length sub then -1
            else eval_tree s (Array.unsafe_get sub i)

    let lookup trees s =
      let len = String.length s in
      if len >= Array.length trees then -1
      else eval_tree s (Array.unsafe_get trees len)

    (* Preparation *)

    let split_at strings i =
      let buckets = Array.make 256 [] in
      let min_char = ref 256 in
      let max_char = ref (-1) in
      let rec loop = function
        | ((_, s) as x) :: tl ->
            let c = Char.code s.[i] in
            if c > !max_char then max_char := c ;
            if c < !min_char then min_char := c ;
            buckets.(c) <- x :: buckets.(c) ;
            loop tl
        | [] -> ()
      in
      loop strings ;
      (!min_char, !max_char, buckets)

    let score (min_char, max_char, buckets) =
      let max_len = ref 0 in
      for i = min_char to max_char do
        let l = List.length buckets.(i) in
        if l > !max_len then max_len := l
      done ;
      !max_len

    let rec split idxs = function
      | [(i, s)] -> Leaf (i, s)
      | [] -> Fail
      | strings ->
          let best_score = ref max_int in
          let best_idx = ref (-1) in
          let best_split = ref (0, 0, [||]) in
          let rec loop = function
            | i :: rest ->
                let res = split_at strings i in
                let score = score res in
                if score < !best_score then (
                  best_score := score ;
                  best_idx := i ;
                  best_split := res ) ;
                loop rest
            | [] -> ()
          in
          loop idxs ;
          let pos = !best_idx in
          let first, last, buckets = !best_split in
          let idxs = List.filter (( != ) pos) idxs in
          (* optim *)
          Node
            { pos
            ; first
            ; sub=
                Array.init
                  (last - first + 1)
                  (fun i -> split idxs buckets.(i + first)) }

    let prepare strings : t =
      let rec max_len acc = function
        | [] -> acc
        | hd :: tl -> max_len (max acc (String.length hd)) tl
      in
      let max_len = max_len 0 strings in
      let buckets = Array.make (max_len + 1) [] in
      let rec dispatch i = function
        | [] -> ()
        | hd :: tl ->
            let len = String.length hd in
            buckets.(len) <- (i, hd) :: buckets.(len) ;
            dispatch (i + 1) tl
      in
      dispatch 0 strings ;
      Array.mapi
        (fun len strings ->
          let idxs = List_.range 0 len in
          split idxs strings )
        buckets
  end
end

module Option_ = struct
  let bind f = function None -> None | Some x -> f x
  let map f = function None -> None | Some x -> Some (f x)

  let value_exn = function
    | None -> raise (Invalid_argument "Expected Some, got None")
    | Some x -> x
end

module List = List_
module Array = Array_
module Option = Option_
