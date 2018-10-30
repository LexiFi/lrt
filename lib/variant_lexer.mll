(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

{

open Lexing

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Tuple of t list
  | List of t list
  | Array of t array
  | Option of t option
  | Record of (string * t) list
  | Constructor of string * t option
  | Variant of t
  | Lazy of t Lazy.t

type token =
  | BARRBRACKET
  | CHAR of char
  | COMMA
  | EQUAL
  | EOF
  | IN
  | LAZY
  | LBRACE
  | LBRACKET
  | LBRACKETBAR
  | LET
  | LIDENT of string
  | ATOM of t
  | LPAREN
  | RBRACKET
  | RBRACE
  | RPAREN
  | SEMI
  | STRING of string
  | UIDENT of string
  | SOME
  | VARIANT

let pos_line pos = pos.pos_lnum

let pos_char pos = pos.pos_cnum - pos.pos_bol

let print_position lexbuf =
  let pstart = lexeme_start_p lexbuf in
  if lexbuf.lex_curr_p == Lexing.dummy_pos then
    (* Positions not tracked.  From variant_of_string, could either
       reparse, or count lines directly in the input string *)
    Printf.sprintf "Characters %i-%i:" lexbuf.lex_start_pos lexbuf.lex_curr_pos
  else
    let pend = lexeme_end_p lexbuf in
    let file =
      if pstart.pos_fname = "" then None else Some pstart.pos_fname
    in
    (* If there is a file to be parsed as a variant, print the name of the file
       and the line of the error (so print the first letter of
       "line" in lowercase)
       If it is at toplevel, do not print any file name
       (so print the first letter of "line" in uppercase) *)
    let file_s, line_s = match file with
      | None -> "", "L"
      | Some f -> Printf.sprintf "File %S, " f , "l"
    in
    Printf.sprintf "%s%sine %i, characters %i-%i:" file_s line_s (pos_line pstart) (pos_char pstart) (pos_char pend)

let print_position_string str lexbuf =
  let start = lexbuf.lex_start_pos + lexbuf.lex_abs_pos in
  let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
  let lineno = ref 1 and bol = ref 0 in
  for i = 0 to start - 1 do
    if str.[i] = '\n' then (incr lineno; bol := i)
  done;
  Printf.sprintf "Line %i, characters %i-%i:" !lineno (start - !bol) (start - !bol + len)

type error =
  | Unterminated_string
  | Unterminated_string_in_comment
  | Unterminated_comment
  | Illegal_escape of string
  | Literal_overflow of string
  | Illegal_date_value
  | Illegal_character of char
  | Syntax_error

exception Internal_error of error

let pp_error = function
  | Unterminated_string ->
      "String literal not terminated"
  | Unterminated_string_in_comment ->
      "String literal not terminated in comment"
  | Unterminated_comment ->
      "Comment not terminated"
  | Illegal_escape s ->
      Printf.sprintf "Illegal backslash escape in string or character (%s)" s
  | Literal_overflow ty ->
      Printf.sprintf "Integer literal exceeds the range of representable integers of type %s" ty
  | Illegal_date_value ->
      "Illegal date value"
  | Illegal_character c ->
      Printf.sprintf "Illegal character %C" c
  | Syntax_error ->
      "Syntax error"

exception Error of {msg:string; text:string; loc:string}

let print_marker_error_in_variant variant (lexbuf : Lexing.lexbuf) =
  let truncate_len = 100 in
  let pstart = lexbuf.lex_start_pos in
  let pend = lexbuf.lex_curr_pos in
  if String.length variant < 1024 then
    String.concat ""
      [
       (try String.sub variant 0 pstart with _ -> variant);
       "[[!ERROR!";
       (try String.sub variant pstart (pend - pstart) with _ -> "");
       "!ERROR!]]";
       Ext.String.cut_start pend variant;
      ]
  else
    let trunc_start = if pstart - truncate_len < 0 then 0 else pstart - truncate_len in
    let trunc_end =
      if pend + truncate_len > String.length variant then
        (String.length variant) - pend
      else
        truncate_len
    in
    String.concat ""
      [
       " ... ";
       String.sub variant trunc_start truncate_len;
       "[[!ERROR!";
       String.sub variant pstart (pend - pstart);
       "!ERROR!]]";
       String.sub variant pend trunc_end;
       " ... "
      ]


(* Registers a custom printer which will be used by the application
   to print the exception raised by the variant lexer and parser *)
let () =
  Printexc.register_printer
    (function
      | Error {msg;text;loc} ->
          Some (Printf.sprintf "%s\nError: %s\n\nCannot parse variant:\n%s\n=====\n" loc msg text)
      | _ ->
          None
    )


(* Some instrumentation in preparation of #4668 *)

let v_record x = Record x
let v_constructor name arg = Constructor (name, arg)
let v_option_some x = Option (Some x)
let v_option_none = Option None

let _v_option = function
  | None -> v_option_none
  | Some x -> v_option_some x

let v_bool_true = Bool true
let v_bool_false = Bool false
let _v_string_empty = String ""
let v_string s = String s

let v_lazy v = Lazy v
let v_variant v = Variant v

let _v_bool = function
  | true -> v_bool_true
  | false -> v_bool_false

let v_list x = List x
let v_tuple x = Tuple x

let _v_array_empty = Array [| |]
let _v_list_empty = List [ ]
let v_array a = Array a
let v_int n = Int n
let v_unit = Unit
let v_float f = Float f

module StringTable = Weak.Make(struct
  type t = string
  let hash = Hashtbl.hash
  let equal (x : string) y = x = y
end)

let string_table = StringTable.create 512
let empty_string = ""
let intern_string s =
    let l = String.length s in
    if l = 0 then empty_string
    else if l < 30 then StringTable.merge string_table s
    else s

let a_nan = ATOM (v_float nan)
let a_inf = ATOM (v_float infinity)
let a_neginf = ATOM (v_float neg_infinity)
let a_true = ATOM v_bool_true
let a_false = ATOM v_bool_false
let a_zero = ATOM (v_float 0.)
let a_one = ATOM (v_float 1.)
let a_hundred = ATOM (v_float 100.)
let a_thousand = ATOM (v_float 1000.)


let float_of_string  =
  let n = 100 in
  let last_s = Array.make n "" and last_f = Array.make n (ATOM (v_float 0.)) in
  fun s ->
    let h = Hashtbl.hash s mod n in
    if last_s.(h) = s then last_f.(h)
    else begin
      last_s.(h) <- s;
      let r = ATOM (v_float (float_of_string s)) in
      last_f.(h) <- r;
      r
    end

let _sub2 lexbuf i =
  let i = i + lexbuf.lex_start_pos in
  10 * Char.code (Bytes.unsafe_get lexbuf.lex_buffer i)
  + Char.code (Bytes.unsafe_get lexbuf.lex_buffer (i + 1))
  - (10 * 48 + 48)

(* Update the current location with file name and line number. *)

let update_loc lexbuf chars =
  let pos = lexbuf.lex_curr_p in
  if pos != Lexing.dummy_pos
  then
    lexbuf.lex_curr_p <-
      { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = pos.pos_cnum - chars;
      }


(* To buffer string literals *)

let string_buff = Buffer.create 256

let reset_string_buffer () =
  Buffer.clear string_buff

let store_string_char c =
  Buffer.add_char string_buff c

let store_string_bytes b start len =
  Buffer.add_subbytes string_buff b start len

let get_stored_string () =
  let s = Buffer.contents string_buff in
  Buffer.clear string_buff;
  s

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    raise (Internal_error(Illegal_escape (Lexing.lexeme lexbuf)))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let _lexeme_size lexbuf =
  lexbuf.lex_curr_pos - lexbuf.lex_start_pos
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let base64_litteral =
   ['A'-'Z' 'a'-'z' '0'-'9' '+' '/']+

let string_char = ['\000'-'\009' '\011' '\012' '\014'-'\033' '\035'-'\091' '\093'-'\255']
(* Normal chars in strings: exclude backslash \092, double quote \034, and newline chars *)

let digit = ['0'-'9']

let date_literal =
  digit digit digit digit
  '-'
  digit digit
  '-'
  digit digit
  (
   'T' digit digit (* hour *)
   ':' digit digit (* minutes *)
   (
    ':' digit digit (* seconds *)
   )? (* optional seconds *)
   (
    "GMT" ['+' '-']digit digit? (* hr displacement *)
   )? (* optional GMT displacement *)
  )? (* optional time *)


rule token = parse
| newline
    { update_loc lexbuf 0;
      token lexbuf
    }
| blank +
    { token lexbuf }
| lowercase identchar* {
    match Lexing.lexeme lexbuf with
    | "lazy" -> LAZY
    | "let" -> LET
    | "variant" -> VARIANT
    | "in" -> IN
    | "true" -> a_true
    | "false" -> a_false
    | "nan" -> a_nan
    | "infinity" -> a_inf
    | "neg_infinity" -> a_neginf
    | s -> LIDENT s
  }
| uppercase identchar* {
    match Lexing.lexeme lexbuf with
    | "Some" -> SOME
    | "None" -> ATOM v_option_none
    | s -> UIDENT s
  }
| '-'? decimal_literal
    {
      if lexbuf.lex_curr_pos - lexbuf.lex_start_pos <= 9 then begin
        (* no overflow checking here... *)
        let i = ref lexbuf.lex_start_pos in
        let n = ref 0 in
        let stop = lexbuf.lex_curr_pos in
        let buf = lexbuf.lex_buffer in
        let neg = if Bytes.unsafe_get buf !i = '-' then (incr i; true) else false in
        while !i < stop do
           let c = Bytes.unsafe_get buf !i in
           if c <> '_' then n := !n * 10 + Char.code c - 48;
           incr i
        done;
       ATOM (v_int (if neg then -(!n) else !n))
      end else
        try
        ATOM (v_int (int_of_string (Lexing.lexeme lexbuf)))
        with Failure _ ->
          raise (Internal_error (Literal_overflow "int"))
    }
| '-'? (float_literal | hex_float_literal) {
    match Lexing.lexeme lexbuf with
    | "0." -> a_zero
    | "1." -> a_one
    | "100." -> a_hundred
    | "1000." -> a_thousand
    | s -> float_of_string s
}

| "\""
    {
      reset_string_buffer();
      string lexbuf
    }
| "'" newline "'"
    { update_loc lexbuf 1;
      CHAR (Lexing.lexeme_char lexbuf 1) }
| "'" [^ '\\' '\'' '\010' '\013'] "'"
    { CHAR(Lexing.lexeme_char lexbuf 1) }
| "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
  { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
| "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { CHAR(char_for_decimal_code lexbuf 2) }
| "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { CHAR(char_for_hexadecimal_code lexbuf 3) }
| "'\\" _
    { let l = Lexing.lexeme lexbuf in
    let esc = String.sub l 1 (String.length l - 1) in
    raise (Internal_error (Illegal_escape esc))
     }
| "(*"
    { comment 0 lexbuf;
      token lexbuf }
| "(*)"
    {
      comment 0 lexbuf;
      token lexbuf
     }
| "*)"
      {
       lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
       let curpos = lexbuf.lex_curr_p in
       if curpos != Lexing.dummy_pos then
         lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
       token lexbuf
      }
| "="       { EQUAL }
| ","       { COMMA }
| "[|"      { LBRACKETBAR }
| "["       { LBRACKET }
| "|]"      { BARRBRACKET }
| "]"       { RBRACKET }
| ";"       { SEMI }
| "{"       { LBRACE }
| "}"       { RBRACE }
| "("       { LPAREN }
| ")"       { RPAREN }
| eof       { EOF }
| _  {
    let c = Lexing.lexeme_char lexbuf 0 in
    raise (Internal_error (Illegal_character c))
   }

and comment start = parse
    "(*"
      { comment (start + 1) lexbuf }
  | "*)"
      { if start = 0 then ()
        else comment (start - 1) lexbuf
      }
  | "\""
      {
        reset_string_buffer();
        begin
          try ignore (string lexbuf)
          with Internal_error Unterminated_string ->
            raise (Internal_error Unterminated_string_in_comment)
        end;
        comment start lexbuf
      }
  | "''"
      { comment start lexbuf }
  | "'" newline "'"
      { update_loc lexbuf 1;
        comment start lexbuf
       }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { comment start lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { comment start lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment start lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { comment start lexbuf }
  | newline
      { update_loc lexbuf 0;
        comment start lexbuf
      }
  | eof
      { raise (Internal_error Unterminated_comment) }
  | _
      { comment start lexbuf }


and string = parse
| '"'
    {
      STRING (get_stored_string ())
    }
| '\\' newline ([' ' '\t'] * as space)
    { update_loc lexbuf (String.length space);
      string lexbuf
     }
| '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
    { store_string_char (char_for_backslash(Lexing.lexeme_char lexbuf 1));
      string lexbuf }
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { store_string_char (char_for_decimal_code lexbuf 1);
      string lexbuf }
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { store_string_char (char_for_hexadecimal_code lexbuf 2);
      string lexbuf }
| '\\' _
    {
     (*  Should be an error, but we are very lax.
         raise (Internal_error (Illegal_escape (Lexing.lexeme lexbuf)))
      *)
      store_string_char (Lexing.lexeme_char lexbuf 0);
      store_string_char (Lexing.lexeme_char lexbuf 1);
      string lexbuf
     }
| newline
    {
      update_loc lexbuf 0;
      store_string_bytes lexbuf.lex_buffer lexbuf.lex_start_pos (lexbuf.lex_curr_pos - lexbuf.lex_start_pos);
      string lexbuf
     }
| eof
    { raise (Internal_error Unterminated_string) }
| string_char+  "\"" (* treat standard chars in bulk mode *)
    {
      (* fast track to avoid double copying *)
      if Buffer.length string_buff = 0 then
        STRING (Bytes.sub_string lexbuf.lex_buffer lexbuf.lex_start_pos (lexbuf.lex_curr_pos - lexbuf.lex_start_pos - 1))
      else begin
        store_string_bytes lexbuf.lex_buffer lexbuf.lex_start_pos (lexbuf.lex_curr_pos - lexbuf.lex_start_pos - 1);
        STRING (get_stored_string ())
      end
    }
| string_char+  (* treat standard chars in bulk mode *)
    {
      store_string_bytes lexbuf.lex_buffer lexbuf.lex_start_pos (lexbuf.lex_curr_pos - lexbuf.lex_start_pos);
      string lexbuf
    }


{
let arg_start = function
  | ATOM _ | CHAR _ | STRING _ | LIDENT _ | UIDENT _
  | LBRACE | LBRACKET | LPAREN | LBRACKETBAR -> true
  | _ -> false

let variant_parser token lexbuf : t =
  (* TODO:
     - reject "C1 C2 C3"
  *)
  let env = ref [] in
  let tok = ref (token lexbuf) in
  let error () = raise (Internal_error Syntax_error) in
  let rec next () = tok := token lexbuf
  and variant0 = function
    | ATOM x -> next (); x
    | CHAR x -> next (); v_string (intern_string (String.make 1 x))
    | STRING x -> next (); v_string (intern_string x)
    | LBRACE ->
        let r = v_record (record ()) in
        next (); r
    | LBRACKET ->
        let r = v_list (list ()) in
        next (); r
    | LBRACKETBAR ->
        let r = v_array (Array.of_list (array ())) in
        next (); r
    | UIDENT s ->
        let t = token lexbuf in
        if arg_start t then v_constructor (intern_string s) (Some (variant0 t))
        else (tok := t; v_constructor (intern_string s) None)
    | SOME ->
        let t = token lexbuf in
        if arg_start t then v_option_some (variant0 t)
        else error ()
    | LPAREN ->
        begin match token lexbuf with
        | RPAREN -> next (); v_unit
        | t ->
            let v = variant1 t in
            begin match !tok with
            | RPAREN -> next (); v
            | _ -> error ()
            end
        end
    | VARIANT ->
        let t = token lexbuf in
        if arg_start t then v_variant (variant0 t)
        else error ()
    | LAZY ->
        let t = token lexbuf in
        if arg_start t then v_lazy (Lazy.from_val (variant0 t))
        else error ()
   | LET ->
        next ();
        begin match !tok with
        | LIDENT id ->
            next ();
            begin match !tok with
            | EQUAL ->
                env := (id, variant1 (token lexbuf)) :: !env;
                begin match !tok with
                | IN ->
                    let v2 = variant1 (token lexbuf) in
                    env := List.tl !env;
                    v2
                | _ -> error ()
                end
            | _ -> error ()
            end
        | _ -> error ()
        end
    | LIDENT s ->
        begin try List.assoc s !env
        with Not_found -> error ()
        end
    | _ -> error ()

  and variant1 t =
    let v = variant0 t in
    match !tok with
    | COMMA -> v_tuple (v :: variant_tuple ())
    | _ -> v
  and variant_tuple () =
    let v = variant0 (token lexbuf) in
    match !tok with
    | COMMA -> v :: variant_tuple ()
    | _ -> [v]

  and record () =
    match token lexbuf with
    | RBRACE -> []
    | LIDENT lab ->
        begin match token lexbuf with
        | EQUAL ->
            let r = (intern_string lab, variant1 (token lexbuf)) in
            begin match !tok with
            | SEMI -> r :: record ()
            | RBRACE -> [r]
            | _ -> error ()
            end
        | _ -> error ()
        end
    | _ -> failwith "Syntax error"
  and list () =
    match token lexbuf with
    | RBRACKET -> []
    | t ->
        let r = variant1 t in
        begin match !tok with
        | SEMI -> r :: list ()
        | RBRACKET -> [r]
        | _ -> error ()
        end
  and array () =
    match token lexbuf with
    | BARRBRACKET -> []
    | t ->
        let r = variant1 t in
        begin match !tok with
        | SEMI -> r :: array ()
        | BARRBRACKET -> [r]
        | _ -> error ()
        end
  in
  let r = variant1 !tok in
  match !tok with
  | EOF -> r
  | _ -> error ()


let variant_of_string s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- Lexing.dummy_pos; (* disable pos tracking, which is expensive *)
  try
    variant_parser token lexbuf
  with
  | Internal_error err ->
      let loc = print_position_string s lexbuf in
      raise (Error {msg=pp_error err;text=print_marker_error_in_variant s lexbuf;loc})

let variant_of_file file_name =
  let f = open_in file_name in
  let x =
    try
      let lexbuf = Lexing.from_channel f in
      try
        variant_parser token lexbuf
      with
      | Internal_error err ->
          let loc = print_position lexbuf in
          raise (Error {msg=pp_error err;text=Printf.sprintf "(file %s)" file_name;loc})
    with exn -> close_in f; raise exn
  in
  close_in f;
  x



}
