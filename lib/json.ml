(***************************************************************************)
(*  Copyright (C) 2000-2018 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(* Inspired by Bryan O'Sullivan's aeson library. *)

open Xtype
open Std

module Parsec: sig
  type 'a parser

  val return: 'a -> 'a parser

  val (<$>): ('a -> 'b) -> 'a parser -> 'b parser
  val (<*>): ('a -> 'b) parser -> 'a parser -> 'b parser
  val (>>=): 'a parser -> ('a -> 'b parser) -> 'b parser
  val (@): 'a parser -> 'b parser -> 'b parser
  val (<|>): 'a parser -> 'a parser -> 'a parser

  val satisfy: (char -> bool) -> char parser
  val char: char -> unit parser
  val string: string -> unit parser
  val skip_space: unit parser
  val sep_by: unit parser -> 'a parser -> 'a list parser
  val comma: unit parser
  val colon: unit parser

  val run: ?filename:string -> 'a parser -> string -> [`KO of string | `OK of 'a]

  module State :
  sig
    type t
    val eof: t -> bool
    val peek: t -> char
    val step: t -> unit
    val read: t -> int -> string
    val index: t -> int
    val buf: t -> string
    val tail_len: t -> int
    val jump: t -> int -> unit
  end

  type 'a direct = {f: 'b. State.t -> ('a -> 'b) -> (unit -> 'b) -> 'b}
  val direct: 'a direct -> 'a parser
end = struct
  module State =
  struct
    type t =
      {
        buf: string;
        mutable idx: int;
        mutable eof: bool;
      }

    let start buf =
      {buf; idx = 0; eof = String.length buf = 0}

    let index {idx; _} = idx

    let eof {eof; _} = eof

    let peek {buf; idx; _} = String.unsafe_get buf idx

    let sub {buf; _} start length =
      let start = max 0 start in
      let length = min length (String.length buf - start) in
      String.sub buf start length

    let step ({buf; idx; _} as s) =
      s.idx <- idx + 1;
      s.eof <- String.length buf = s.idx

    let read ({buf; idx; _} as s) length =
      let res = String.sub buf idx length in
      s.idx <- idx + length;
      s.eof <- String.length buf = s.idx;
      res

    let jump s idx = s.idx <- idx

    let buf {buf; _} = buf

    let tail_len {buf; idx; _} = String.length buf - idx

    let pos {buf; idx; _} =
      let len = String.length buf in
      if len = 0 then
        (0, 0)
      else begin
        let line = ref 0 in
        let bol = ref 0 in
        for i = 0 to min (len - 1) idx do
          let c = buf.[i] in
          if c = '\n' || (c = '\r' && (i+1 >= len || buf.[i+1] <> '\n')) then
            (incr line; bol := i)
        done;
        let col = idx - !bol in
        !line, col
      end
  end

  type 'a direct = {f: 'b. State.t -> ('a -> 'b) -> (unit -> 'b) -> 'b}

  type 'a parser =
    | Return: 'a -> 'a parser
    | Map: ('a -> 'b) * 'a parser -> 'b parser
    | App: ('a -> 'b) parser * 'a parser -> 'b parser
    | Bind: 'b parser * ('b -> 'a parser) -> 'a parser
    | Many: 'a parser -> 'a list parser
    | Seq: 'b parser * 'a parser -> 'a parser
    | Choice: 'a parser * 'a parser -> 'a parser
    | Satisfy: (char -> bool) -> char parser
    | Char: char -> unit parser
    | SkipWhile: (char -> bool) -> unit parser
    | Direct: 'a direct -> 'a parser

  let return x = Return x
  let (<$>) f p = Map (f, p)
  let (<*>) p q = App (p, q)
  let (>>=) p f = Bind (p, f)
  let (@) p q = Seq (p, q)
  let (<|>) p q = Choice (p, q)
  let char c = Char c
  let satisfy pred = Satisfy pred
  let skip_while pred = SkipWhile pred
  let direct x = Direct x

  let run ?filename p s =
    let t = State.start s in
    let rec eval: type t. t parser -> t = function
      | Return x ->
        x
      | Map (f, p) ->
        f (eval p)
      | App (p, q) ->
        let f = eval p in
        f (eval q)
      | Many p ->
        let rec loop acc =
          match eval p with
          | exception Exit -> List.rev acc
          | r -> loop (r :: acc)
        in
        loop []
      | Bind (p, f) ->
        eval (f (eval p))
      | Seq (p, q) ->
        ignore (eval p); eval q
      | Choice (p, q) ->
        let i0 = State.index t in
        begin try eval p with
          | Exit as exn ->
            if i0 = State.index t then
              eval q
            else
              raise exn
        end
      | Char c ->
        if State.eof t then
          raise_notrace Exit
        else
          let d = State.peek t in
          if c = d then State.step t
          else raise_notrace Exit
      | Satisfy pred ->
        if State.eof t then
          raise_notrace Exit
        else
          let d = State.peek t in
          if pred d then (State.step t; d)
          else raise_notrace Exit
      | SkipWhile pred ->
        while not (State.eof t) && pred (State.peek t) do
          State.step t
        done
      | Direct {f} ->
        f t (fun x -> x) (fun () -> raise_notrace Exit)
    in
    try `OK (eval p) with
      Exit ->
      let line, col = State.pos t in
      let s = Printf.sprintf "line %i, character %i" line col in
      let s =
        match filename with
        | None -> s
        | Some fn -> "file \"" ^ Filename.basename fn ^ "\", " ^ s
      in
      let c = if State.eof t then "end of input" else String.make 1 (State.peek t) in
      `KO (
        "Parser error at " ^ s ^ ": unexpected " ^ c ^
        " in \"" ^ State.sub t (State.index t - 5) 10 ^ "\"")

  let string s =
    direct
      {f = fun t succ fail ->
          let open State in
          let i_start = index t in
          let i_end = i_start + String.length s in
          if i_end > String.length (buf t) then
            fail ()
          else begin
            while index t < i_end && s.[index t - i_start] = peek t do
              step t
            done;
            if index t = i_end then
              succ ()
            else
              fail ()
          end
      }

  let many p = Many p

  let is_space = function
    | ' ' | '\t' .. '\r' -> true
    | _ -> false

  let skip_space = skip_while is_space

  let sep_by1 sep p =
    p >>= fun x ->
    many (sep @ p) >>= fun xs ->
    return (x :: xs)

  let sep_by sep p =
    sep_by1 sep p <|> return []

  let comma = char ','

  let colon = char ':'
end

type number = I of int | F of float
[@@deriving t]

type value =
  | Null
  | Bool of bool
  | Number of number
  | String of string
  | Array of value list
  | Object of (string * value) list
[@@deriving t]

type ctx = { to_json_field: string -> string; }

let ctx ?(to_json_field=fun x -> x) () = { to_json_field }

let empty_ctx = ctx ()

let json_unit = Object []

type 'a custom_json =
  { to_json: 'a -> value
  ; of_json: value -> 'a
  }

module Matcher = Matcher.Make (struct type 'a t = 'a custom_json end)

let matcher = Matcher.create ~modulo_props:true

let register_custom ~t conv = Matcher.add matcher ~t conv
let register_custom_0 m = Matcher.add0 matcher m
let register_custom_1 m = Matcher.add1 matcher m
let register_custom_2 m = Matcher.add2 matcher m

(* OPTIMs:
   - memoize the function (for a given set of flags)
     in DT_node
*)

let variant_xt = Xtype.of_ttype [%t: Variant.t]

let to_json ?(ctx=empty_ctx) ~t =
  let rec to_json: type a. a Xtype.t -> a -> value = fun t ->
    let open Matcher in
    match apply matcher ~t:(t.t) with
    | Some (M0 (module M : M0 with type matched = a)) ->
      let TypEq.Eq = M.eq in M.data.to_json
    | Some (M1 (module M : M1 with type matched = a)) ->
      let TypEq.Eq = M.eq in M.data.to_json
    | Some (M2 (module M : M2 with type matched = a)) ->
      let TypEq.Eq = M.eq in M.data.to_json
    | None -> to_json_xtype (Lazy.force t.xt)

  and to_json_xtype: type a. a xtype -> a -> value = fun t ->
    match t with
    | Unit -> failwith "to_json: missing matcher candidate"
    | Bool -> failwith "to_json: missing matcher candidate"
    | Int -> failwith "to_json: missing matcher candidate"
    | Float -> failwith "to_json: missing matcher candidate"
    | String -> failwith "to_json: missing matcher candidate"
    | List _ -> failwith "to_json: missing matcher candidate"
    | Array _ -> failwith "to_json: missing matcher candidate"
    | Lazy _ -> failwith "to_json: missing matcher candidate"
    | Prop (_, t) -> to_json t
    | Option t -> fun x ->
      (* TODO: push x deeper *)
      begin match Lazy.force (remove_outer_props t).xt, x with
        | Option _, None -> Object ["type", String "None"]
        | Option t, Some (Some x) ->
          Object ["type", String "Some"; "val", to_json t x]
        | Option _, Some None ->
          Object ["type", String "Some"]
        | _, None -> Null
        | _, Some x -> to_json t x
      end
    (* TODO: push x deeper *)
    | Tuple tup -> fun x -> Array (Fields.map_tuple tup dyn x)
    | Record r -> fun x -> Object (Fields.map_record r dyn_named x)
    | Sum s -> fun x ->
      (* TODO: mlfi_json has special handling for Variant.t and Json.t at
         this point. This might be mirrored using the new Matcher module. *)
      let name, obj_flds = match s.s_cstr_by_value x with
        | Constant c -> fst c.cc_label, []
        | Regular c ->
          let l = Fields.map_regular c dyn x
          in fst c.rc_label, ["val", Array l]
        | Inlined c ->
          fst c.ic_label, Fields.map_inlined c dyn_named x
      in Object (("type", String name) :: obj_flds)
    | Function _ -> failwith "Json: functions not supported"
    (* TODO: avoid indirection via variant *)
    | Char -> fun x -> to_json variant_xt (Variant.to_variant ~t:[%t: char] x)
    | Int32 -> fun x -> to_json variant_xt (Variant.to_variant ~t:[%t: int32] x)
    | Int64 -> fun x -> to_json variant_xt (Variant.to_variant ~t:[%t: int64] x)
    | Nativeint -> fun x -> to_json variant_xt (Variant.to_variant ~t:[%t: nativeint] x)
    | Object _ -> failwith "Json: objects not supported"
    | Abstract _ ->
      failwith "TODO: abstract handling + fallback to variant"

  and dyn (Fields.Dyn (t,x)) =
    to_json t x

  and dyn_named ~name (Fields.Dyn (t,x)) =
    ctx.to_json_field name, to_json t x

  in to_json (Xtype.of_ttype t)

module Reader: sig
  type 'a t
  val mk: (string * 'a) list -> 'a t
  val get: 'a t -> string -> 'a -> 'a
end = struct
  type 'a t = (string * 'a) list ref

  let mk l = ref l

  (* Optimized lookup for record fields, for the case where the ordering
     match between the JSON value and the OCaml record definition. *)

  let get (r : 'a t) s default =
    match !r with
    | (t, v) :: rest when t = s -> r := rest; v
    | [] -> default
    | _ :: rest ->
        (* could fallback to building a lookup table here... *)
        try List.assoc s rest
        with Not_found -> default
end

let of_json ?(ctx=empty_ctx) ~t =
  let rec of_json: type a. a Xtype.t -> string list -> value -> a = fun t ->
    let open Matcher in
    match apply matcher ~t:(t.t) with
    | Some (M0 (module M : M0 with type matched = a)) ->
      let TypEq.Eq = M.eq in fun _ -> M.data.of_json
    | Some (M1 (module M : M1 with type matched = a)) ->
      let TypEq.Eq = M.eq in fun _ -> M.data.of_json
    | Some (M2 (module M : M2 with type matched = a)) ->
      let TypEq.Eq = M.eq in fun _ -> M.data.of_json
    | None -> fun path -> of_json_xt (Lazy.force t.xt) path

  and of_json_xt: type t. t xtype -> string list -> value -> t = fun t path ->
    match t with
    | Option t ->
      let of_json = of_json t ("(Some)" :: path) in
      begin match Lazy.force (remove_outer_props t).xt with
        | Option _ -> fun v ->
          begin match v with
            | Object l ->
              let constr, arg =
                match l with
                | [ "type", String ty; "val", v ] -> ty, v
                | [ "type", String ty ] -> ty, Null
                | _ ->
                  get_constr l,
                  try List.assoc "val" l
                  with Not_found -> Null
              in
              begin match constr with
                | "None" -> None
                | "Some" -> Some (of_json arg)
                | _ ->
                  Print.show ~t:value_t v;
                  failwith "Nested option, 'type' field should be Some or None"
              end
            | _ ->
              Print.show ~t:value_t v;
              failwith "Type/value mismatch"
          end
        | _ ->
          begin function
            | Null -> None
            | v -> Some (of_json v)
          end
      end
    (* TODO: push function deeper *)
    | Tuple tup -> (function
        | Array l -> Builder.tuple tup (field_builder ("(_/_)" :: path) l)
        | _ -> failwith "tuple expected")
    | Record r -> (function
        | Object l -> Builder.record r (record_field_builder path l)
        | _ -> failwith "object expected" )
    | Sum sum -> (function
        | Object l ->
          (* TODO: mlfi_json has special handling for Variant.t and Json.t at
             this point. This might be mirrored using the new Matcher module. *)
          let name, arg =
            match l with
            | [ "type", String ty; "val", v ] -> ty, v
            | ("type", String ty) :: rest -> ty, Object rest
            | _ ->
              get_constr l,
              try List.assoc "val" l
              with Not_found -> Object (List.remove_assoc "type" l)
          in
          let c = match sum.s_lookup name with
            | Some c -> c
            | None -> failwith (Printf.sprintf "Unexpected constructor %S" name)
          in begin match c, arg with
            | Constant c, _ -> Builder.constant_constructor c
            | Regular c, Array l ->
              Builder.regular_constructor c
                (field_builder (("(" ^ name ^ ")") :: path) l)
            | Inlined c, Object l ->
              Builder.inlined_constructor c
                (record_field_builder (("(" ^ name ^ ")") :: path) l)
            | _ -> failwith "TODO: what happened here?"
          end
        | _ -> failwith "object expected")
    | Lazy t ->
      let of_json = of_json t ("lazy" :: path) in
      fun v -> lazy (of_json v)
    | Prop (_, t) -> of_json t path
    | Function _ -> failwith "Mlfi_json: functions not supported"
    | Abstract _ ->
      failwith "TODO: abstract handling with variant fallback"
    | _ -> failwith "of_json_xt: missing candidate in matcher"

  and field_builder: type a. string list -> value list -> a Builder.t =
    fun path lst ->
      (* we assume that the calls to mk happen in correct order *)
      let lref = ref lst in
      let mk {typ; _} =
        match !lref with
        | [] -> failwith "tuple length mismatch"
        | hd :: tl -> lref := tl; of_json typ path hd
      in {mk}

  and record_field_builder:
    type a. string list -> (string * value) list -> a Builder.t' =
    fun path lst ->
      let r = Reader.mk lst in
      let mk (name, _) el =
        of_json el.typ (name :: path)
          (Reader.get r (ctx.to_json_field name) Null)
      in {mk}

  and get_constr l =
    match List.assoc "type" l with
    | String constr -> constr
    | exception Not_found ->
      Print.show ~t:[%t: (string * value) list] l;
      failwith "No 'type' field in object of sum type"
    | _ ->
      Print.show ~t:[%t: (string * value) list] l;
      failwith "'type' field is not a string"

  in fun x -> of_json (of_ttype t) [] x

let () =
  let of_json = function
    | Number (I x) -> x
    | Number (F x) -> int_of_float x
    | _ -> failwith "integer expected"
  and to_json i = Number (I i) in
  register_custom ~t:int_t {of_json; to_json}

let () =
  let of_json = function
    | Number (F x) -> x
    | Number (I x) -> float_of_int x
    | String "Infinity" -> infinity
    | String "-Infinity" -> neg_infinity
    | String "NaN" -> nan
    | _ -> failwith "float expected"
  and to_json x =
    match classify_float x with
    | FP_infinite when x < 0. -> String "-Infinity"
    | FP_infinite -> String "Infinity"
    | FP_nan -> String "NaN"
    | _ -> Number (F x)
  in
  register_custom ~t:float_t {of_json; to_json}

let () =
  let to_json () = json_unit
  and of_json = function
    | Object _ -> ()
    | _ -> failwith "unit object expected"
  in
  register_custom ~t:unit_t {of_json; to_json}

let () =
  let to_json b = Bool b
  and of_json = function
    | Bool b -> b
    | _ -> failwith "boolean expected"
  in
  register_custom ~t:bool_t {of_json; to_json}

let () =
  let to_json s = String s
  and of_json = function
    | String x -> x
    | _ -> failwith "string expected"
  in
  register_custom ~t:string_t {of_json; to_json}

let () =
  register_custom_1 (module struct
    type 'a t = 'a list [@@deriving t]
    let data (t: 'a Ttype.t) =
      let to_json_el = to_json ?ctx:None ~t in
      let of_json_el = of_json ?ctx:None ~t in
      let to_json l =
        Array (List.map to_json_el l)
      and of_json = function
        | Array l -> List.map of_json_el l
        | _ -> failwith "array/list expected"
      in
      {of_json; to_json}
  end)

let () =
  register_custom_1 (module struct
    type 'a t = 'a array [@@deriving t]
    let data (t: 'a Ttype.t) =
      let to_json_el = to_json ?ctx:None ~t in
      let of_json_el = of_json ?ctx:None ~t in
      let to_json l =
        Array (Ext.Array.map_to_list to_json_el l)
      and of_json = function
        | Array l -> Ext.Array.of_list_map of_json_el l
        | _ -> failwith "array/list expected"
      in
      {of_json; to_json}
  end)

open Buffer

let utf8_mode = ref false

let encode_string b s =
  add_char b '\"';
  add_string b
    (if !utf8_mode then Ext.String.js_string_escaping ~utf8:true s
     else Ext.String.js_string_escaping s
    );
  add_char b '\"'

let encode_collection b open_ close enc = function
  | [] ->
    add_char b open_;
    add_char b close
  | x :: xs ->
    add_char b open_;
    enc b x;
    List.iter (fun x -> add_char b ','; enc b x) xs;
    add_char b close

let encode_float x =
  if x <> x then
    "NaN"
  else
    let s = Ext.Float.repres x in
    if Ext.String.string_end ~pattern:"." s then s ^ "0" else s

let rec encode b = function
  | Null -> add_string b "null"
  | Bool true -> add_string b "true"
  | Bool false -> add_string b "false"
  | Number (I n) -> add_string b (string_of_int n)
  | Number (F f) -> add_string b (encode_float f)
  | String s -> encode_string b s
  | Array a -> encode_array b a
  | Object o -> encode_object b o

and encode_array b l =
  encode_collection b '[' ']' encode l

and encode_object b l =
  let encode_prop _ (s,v) =
    encode_string b s;
    add_char b ':';
    encode b v
  in
  encode_collection b '{' '}' encode_prop l

let encode ?(utf8=false) v =
  let b = Buffer.create 1024 in
  utf8_mode := utf8;
  encode b v;
  Buffer.contents b

let float_of_substring s pos len =
  float_of_string (String.sub s pos len)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let number =
  let open Parsec in
  direct
    {f = fun t succ fail ->
        if State.eof t then
          fail ()
        else begin
          let i0 = State.index t in
          let sign = if State.peek t = '-' then (State.step t; false) else true in
          let i1 = State.index t in
          let n = ref 0 in
          while not (State.eof t) && is_digit (State.peek t) do
            n := !n * 10 + int_of_char (State.peek t) - 48;
            State.step t
          done;
          let exponent () =
            State.step t;
            if State.eof t then
              fail ()
            else begin
              let c = State.peek t in
              if c = '+' || c = '-' then State.step t;
              let index0 = State.index t in
              while not (State.eof t) && is_digit (State.peek t) do
                State.step t
              done;
              if index0 = State.index t then
                fail ()
              else
                succ (F (float_of_substring (State.buf t) i0 (State.index t - i0)))
            end
          in
          if i1 = State.index t then
            fail ()
          else if State.eof t then
            succ (I (if sign then !n else - !n))
          else if State.peek t = 'e' || State.peek t = 'E' then
            exponent ()
          else if State.peek t = '.' then begin
            State.step t;
            while not (State.eof t) && is_digit (State.peek t) do
              State.step t
            done;
            if State.eof t || (State.peek t <> 'e' && State.peek t <> 'E') then
              succ (F (float_of_substring (State.buf t) i0 (State.index t - i0)))
            else
              exponent ()
          end else
            succ (I (if sign then !n else - !n))
        end
    }

let read_unicode_escaping t =
  let open Parsec.State in
  let s = "0x" ^ read t 4 in
  int_of_string_opt s

let buffer_add_cp b cp =
  if !utf8_mode then
    Buffer.add_utf_8_uchar b (Uchar.of_int cp)
  else if cp <= 255 then
    Buffer.add_char b (char_of_int cp)
  else
    Printf.bprintf b "\\%i;" cp

let in_high_surrogate_range cp = 0xD800 <= cp && cp <= 0xDBFF
let in_low_surrogate_range cp = 0xDC00 <= cp && cp <= 0xDFFF

let json_parser =
  let open Parsec in
  let rec value = lazy (
         let num = (fun x -> Number x) <$> number in
         let string_full t succ fail =
           let open State in
           let b = Buffer.create 8 in
           let rec go esc = if eof t then fail () else begin
               let c = peek t in
               step t;
               if esc then
                 match c with
                 | '\"' -> Buffer.add_char b '\"'; go false
                 | '\\' -> Buffer.add_char b '\\'; go false
                 | '/' -> Buffer.add_char b '/'; go false
                 | 'b' -> Buffer.add_char b '\b'; go false
                 | 'f' -> Buffer.add_char b (Char.chr 0xc); go false
                 | 'n' -> Buffer.add_char b '\n'; go false
                 | 'r' -> Buffer.add_char b '\r'; go false
                 | 't' -> Buffer.add_char b '\t'; go false
                 | 'u' when tail_len t < 4 -> fail ()
                 | 'u' ->
                   begin match read_unicode_escaping t with
                     | Some cp when in_high_surrogate_range cp ->
                       if tail_len t >= 6 && read t 2 = "\\u" then begin
                         match read_unicode_escaping t with
                         | Some low_surrogate when in_low_surrogate_range low_surrogate ->
                           let high_cp = (cp - 0xD800) lsl 10 in
                           let low_cp = (low_surrogate - 0xDC00) in
                           buffer_add_cp b (high_cp + low_cp + 0x1_0000);
                           go false
                         | _ -> fail ()
                       end else
                         fail () (* No second code point *)
                     | Some cp when in_low_surrogate_range cp -> fail ()
                     | Some cp ->
                       buffer_add_cp b cp;
                       go false
                     | None -> fail ()
                   end
                 | _ -> Buffer.add_char b c; go false
               else
                 match c with
                 | '\"' -> succ (Buffer.contents b)
                 | '\\' -> go true
                 | _ ->
                   let i = ref (index t - 1) in
                   match Utf8.next (buf t) i with
                   | exception Failure _ -> fail ()
                   | cp ->
                     if !utf8_mode then Buffer.add_utf_8_uchar b cp
                     else begin
                       let cp = Uchar.to_int cp in
                       if cp <= 255 then Buffer.add_char b (Char.chr cp) else Printf.bprintf b "\\%i;" cp;
                     end;
                     if !i >= String.length (buf t) then fail () else (jump t !i; go false)
             end
           in
           go false
         in
         let string_rest =
           direct {f = fun t succ fail ->
               let open State in
               let i0 = index t in
               while not (eof t) && peek t <> '\"' && peek t <> '\\' && int_of_char (peek t) <= 127 do
                 step t
               done;
               if eof t then fail () else
               if peek t = '\"' then
                 (step t; succ (String.sub (buf t) i0 (index t - i0 - 1)))
               else
                 (jump t i0; string_full t succ fail)
             }
         in
         let jstring = char '\"' @ string_rest in
         let rec obj = lazy (
                (fun _ l _ _ -> Object l) <$>
                skip_space <*>
                sep_by (comma @ skip_space) (Lazy.force key_val) <*>
                char '}' <*>
                skip_space )
           and key_val = lazy (
             (fun s _ _ _ v _ -> s, v) <$>
             jstring <*>
             skip_space <*>
             colon <*>
             skip_space <*>
           (Lazy.force value) <*>
             skip_space )
           and array = lazy (
             (fun _ l _ _ _ -> Array l) <$>
             skip_space <*>
             sep_by (comma @ skip_space) ((Lazy.force value) >>= fun x -> skip_space @ return x) <*>
             skip_space <*>
             char ']' <*>
             skip_space )
           in
           let most =
             satisfy (fun c -> c = '{' || c = '[' || c = '\"' || c = 't' || c = 'f' || c = 'n') >>= fun c ->
             match c with
             | '{' -> Lazy.force obj
             | '[' -> Lazy.force array
             | '\"' -> (fun s -> String s) <$> string_rest
             | 't' -> string "rue" @ (return (Bool true))
             | 'f' -> string "alse" @ (return (Bool false))
             | 'n' -> string "ull" @ (return Null)
             | _ -> assert false
           in
           most <|> num )
    in
    skip_space @ (Lazy.force value)

let decode ?(utf8=false) ?filename s =
  utf8_mode := utf8;
  match Parsec.run ?filename json_parser s with
  | `KO s -> failwith s
  | `OK x -> x

let to_pretty_string x =
  utf8_mode := false;
  let buf = Buffer.create 10 in
  let emit_indent n =
    for _i = 0 to n-1 do
      Buffer.add_char buf ' '
    done
  in
  let emit_string s = Buffer.add_string buf s in
  let rec list_iter f = function
    | [] -> assert false
    | [x] -> f true x
    | x :: ((_ :: _) as xs) -> f false x; list_iter f xs
  in
  let rec aux d x =
    match x with
    | Null -> emit_string "null"
    | Bool true -> emit_string "true"
    | Bool false -> emit_string "false"
    | Number (I n) -> emit_string (string_of_int n)
    | Number (F x) -> emit_string (encode_float x)
    | String s -> encode_string buf s
    | Array [] -> emit_string "[]"
    | Array ((_ :: _) as vs) ->
      emit_string "[\n";
      list_iter (fun is_last v -> emit_indent (d + 2); aux (d + 2) v; if not is_last then emit_string ","; emit_string "\n") vs;
      emit_indent d; emit_string "]"
    | Object [] -> emit_string "{}"
    | Object ((_ :: _) as fields) ->
      emit_string "{\n";
      list_iter
        (fun is_last (name, v) ->
           emit_indent (d + 2);
           emit_string name;
           emit_string ": ";
           aux (d + 2 + String.length name + 2) v;
           if not is_last then emit_string ",";
           emit_string "\n"
        )
        fields;
      emit_indent d;
      emit_string "}"
  in
  aux 0 x;
  Buffer.contents buf

(*
let () =
  let module M = struct
    type 'a t = 'a Mlfi_sets_maps.StringMap.t
    let t = (Ttype.t_of: unit Mlfi_sets_maps.StringMap.t)

    let to_json ~(t:_ Ttype.t) ?ctx map =
      let f key value elts = (key, to_json ~t ?ctx value) :: elts in
      Object (Mlfi_sets_maps.StringMap.fold f map [])

    let of_json ~(t:_ Ttype.t) ?ctx = function
      | Object elts ->
        let f map (key, value) = Mlfi_sets_maps.StringMap.add key (of_json ~t ?ctx value) map in
        List.fold_left f Mlfi_sets_maps.StringMap.empty elts
      | _ -> failwith "Invalid JSON representation for StringMap"
  end
  in
  register_parametric_conversion (module M)
 *)

let of_get_params ?utf8 get_params =
  let try_json_decode s = try decode ?utf8 s with _ -> String s in
  match get_params with
  | ["$value", s] -> try_json_decode s
  | _ ->
    Object (
      List.map
        (fun (k, v) ->
           k, try_json_decode (Ext.String.url_decode ~is_query_string_value:true v)
        ) get_params
    )

let to_get_params ?utf8 x =
  match x with
  | Object fields ->
    List.map
      (fun (k, v) ->
         k, encode ?utf8 v |> Ext.String.url_encode ~is_uri_component:true
      ) fields
  | _ -> ["$value", encode ?utf8 x]
(*
let () =
  let s = "{\"hejsan\" : [  \"h\", \"e\"], \"x\": 56,\"z\": {\"bla\": false,   \"koko\":true}}" in
  match decode s with
  | `Failure s -> print_endline s
  | `Success x -> debug x
*)

(*
let () =
  let s =
"{\"results\":[{\"value\":[[\"ForwardCurve_NBP_Mid_M11\", 796321.0356289984 ]] } ]}"
  in
  debug (decode s)
*)

(*
let () =
  let s = "{\"y\" : true, \"z\" : false}" in
  debug (decode s)
*)

(*
let () =
  let s = "\"" ^ Utf8.of_latin1 "öhh! vad säger du nu då?" ^ "\"" in
  match decode s with
  | String s -> print_endline s
  | _ -> print_endline "error"
*)

let failwithf fmt =
  Printf.ksprintf failwith fmt

module Access = struct
  let get_field field = function
    | Object fields ->
      begin match List.assoc field fields with
        | exception Not_found ->
          failwithf "No field named '%s' found amongst fields '%s'" field (Ext.List.to_string ", " fst fields)
        | json -> json
      end
    | json -> failwithf "Try to access a field of a non object node: %s" (to_pretty_string json)

  let to_int = function
    | Number (I i) -> i
    | json -> failwithf "Not an integer: %s" (to_pretty_string json)

  let to_float = function
    | Number (I i) -> float i
    | Number (F f) -> f
    | json -> failwithf "Not a number: %s" (to_pretty_string json)

  let to_string = function
    | String s -> s
    | json -> failwithf "Not a string: %s" (to_pretty_string json)

  let to_list = function
    | Array l -> l
    | json -> failwithf "Not an array: %s" (to_pretty_string json)

  let mk_get_field of_value field obj =
    let v = get_field field obj in
    try of_value v with exn -> failwithf "%s: %s" field (Printexc.to_string exn)

  let int_field = mk_get_field to_int

  let float_field = mk_get_field to_float

  let string_field = mk_get_field to_string

  let list_field = mk_get_field to_list

  let object_field =
    mk_get_field
      (function Object _ as x -> x | json -> failwithf "Not an object: %s" (to_pretty_string json))

  let is_null = function
    | Null -> true
    | _ -> false
end
