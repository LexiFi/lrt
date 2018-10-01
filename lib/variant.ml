open Dynt_core.Ttype
open Xtype

open Dynt_core.Std

exception Variant_parser = Variant_lexer.Error

type t = Variant_lexer.t =
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
  | Lazy of (t Lazy.t [@patch lazy_t])
[@@deriving t]

type 'a to_variant = 'a -> t
type 'a of_variant = t -> 'a
type failwith = {failwith: 'a. string -> 'a} [@@unboxed]

module type VARIANTIZABLE_0 = sig
  include Xtype.TYPE_0
  val to_variant: t to_variant
  val of_variant: failwith -> t of_variant
end

module type VARIANTIZABLE_1 = sig
  include Xtype.TYPE_1
  val to_variant: 'a to_variant -> 'a t to_variant
  val of_variant: failwith -> 'a of_variant -> 'a t of_variant
end

module type VARIANTIZABLE_2 = sig
  include Xtype.TYPE_2
  val to_variant: 'a to_variant -> 'b to_variant -> ('a, 'b) t to_variant
  val of_variant: failwith -> 'a of_variant -> 'b of_variant -> ('a, 'b) t of_variant
end

module type VMATCHER_0 = sig
  include VARIANTIZABLE_0
  include Xtype.MATCHER_0 with type t := t
end

module type VMATCHER_1 = sig
  include VARIANTIZABLE_1
  include Xtype.MATCHER_1 with type 'a t := 'a t
end

module type VMATCHER_2 = sig
  include VARIANTIZABLE_2
  include Xtype.MATCHER_2 with type ('a,'b) t := ('a,'b) t
end

type variantizable =
  | T0 of (module VMATCHER_0)
  | T1 of (module VMATCHER_1)
  | T2 of (module VMATCHER_2)

let abstract_variantizers : (string, variantizable) Hashtbl.t =
  Hashtbl.create 17

let add_abstract_0 (module M : VARIANTIZABLE_0) =
  let module T = struct
    include Xtype.Matcher_0(M)
    let to_variant = M.to_variant
    let of_variant = M.of_variant
  end in
  match T.is_abstract with
  | Some name ->
    Hashtbl.add abstract_variantizers name (T0 (module T))
  | None -> raise (Invalid_argument "add_abstract: received non abstract type")

let add_abstract_1 (module M : VARIANTIZABLE_1) =
  let module T = struct
    include Xtype.Matcher_1(M)
    let to_variant = M.to_variant
    let of_variant = M.of_variant
  end in
  match T.is_abstract with
  | Some name ->
    Hashtbl.add abstract_variantizers name (T1 (module T))
  | _ -> raise (Invalid_argument "add_abstract: received non abstract type")

let add_abstract_2 (module M : VARIANTIZABLE_2) =
  let module T = struct
    include Xtype.Matcher_2(M)
    let to_variant = M.to_variant
    let of_variant = M.of_variant
  end in
  match T.is_abstract with
  | Some name ->
    Hashtbl.add abstract_variantizers name (T2 (module T))
  | _ -> raise (Invalid_argument "add_abstract: received non abstract type")

let rec to_variant: type a. t: a ttype -> a to_variant = fun ~t x ->
  match xtype_of_ttype t with
  | Unit -> Unit
  | Bool -> Bool x
  | Int -> Int x
  | Int32 -> String (Int32.to_string x)
  | Int64 -> String (Int64.to_string x)
  | Nativeint -> String (Nativeint.to_string x)
  | Float -> Float x
  | String -> String x
  (* TODO: old implementation stores char in string *)
  | Char -> Int (int_of_char x)
  | List {t;_} -> List (List.map (to_variant ~t) x)
  | Array {t;_} -> Array (Array.map (to_variant ~t) x)
  | Option {t;_} -> Option (Ext.Option.map (to_variant ~t) x)
  | Lazy {t;_} -> Lazy (lazy (to_variant ~t (Lazy.force x)))
  | Tuple tup ->
    Tuple (Fields.map_tuple tup dyn x)
  | Record r ->
    Record (Fields.map_record r dyn_named x)
  | Sum s -> begin match constructor_by_value s x with
      | Constant c -> Constructor (fst c.cc_label, None)
      | Regular ({rc_label; rc_flds = [Field el]; _} as c) ->
        let arg = to_variant ~t:el.typ.t
            (Fields.regular_constructor c el x |> Ext.Option.value_exn)
        in Constructor (fst rc_label, Some arg)
      | Regular c ->
        let l = Fields.map_regular c dyn x in
        Constructor (fst c.rc_label, Some (Tuple l))
      | Inlined c ->
        let l = Fields.map_inlined c dyn_named x in
        Constructor (fst c.ic_label, Some (Record l))
    end
  | Prop (_,{t;_}) -> to_variant ~t x
  | Object _ -> failwith "Objects cannot be variantized"
  | Function _ -> failwith "Functions cannot be variantized"
  | Abstract (name, _) ->
    let rec use_first = function
      | [] ->
        failwith ("no suitable variantizer registered for abstract type "
                  ^ name)
      | hd :: tl -> begin
          match hd with
          | T0 (module T) -> begin
              match T.is_t t with
              | None -> use_first tl
              | Some (T.Is TypEq.Eq) ->
                T.to_variant x
            end
          | T1 (module T) -> begin
              match T.is_t t with
              | None -> use_first tl
              | Some (T.Is (t1, TypEq.Eq)) ->
                let t1 = to_variant ~t:t1 in
                T.to_variant t1 x
            end
          | T2 (module T) -> begin
              match T.is_t t with
              | None -> use_first tl
              | Some (T.Is (t1, t2, TypEq.Eq)) ->
                let t1 = to_variant ~t:t1
                and t2 = to_variant ~t:t2 in
                T.to_variant t1 t2 x
            end
        end
    in
    use_first (Hashtbl.find_all abstract_variantizers name) ;

and dyn = fun (Dyn (t,x)) -> to_variant ~t x
and dyn_named = fun ~name (Dyn (t,x)) -> name, to_variant ~t x

exception Bad_type_for_variant of Dynt_core.Stype.stype * t * string

type mapper = t -> t option

module Mappers : sig
  type uid = int
  val register: ?name:string -> mapper -> uid
  val get: uid -> string option * mapper
end = struct
  module IntMap = Map.Make(struct type t = int let compare = compare end)

  type uid = int

  let last_uid = ref (-1)
  let t = ref IntMap.empty

  let register ?name mapper =
    let uid = succ !last_uid in
    last_uid := uid;
    t := IntMap.add uid (name, mapper) !t;
    uid

  let get uid =
    IntMap.find uid !t
end

let of_variant_mapper ?name ~t mapper =
  let uid = Mappers.register ?name mapper in
  let props = ["of_variant_mapper", string_of_int uid] in
  Dynt_core.Ttype.add_props props t

let of_variant_default ?name ~t init =
  let mapper = fun _ -> Some (to_variant ~t (init ())) in
  of_variant_mapper ?name ~t mapper

let conv: type a. (string -> a) -> (string -> a) -> string -> a =
  fun failwith conv s -> match conv s with
    | v -> v
    | exception (Failure _) -> failwith "conversion error"

let assoc_consume key lst =
  let rec f acc = function
    | [] -> None
    | (k,v) :: tl when k = key -> Some (v, List.rev_append acc tl)
    | hd :: tl -> f (hd :: acc) tl
  in f [] lst

let variant_of_string = Variant_lexer.variant_of_string
let variant_of_file = Variant_lexer.variant_of_file

type properties = Dynt_core.Stype.stype_properties

let rec of_variant: type a. t: a ttype -> properties -> a of_variant =
  fun ~t properties v ->
    let bad_variant s =
      raise (Bad_type_for_variant(stype_of_ttype t, v, s)) in
    let field_builder: t list -> _ Builder.t = fun lst ->
      let arr = Array.of_list lst in
      let len = Array.length arr in
      let mk {nth; typ} =
        if nth < len then of_variant ~t:typ.t [] arr.(nth)
        else bad_variant ("tuple length mismatch")
      in {mk}
    and record_field_builder: (string * t) list -> _ Builder.t' = fun lst ->
      let lref = ref lst in
      let rec mk (name, props) el =
        (* When lst is in the correct order, it is traversed only once. *)
        match assoc_consume name !lref with
        | Some (v, newlst) -> lref := newlst; of_variant [] ~t:el.typ.t v
        | None ->
          (* Record field <name> not found in variant. Check old_name. *)
          match assoc_consume "of_variant_old_name" props with
          | Some (old_name, reduced_props) ->
            mk (old_name, reduced_props) el
          | None ->
            (* old_name not specified or not part of variant. Check default. *)
            match assoc_consume "of_variant_default" props with
            | Some (default, _reduced_props) -> begin
                try
                  of_variant [] ~t:el.typ.t (variant_of_string default)
                with
                | Variant_parser {msg; text; loc} ->
                  raise (Variant_parser
                           {msg = "of_variant_default: " ^ msg; text; loc})
                | Bad_type_for_variant (typ, v, msg) ->
                  raise (Bad_type_for_variant
                           (typ, v, "of_variant_default: " ^ msg))
              end
            (* Not able to recover *)
            | None -> bad_variant ("missing field in variant: " ^ name)
      in {mk}
    in try match xtype_of_ttype t, v with
      | Unit, Unit -> ()
      | Bool, Bool x -> x
      | Float, Float x -> x
      | Int, Int x -> x
      | Int32, String x -> conv bad_variant Int32.of_string x
      | Int64, String x -> conv bad_variant Int64.of_string x
      | Nativeint, String x -> conv bad_variant Nativeint.of_string x
      | String, String x -> x
      | Char, Int x -> char_of_int x
      | List {t;_}, List x -> List.map (of_variant [] ~t) x
      | Array {t;_}, Array x -> Array.map (of_variant [] ~t) x
      | Option {t;_}, Option x -> Ext.Option.map (of_variant [] ~t) x
      | Lazy {t;_}, Lazy l -> lazy (of_variant [] ~t (Lazy.force l))
      | Tuple tup, Tuple l ->
        Builder.tuple tup (field_builder l)
      | Record r, Record l ->
        Builder.record r (record_field_builder l)
      | Sum s, Constructor (name, args) ->
        let handle_constr c = match c, args with
          | Constant c, None -> Builder.constant_constructor c
          | Regular c, Some (Tuple l) ->
            Builder.regular_constructor c (field_builder l)
          | Regular c, Some singleton ->
            Builder.regular_constructor c (field_builder [singleton])
          | Inlined c, Some (Record l) ->
            Builder.inlined_constructor c (record_field_builder l)
          | _ -> bad_variant "constructor argument mismatch"
        in let next_old old (_,props) constr =
             match old, List.assoc_opt "of_variant_old_name" props with
             | None, Some old_name when old_name = name -> Some constr
             | Some _, Some old_name when old_name = name ->
               assert false (* TODO: old_name given twice. Recover or fail? *)
             | x, _ -> x
        in let rec use_first old = function
            (* Find constructor, remember old_name and use as fallback. *)
            | Constant c as hd :: _ when fst c.cc_label = name ->
              handle_constr hd
            | Regular c as hd :: _ when fst c.rc_label = name ->
              handle_constr hd
            | Inlined c as hd :: _ when fst c.ic_label = name ->
              handle_constr hd
            | Constant c as hd :: tl ->
              use_first (next_old old c.cc_label hd) tl
            | Regular c as hd :: tl ->
              use_first (next_old old c.rc_label hd) tl
            | Inlined c as hd :: tl ->
              use_first (next_old old c.ic_label hd) tl
            | [] -> match old with
              | Some c -> handle_constr c
              | None -> bad_variant "constructor does not exist"
        in use_first None s.cstrs
      (* accumulate properties TODO: which order is correct?*)
      | Prop (lst, {t; _}), v -> of_variant (lst @ properties) ~t v
      | Abstract (name, _), v ->
        let rec use_first : variantizable list -> a = function
          | [] -> failwith (
              "no suitable variantizer registered for abstract type " ^ name)
          | hd :: tl -> begin
              let bad_variant = { failwith = bad_variant } in
              match hd with
              | T0 (module T) -> begin
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is TypEq.Eq) ->
                    T.of_variant bad_variant v
                end
              | T1 (module T) -> begin
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t1, TypEq.Eq)) ->
                    let t1 = of_variant [] ~t:t1 in
                    T.of_variant bad_variant t1 v
                end
              | T2 (module T) -> begin
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t1, t2, TypEq.Eq)) ->
                    let t1 = of_variant [] ~t:t1
                    and t2 = of_variant [] ~t:t2 in
                    T.of_variant bad_variant t1 t2 v
                end
            end
        in
        use_first (Hashtbl.find_all abstract_variantizers name) ;
      | Function _, _ -> bad_variant "function"
      | Object _, _ -> bad_variant "object"
      | Unit, _ -> bad_variant "type mismatch"
      | Bool, _ -> bad_variant "type mismatch"
      | Float, _ -> bad_variant "type mismatch"
      | Int, _ -> bad_variant "type mismatch"
      | Int32, _ -> bad_variant "type mismatch"
      | Int64, _ -> bad_variant "type mismatch"
      | Nativeint, _ -> bad_variant "type mismatch"
      | String, _ -> bad_variant "type mismatch"
      | Char, _ -> bad_variant "type mismatch"
      | List _, _ -> bad_variant "type mismatch"
      | Array _, _ -> bad_variant "type mismatch"
      | Option _, _ -> bad_variant "type mismatch"
      | Lazy _, _ -> bad_variant "type mismatch"
      | Tuple _, _
      | Record _, _
      | Sum _, _ -> bad_variant "type mismatch"
    with Bad_type_for_variant _ as e ->
      (* check accumulated properties for recovery mechanism *)
      let rec use_first props =
        match assoc_consume "of_variant_mapper" props with
        | None -> raise e
        | Some (uid_s, reduced_props) ->
          let uid = match int_of_string_opt uid_s with
            | Some uid -> uid
            | None -> failwith "inconsistent property of_variant_mapper"
          in let (_name_opt, mapper) = Mappers.get uid in
          (* TODO: use name_opt for error messages *)
          match mapper v with
          | Some mapped -> of_variant ~t reduced_props mapped
          | None -> use_first reduced_props
      in use_first properties

let of_variant = of_variant []

let pp_may_left_paren ppf parens = if parens then begin Format.pp_open_box ppf 1; Format.pp_print_char ppf '(' end else Format.pp_open_box ppf 1
let pp_may_right_paren ppf parens = if parens then Format.pp_print_char ppf ')'; Format.pp_close_box ppf ()

let pp_may_paren ppf parens abs pp x =
  if abs x <> x then begin
    pp_may_left_paren ppf parens; pp ppf x; pp_may_right_paren ppf parens
  end else
    pp ppf x

let rec print_variant parens ppf =
  let open Format in
  function
  | Unit -> pp_print_string ppf "()"
  | Bool false -> pp_print_string ppf "false"
  | Bool true -> pp_print_string ppf "true"
  | Int x -> pp_may_paren ppf parens abs pp_print_int x
  | Float x -> pp_may_paren ppf parens abs_float Ext.Float.pp_repres x
  | String s ->  pp_print_char ppf '\"'; pp_print_string ppf (String.escaped s); pp_print_char ppf '\"'
  | Option None -> pp_print_string ppf "None"
  | Option(Some v) -> print_application_like parens ppf "Some" v
  | Tuple l -> pp_open_box ppf 1; pp_print_char ppf '('; Ext.List.pp_list "," (print_variant false) ppf l; pp_print_char ppf ')'; pp_close_box ppf ()
  | List l -> pp_open_box ppf 1; pp_print_char ppf '['; Ext.List.pp_list ";" (print_variant false) ppf l; pp_print_char ppf ']'; pp_close_box ppf ()
  | Array l -> pp_open_box ppf 2; pp_print_string ppf "[|"; Ext.Array.pp_array ';' (print_variant false) ppf l; pp_print_string ppf "|]"; pp_close_box ppf ()
  | Record n_v ->
    let print_field ppf (name, v) = pp_open_box ppf 1; pp_print_string ppf name; pp_print_string ppf " ="; pp_print_space ppf (); print_variant false ppf v; pp_close_box ppf () in
    pp_open_hvbox ppf 1; pp_print_char ppf '{'; Ext.List.pp_list ";" print_field ppf n_v; pp_print_char ppf '}'; pp_close_box ppf ()
  | Constructor (s, None) -> pp_print_string ppf s
  | Constructor (s, Some v) -> print_application_like parens ppf s v
  | Variant v -> print_application_like parens ppf "variant" v
  | Lazy v ->
    try
      let v = Lazy.force v in
      print_application_like parens ppf "lazy" v
    with exn ->
      pp_may_left_paren ppf parens;
      pp_print_string ppf "lazy(raise \""; pp_print_string ppf (Printexc.to_string exn); pp_print_string ppf "\")";
      pp_may_right_paren ppf parens
and print_application_like parens ppf name v =
  let open Format in
  pp_may_left_paren ppf parens;
  pp_print_string ppf name;
  begin match v with
    | Tuple _ | List _ | Array _ | Record _ ->
      pp_print_cut ppf (); print_variant false ppf v;
    | Unit | Bool _ | Int _ | Float _ | String _
    | Option None | Constructor (_, None) ->
      pp_print_space ppf (); print_variant true ppf v
    | _ -> pp_print_cut ppf (); print_variant true ppf v;
  end;
  pp_may_right_paren ppf parens

let print_variant = print_variant false

let () = Printexc.register_printer
    (function
      | Bad_type_for_variant (t, v, s) ->
        Some(
          Format.asprintf
            "Bad type for value: %s@.@[** Variant: %a@.@[** Type: %a@]"
            s print_variant v Dynt_core.Stype.print_stype t)
      | _ -> None
    )

(* register hashtable variantizer *)

let () = add_abstract_2 (module struct
    type ('a,'b) t = ('a,'b) Hashtbl.t
    let t = hashtbl_t

    let to_variant t1 t2 ht =
      Hashtbl.fold (fun key value acc ->
          Tuple [t1 key; t2 value] :: acc) ht []
      |> fun x -> List x

    let of_variant {failwith} o1 o2 v =
      let ht = Hashtbl.create 5 in
      let f = function
        | Tuple [key; value] -> Hashtbl.add ht (o1 key) (o2 value)
        | _ -> failwith "expected tuple (key,value)"
      in match v with
      | List l -> (List.iter f l); ht
      | _ -> failwith "expected list of (key,value)"
  end)

(* serializing / deserializing convenience *)

let strings_of_variant v =
  let rec loop acc = function
    | Unit -> acc
    | Bool b -> string_of_bool b :: acc
    | Int n -> string_of_int n :: acc
    | Float f -> string_of_float f :: acc
    | String s -> s :: acc
    | Tuple l
    | List l -> List.fold_left loop acc l
    | Array a -> Array.fold_left loop acc a
    | Option None -> acc
    | Option (Some v) -> loop acc v
    | Record l -> List.fold_left loop acc (List.map snd l)
    | Constructor (s, None) -> s :: acc
    | Constructor (s, Some v) -> loop (s :: acc) v
    | Variant v
    | Lazy (lazy v) -> loop acc v
  in
  loop [] v

let value_of_variant_in_file ~t file_name =
  of_variant ~t (variant_of_file file_name)

let format_to_out_channel ?margin oc printer arg =
  let open Format in
  let ppf = formatter_of_out_channel oc in
  (match margin with
   | None -> ()
   | Some m -> pp_set_margin ppf m);
  printer ppf arg;
  pp_print_newline ppf ()

let format_to_file ?eol_lf ?margin outfile printer arg =
  let oc = match eol_lf with
    | Some () -> open_out_bin outfile
    | None -> open_out outfile in
  try
    format_to_out_channel ?margin oc printer arg;
    close_out oc
  with
  | e -> close_out oc; raise e

let variant_to_file ?eol_lf fn v = format_to_file ?eol_lf fn print_variant v

let value_to_variant_in_file ~t ?eol_lf file_name v =
  variant_to_file ?eol_lf file_name (to_variant ~t v)

(* compact representation *)

let rec same_fields fields fields' =
  match fields, fields' with
  | [], [] -> true
  | [], _ | _, [] -> false
  | (f, _) :: fields, (f', _) :: fields' -> f = f' && same_fields fields fields'

let buf = Buffer.create 512

let rec iter_sep f sep = function
  | [] -> ()
  | [x] -> f x
  | x :: rest -> f x; Buffer.add_char buf sep; iter_sep f sep rest

let print_compact_header fields =
  Buffer.add_string buf "LEXIFICOMPACTRECORDS1(";
  iter_sep (fun (field, _value) -> Buffer.add_char buf '\"'; Buffer.add_string buf field; Buffer.add_char buf '\"') ',' fields;
  Buffer.add_string buf ");"

let compact_buffer_of_variant ?dont_compress_records ?with_more_spaces v =
  let compress_records = (dont_compress_records : unit option) == None in
  let with_more_spaces = (with_more_spaces : unit option) != None in

  let rec compact_print_variant parens = function
    | Unit -> Buffer.add_string buf "()"
    | Bool true -> Buffer.add_string buf "true"
    | Bool false -> Buffer.add_string buf "false"
    | Int i when i < 0 && parens ->
      Buffer.add_char buf '(';
      Buffer.add_string buf (string_of_int i);
      Buffer.add_char buf ')'
    | Int i -> Buffer.add_string buf (string_of_int i)
    | Float f when f < 0. && parens ->
      Buffer.add_char buf '(';
      Buffer.add_string buf (Ext.Float.repres f);
      Buffer.add_char buf ')'
    | Float f -> Buffer.add_string buf (Ext.Float.repres f)
    | String s ->
      Buffer.add_char buf '\"';
      Buffer.add_string buf (String.escaped s);
      Buffer.add_char buf '\"'
    | Option None -> Buffer.add_string buf "None"
    | Option (Some v) -> compact_print_application_like parens "Some" v
    | Tuple l -> Buffer.add_char buf '('; iter_sep_compact ',' l; Buffer.add_char buf ')'
    | List [] -> Buffer.add_string buf "[]"
    | List l ->
      Buffer.add_char buf '[';
      if compress_records then
        match l with
        | Record fields :: (_ :: _ :: _ as other_records) when List.for_all (function | Record fields' -> same_fields fields fields' | _ -> false) other_records ->
          print_compact_header fields;
          iter_sep print_compact_line ';' l
        | _ -> iter_sep_compact ';' l
      else
        iter_sep_compact ';' l;
      Buffer.add_char buf ']'
    | Array [||] -> Buffer.add_string buf "[||]"
    | Array l ->
      Buffer.add_string buf "[|";
      let length = Array.length l in
      let iter f = Array.iteri (fun i v -> f v; if i < length - 1 then Buffer.add_char buf ';') l in
      if compress_records && 3 <= length then
        match l.(0) with
        | Record fields when Array.for_all (function | Record fields' -> same_fields fields fields' | _ -> false) l ->
          print_compact_header fields;
          iter print_compact_line
        | _ -> iter (compact_print_variant false)
      else
        iter (compact_print_variant false);
      Buffer.add_string buf "|]"
    | Record l when with_more_spaces ->
      let rec loop = function
        | [] -> ()
        | (k, v) :: rest ->
          Buffer.add_string buf k; Buffer.add_string buf " = "; compact_print_variant false v;
          if rest != [] then (Buffer.add_string buf "; "; loop rest)
      in
      Buffer.add_char buf '{'; loop l; Buffer.add_char buf '}'
    | Record l ->
      let rec loop = function
        | [] -> ()
        | (k, v) :: rest ->
          Buffer.add_string buf k; Buffer.add_char buf '='; compact_print_variant false v;
          if rest != [] then (Buffer.add_char buf ';'; loop rest)
      in
      Buffer.add_char buf '{'; loop l; Buffer.add_char buf '}'
    | Constructor (s, None) -> Buffer.add_string buf s
    | Constructor ("RAWVARIANT", Some (String s)) -> Buffer.add_string buf s
    | Constructor (s, Some t) -> compact_print_application_like parens s t
    | Variant v -> compact_print_application_like parens "variant" v
    | Lazy v ->
      let v = Lazy.force v in  (* the exception escape... *)
      compact_print_application_like parens "lazy" v
  and compact_print_application_like parens name v =
    if parens then Buffer.add_char buf '(';
    Buffer.add_string buf name;
    begin
      match v with
      | Tuple _ | Record _ | List _ | Array _ -> compact_print_variant false v
      | Unit | Bool _ | Int _ | Float _ | String _ | Option None |  Constructor (_, None) -> Buffer.add_char buf ' '; compact_print_variant true v
      | _ -> Buffer.add_char buf '('; compact_print_variant false v; Buffer.add_char buf ')'
    end;
    if parens then Buffer.add_char buf ')'
  and print_compact_line = function
    | Record l -> iter_sep (fun (_field, value) -> compact_print_variant false value) ',' l;
    | _ -> assert false
  and iter_sep_compact sep = function
    | [] -> ()
    | [x] -> compact_print_variant false x
    | x :: rest -> compact_print_variant false x; Buffer.add_char buf sep; if with_more_spaces then Buffer.add_char buf ' '; iter_sep_compact sep rest
  in
  compact_print_variant false v

let compact_string_of_variant ?dont_compress_records ?with_more_spaces v =
  Buffer.clear buf;
  compact_buffer_of_variant ?dont_compress_records ?with_more_spaces v;
  Buffer.contents buf

let string_one_line_of_variant v =
  compact_string_of_variant ~dont_compress_records:() ~with_more_spaces:() v

let output_compact_string_of_variant ?dont_compress_records ?with_more_spaces oc v =
  Buffer.clear buf;
  compact_buffer_of_variant ?dont_compress_records ?with_more_spaces v;
  Buffer.output_buffer oc buf
