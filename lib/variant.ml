open Dynt_core.Ttype
open Xtype

open Dynt_core.Std

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float of float
  | String of string
  | Char of char
  | Tuple of t list
  | List of t list
  | Array of t array
  | Option of t option
  | Record of (string * t) list
  | Constructor of string * t option
  | Variant of t
  | Lazy of (t Lazy.t [@patch lazy_t])
[@@deriving t]

let rec variant: type a. t: a ttype -> a -> t = fun ~t x ->
  match xtype_of_ttype t with
  | Unit -> Unit
  | Bool -> Bool x
  | Int -> Int x
  | Int32 -> Int32 x
  | Int64 -> Int64 x
  | Nativeint -> Nativeint x
  | Float -> Float x
  | String -> String x
  | Char -> Char x
  | List {t;_} -> List (List.map (variant ~t) x)
  | Array {t;_} -> Array (Array.map (variant ~t) x)
  | Option {t;_} -> Option (Ext.Option.map (variant ~t) x)
  | Lazy {t;_} -> Lazy (lazy (variant ~t (Lazy.force x)))
  | Tuple tup ->
    Tuple (Fields.map_tuple tup dyn x)
  | Record r ->
    Record (Fields.map_record r dyn_named x)
  | Sum s -> begin match constructor_by_value s x with
      | Constant c -> Constructor (fst c.cc_label, None)
      | Regular c ->
        let l = Fields.map_regular c dyn x in
        Constructor (fst c.rc_label, Some (Tuple l))
      | Inlined c ->
        let l = Fields.map_inlined c dyn_named x in
        Constructor (fst c.ic_label, Some (Record l))
    end
  | Prop (_, {t;_}) -> variant ~t x
  | Object _ -> failwith "Objects cannot be variantized"
  | Abstract _ -> failwith "Abstract values cannot be variantized"
  | Function _ -> failwith "Functions cannot be variantized"
and dyn = fun (Dyn (t,x)) -> variant ~t x
and dyn_named = fun ~name (Dyn (t,x)) -> name, variant ~t x

exception Bad_type_for_variant of Dynt_core.Stype.stype * t * string

let rec of_variant: type a. t: a ttype -> t -> a = fun ~t v ->
  match xtype_of_ttype t, v with
  | Unit, Unit -> ()
  | Bool, Bool x -> x
  | Float, Float x -> x
  | Int, Int x -> x
  | Int32, Int32 x -> x
  | Int64, Int64 x -> x
  | Nativeint, Nativeint x -> x
  | String, String x -> x
  | Char, Char x -> x
  | List {t;_}, List x -> List.map (of_variant ~t) x
  | Array {t;_}, Array x -> Array.map (of_variant ~t) x
  | Option {t;_}, Option x -> Ext.Option.map (of_variant ~t) x
  | Lazy {t;_}, Lazy l -> lazy (of_variant ~t (Lazy.force l))
  | Tuple tup, Tuple l ->
    let arr = Array.of_list l in
    let mk = fun {nth; typ} -> of_variant ~t:typ.t arr.(nth) in
    Builder.tuple tup {mk}
  | Record r, Record l ->
    let arr = Array.of_list l in
    let mk = fun {nth; typ} -> of_variant ~t:typ.t (snd arr.(nth)) in
    Builder.record r {mk}
  | Sum s, Constructor (name, args) -> begin
      match Lookup.constructor s name with
      | None ->
        raise (Bad_type_for_variant (stype_of_ttype t, v,
                                     "non-existing constructor"))
      | Some c -> begin match c, args with
          | Constant c, None -> Builder.constant_constructor c
          | Regular c, Some (Tuple l) ->
            let arr = Array.of_list l in
            let mk = fun {nth; typ} -> of_variant ~t:typ.t arr.(nth) in
            Builder.regular_constructor c {mk}
          | Inlined c, Some (Record l) ->
            let arr = Array.of_list l in
            let mk = fun {nth; typ} -> of_variant ~t:typ.t (snd arr.(nth)) in
            Builder.inlined_constructor c {mk}
          | _ ->
            raise (Bad_type_for_variant (stype_of_ttype t, v,
                                         "constructor argument mismatch"))
        end
    end
  | Prop (_,{t;_}), v -> of_variant ~t v
  | Function _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "function"))
  | Object _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "object"))
  | Abstract _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "abstract"))
  | Unit, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Bool, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Float, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Int, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Int32, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Int64, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Nativeint, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | String, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Char, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | List _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Array _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Option _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Lazy _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Tuple _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Record _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))
  | Sum _, _ ->
    raise (Bad_type_for_variant (stype_of_ttype t, v, "type value mismatch"))

let pp_may_left_paren ppf parens = if parens then begin Format.pp_open_box ppf 1; Format.pp_print_char ppf '(' end else Format.pp_open_box ppf 1
let pp_may_right_paren ppf parens = if parens then Format.pp_print_char ppf ')'; Format.pp_close_box ppf ()

let pp_may_paren ppf parens abs pp x =
  if abs x <> x then begin
    pp_may_right_paren ppf parens; pp ppf x; pp_may_right_paren ppf parens
  end else
    pp ppf x

let pp_print_int32 ppf x = Format.pp_print_string ppf (Int32.to_string x)
let pp_print_int64 ppf x = Format.pp_print_string ppf (Int64.to_string x)
let pp_print_nativeint ppf x =
  Format.pp_print_string ppf (Nativeint.to_string x)

let rec print_variant parens ppf =
  let open Format in
  function
  | Unit -> pp_print_string ppf "()"
  | Bool false -> pp_print_string ppf "false"
  | Bool true -> pp_print_string ppf "true"
  | Char c -> pp_print_char ppf c
  | Int x -> pp_may_paren ppf parens abs pp_print_int x
  | Float x -> pp_may_paren ppf parens abs_float pp_print_float x
  | Int32 x -> pp_may_paren ppf parens Int32.abs pp_print_int32 x
  | Int64 x -> pp_may_paren ppf parens Int64.abs pp_print_int64 x
  | Nativeint x -> pp_may_paren ppf parens Nativeint.abs pp_print_nativeint x
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
