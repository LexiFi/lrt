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
