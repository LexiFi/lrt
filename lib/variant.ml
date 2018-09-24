open Dynt_core.Ttype
open Xtype

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
  | Lazy of t Lazy.t

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
and dyn = fun (Dyn (t,x)) -> variant ~t x
and dyn_named = fun ~name (Dyn (t,x)) -> name, variant ~t x
