module TypEq : sig

  type (_, _) t = Eq: ('a, 'a) t

  val refl: ('a, 'a) t
  val trans: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val sym: ('a, 'b) t -> ('b, 'a) t

  val app: ('a, 'b) t -> 'a -> 'b

  module Lift(T : sig type 'a c end) : sig
    val eq: ('a, 'b) t -> ('a T.c, 'b T.c) t
  end

  val unsafe: ('a, 'b) t

end = struct

  type (_, _) t = Eq: ('a, 'a) t

  let refl = Eq

  let trans (type a) (type b) (type c) (Eq : (a, b) t) (Eq : (b, c) t) =
    (Eq : (a, c) t)

  let sym (type a) (type b) (Eq : (a, b) t) = (Eq : (b, a) t)

  let app (type a) (type b) (Eq : (a, b) t) (x : a) = (x : b)

  let unsafe = Obj.magic Eq

  module Lift(T : sig type 'a c end) = struct
    let eq (type a) (type b) (Eq : (a, b) t) = (Eq : (a T.c, b T.c) t)
  end

end

open Stype

type 'a ttype = stype

type dynamic = Dyn: 'a ttype * 'a -> dynamic

external stype_of_ttype: _ ttype -> stype = "%identity"

external __use_ttype: 'a ttype -> 'b -> 'b = "%use_ttype"

let remove_first_props_ttype = remove_first_props
let add_props props t = DT_prop(props, t)
let abstract_ttype = abstract_stype

let split_arrow_ttype t =
  match remove_first_props t with
  | DT_arrow(_, t1, t2) -> t1, t2
  | _ -> assert false

let build_arrow_ttype t1 t2 =
  DT_arrow("", t1, t2)

let ttype_fst = function Stype.DT_tuple [t; _] -> t | _ -> assert false
let ttype_snd = function Stype.DT_tuple [_; t] -> t | _ -> assert false

let ttypes_equality t1 t2 =
  if Stype.types_equality t1 t2 then Some TypEq.unsafe
  else None

let ttypes_equality_modulo_props t1 t2 =
  if Stype.types_equality_modulo_props t1 t2 then Some TypEq.unsafe
  else None
