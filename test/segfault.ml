open Dynt
open Xtypes

module Gen : sig
  type 'a t
  val run : 'a t -> 'a
end = struct
  type 'a t = unit -> 'a
  let run g = g ()
end

let f (type a) (c : a constructor) : a Gen.t =
  ((match c with
      (* TODO: SEGV by removing return *)
      | Constant c -> (Builder.constant_constructor c)
      | _ -> assert false))

type enum = A | B | C [@@deriving t]

let _value = A, B, C

let _ =
  match xtype_of_ttype enum_t with
  | Sum [| a ; _b; _c |] -> Gen.run (f a)
  | _ -> assert false
