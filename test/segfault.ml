type 's constant_constructor = 's
type ('s, 't) noncst_constructor = 't -> 's

type 's constructor =
  | Constant of 's constant_constructor
  | NonConstant : ('s, 't) noncst_constructor -> 's constructor

module Build : sig
  type 'a t = { mk : 'b. ('a,'b) noncst_constructor -> 'b }

  val constant_constructor : 'a constant_constructor -> 'a
  val noncst_constructor : ('a,'b) noncst_constructor -> 'a t -> 'a
end = struct
  type 'a t = { mk : 'b. ('a,'b) noncst_constructor -> 'b }

  let constant_constructor : type a. a constant_constructor -> a =
    fun x -> x

  let noncst_constructor : type a b. (a,b) noncst_constructor -> a t -> a =
    fun f t -> f (t.mk f)
end

module Gen : sig
  type 'a t
  val return : 'a -> 'a t
  val run : 'a t -> 'a
end = struct
  type 'a t = unit -> 'a
  let return x = fun () -> x
  (* This application segfaults *)
  let run g = g ()
end

let magic = Obj.magic ()

let _gen_of_constructor (type a) (c : a constructor) : a Gen.t =
  match c with
  | Constant c -> Gen.return (Build.constant_constructor c)
  | NonConstant c -> Gen.return (Build.noncst_constructor c magic)

open Dynt
open Xtypes

let gen_of_constructor (type a) (c : a constructor) : a Gen.t =
  match c with
  (* TODO: Why is this not rejected by the typechecker: *)
  (* | Constant c -> Builder.constant_constructor c *)
  | Constant c -> Builder.constant_constructor c |> Gen.return
  | _ -> assert false

type enum = A | B | C [@@deriving t]

let _ = A, B, C

let _ =
  match xtype_of_ttype enum_t with
  | Sum [| a ; _b; _c |] -> Gen.run (gen_of_constructor a)
  | _ -> assert false

let () =
  print_endline "success"
