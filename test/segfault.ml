type 's constant_constructor = string

type 's constructor =
  | Constant: 's constant_constructor -> 's constructor

module Build : sig
  val constant_constructor : 'a constant_constructor -> 'a
end = struct
  let constant_constructor : type a. a constant_constructor -> a =
    fun x -> failwith x
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
  | Constant c -> Build.constant_constructor c

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
  let open Xtypes in
  match xtype_of_ttype enum_t with
  | Sum { cstrs = [_;_;c] } -> Gen.run (gen_of_constructor c)
  | _ -> assert false

let () =
  print_endline "success"
