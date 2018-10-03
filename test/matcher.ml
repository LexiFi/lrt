(* Let's try to unify multiple types at once. *)

open Dynt

type a = (int * int)
and b = int option
and c = string list
and d = (int * (int * string))
and 'a e = 'a list
[@@deriving t]

let e_t = e_t

module type C0 = sig
  include Unify.T0
  type res
  val f: t -> res
end

module type C1 = sig
  include Unify.T1
  type res
  val f: 'a Ttype.t -> 'a t -> res
end

module type C2 = sig
  include Unify.T2
  type res
  val f: 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t -> res
end

module Matcher : sig
  type 'a t
  type 'a candidates

  val empty: 'a candidates
  val add: t: 'b Ttype.t -> f:('b -> 'a) -> 'a candidates -> 'a candidates
  val add0: (module C0 with type res = 'a) -> 'a candidates -> 'a candidates
  val add1: (module C1 with type res = 'a) -> 'a candidates -> 'a candidates
  val add2: (module C2 with type res = 'a) -> 'a candidates -> 'a candidates

  val create: 'a candidates -> 'a t
  (** Prepare the list of candidates for efficient matching. *)

  val apply: 'a t -> t: 'b Ttype.t -> 'b -> 'a
  (** [match c ~default ~t x] checks for type equality of [t] with a type
      registered with [add ~t ~f].

      Returns [f x] if such a [f] is present and otherwise raises [Not_found].
  *)
end = struct
  type 'a candidate =
    | T0 of (module C0 with type res = 'a)
    | T1 of (module C1 with type res = 'a)
    | T2 of (module C2 with type res = 'a)

  type 'a candidates = 'a candidate list

  type 'a t = 'a candidates

  let empty = []

  let add (type t res) ~(t: t Ttype.t) ~(f: t -> res) lst =
    T0 (module struct
      type nonrec t = t [@@deriving t]
      type nonrec res = res
      let f = f end) :: lst

  let add0 (type a) (module C : C0 with type res = a) lst =
    T0 (module C : C0 with type res = a) :: lst

  let add1 (type a) (module C : C1 with type res = a) lst =
    T1 (module C : C1 with type res = a) :: lst

  let add2 (type a) (module C : C2 with type res = a) lst =
    T2 (module C : C2 with type res = a) :: lst

  let create x = List.rev x

  let apply : type res a. res t -> t: a Ttype.t -> a -> res =
    fun matcher ~t x ->
      let (module B) = Unify.t0 t in
      let rec loop = function
        | [] -> raise Not_found
        | T0 (module C : C0 with type res = res) :: tl ->
          begin try
              let module U = Unify.U0 (C) (B) in
              let TypEq.Eq = U.eq in C.f x
            with Unify.Not_unifiable -> loop tl end
        | T1 (module C : C1 with type res = res) :: tl ->
          begin try
              let module U = Unify.U1 (C) (B) in
              let TypEq.Eq = U.eq in C.f U.a_t x
            with Unify.Not_unifiable -> loop tl end
        | T2 (module C : C2 with type res = res) :: tl ->
          begin try
              let module U = Unify.U2 (C) (B) in
              let TypEq.Eq = U.eq in C.f U.a_t U.b_t x
            with Unify.Not_unifiable -> loop tl end
      in loop matcher
end

let matcher =
  let open Matcher in
  empty
  |> add ~t:a_t ~f:(fun _ -> print_endline "a")
  |> add ~t:b_t ~f:(fun _ -> print_endline "b")
  |> add ~t:c_t ~f:(fun _ -> print_endline "c")
  |> add0 (module struct
    type res = unit
    type t = d [@@deriving t]
    let f _ = print_endline "d" end)
  |> add1 (module struct
    type res = unit
    type 'a t = 'a e [@@deriving t]
    let f _ _ = print_endline "e" end)
  |> add2 (module struct
    type res = unit
    type ('a,'b) t = ('a,'b) Hashtbl.t [@patch hashtbl_t] [@@deriving t]
    let f _ _ _ = print_endline "f" end)
  |> create

let%expect_test _ =
  Matcher.apply matcher ~t:a_t (1,1) ;
  Matcher.apply matcher ~t:b_t None ;
  Matcher.apply matcher ~t:c_t ["string";"list"] ;
  Matcher.apply matcher ~t:d_t (1,(1,"")) ;
  Matcher.apply matcher ~t:(e_t float_t) [1.] ;
  Matcher.apply matcher ~t:(hashtbl_t int_t float_t) (Hashtbl.create 1);
  [%expect {|
    a
    b
    c
    d
    e
    f |}]


