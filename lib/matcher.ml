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

type 'a candidate =
  | T0 of (module C0 with type res = 'a)
  | T1 of (module C1 with type res = 'a)
  | T2 of (module C2 with type res = 'a)

type 'a compiled = 'a candidate list

type 'a t = 'a candidate list * ('a compiled Lazy.t)

let compile : type res. res candidate list -> res t =
  fun candidates -> (candidates, lazy (List.rev candidates))
(* This implies oldest added is tried first. What do we want? *)
(* TODO: Build some efficient data structure. *)

let empty : 'a t = [], lazy []

let add (type t res) ~(t: t Ttype.t) ~(f: t -> res) (lst, _) =
  T0 (module struct
    type nonrec t = t [@@deriving t]
    type nonrec res = res
    let f = f end) :: lst
  |> compile

let add0 (type a) (module C : C0 with type res = a) (lst, _) =
  T0 (module C : C0 with type res = a) :: lst
  |> compile

let add1 (type a) (module C : C1 with type res = a) (lst, _) =
  T1 (module C : C1 with type res = a) :: lst
  |> compile

let add2 (type a) (module C : C2 with type res = a) (lst, _) =
  T2 (module C : C2 with type res = a) :: lst
  |> compile

let apply' : type res. res t -> Ttype.dynamic -> res =
  fun (_, lazy matcher) (Ttype.Dyn (t,x)) ->
    let (module B) = Unify.t0 t
    and (module P) = Unify.init ~modulo_props:false in
    let rec loop = function
      | [] -> raise Not_found
      | T0 (module A : C0 with type res = res) :: tl ->
        begin try
            let module U = Unify.U0 (P) (A) (B) in
            let TypEq.Eq = U.eq in A.f x
          with Unify.Not_unifiable -> loop tl end
      | T1 (module A : C1 with type res = res) :: tl ->
        begin try
            let module U = Unify.U1 (P) (A) (B) in
            let TypEq.Eq = U.eq in A.f U.a_t x
          with Unify.Not_unifiable -> loop tl end
      | T2 (module A : C2 with type res = res) :: tl ->
        begin try
            let module U = Unify.U2 (P) (A) (B) in
            let TypEq.Eq = U.eq in A.f U.a_t U.b_t x
          with Unify.Not_unifiable -> loop tl end
    in loop matcher

let apply matcher ~t x = apply' matcher (Ttype.Dyn (t, x))
