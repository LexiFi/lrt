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
  include Xtype.T0
  type res
  val f: t -> res
end

module type C1 = sig
  include Xtype.T1
  type res
  val f: 'a Ttype.t -> 'a t -> res
end

module type C2 = sig
  include Xtype.T2
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
    | T : { t: 'b Ttype.t; f:('b -> 'a) } -> 'a candidate
    | T0 : (module C0 with type res = 'a) -> 'a candidate
    | T1 : (module C1 with type res = 'a) -> 'a candidate
    | T2 : (module C2 with type res = 'a) -> 'a candidate

  type 'a candidates = (Stype.t * 'a candidate) list

  type 'a t = 'a candidates

  type var
  let var i : var Ttype.t = Obj.magic (Stype.DT_var i)

  let empty = []

  let add ~t ~f lst =
    let s = Ttype.to_stype t in
    (s, T {t;f}) :: lst

  let add0 (type a) (module C : C0 with type res = a) lst =
    let s = Ttype.to_stype C.t in
    (s, T0 (module C : C0 with type res = a)) :: lst

  let add1 (type a) (module C : C1 with type res = a) lst =
    let s = Ttype.to_stype (C.t (var 0)) in
    (s, T1 (module C : C1 with type res = a)) :: lst

  let add2 (type a) (module C : C2 with type res = a) lst =
    let s = Ttype.to_stype (C.t (var 0) (var 1)) in
    (s, T2 (module C : C2 with type res = a)) :: lst

  let create x = List.rev x

  module IntMap = Map.Make(struct type t = int let compare = compare end)

  module U = struct
    exception Not_unifiable

    let rec unify_list_iter2 f l1 l2 =
      match l1, l2 with
      | [], [] -> ()
      | [], _
      | _, [] -> raise Not_unifiable
      | h1 :: t1 , h2 :: t2 -> f h1 h2 ; unify_list_iter2 f t1 t2

    let variant_constrs_iter2 (f: Stype.t -> Stype.t -> unit)
        (name1, props1, vargs1)
        (name2, props2, vargs2) =
      if name1 <> name2 then raise Not_unifiable;
      if props1 <> props2 then raise Not_unifiable;
      match vargs1, vargs2 with
      | Stype.C_inline s1, Stype.C_inline s2 -> f s1 s2
      | C_tuple l1, C_tuple l2 -> unify_list_iter2 f l1 l2
      | C_inline _, _
      | C_tuple _, _ -> raise Not_unifiable

    (* iterate over all stypes in a node *)
    let node_iter2 (f: Stype.t -> Stype.t -> unit)
        ({rec_descr = descr1; rec_name = name1; rec_args = args1; _}: Stype.node)
        ({rec_descr = descr2; rec_name = name2; rec_args = args2; _}: Stype.node) =
      if name1 <> name2 then raise Not_unifiable;
      unify_list_iter2 f args1 args2;
      match descr1, descr2 with
      | DT_variant {variant_constrs = c1; variant_repr = r1},
        DT_variant {variant_constrs = c2; variant_repr = r2} when r1 = r2 ->
        unify_list_iter2 (variant_constrs_iter2 f) c1 c2
      | DT_record {record_fields = l1; record_repr = r1},
        DT_record {record_fields = l2; record_repr = r2} when r1 = r2 ->
        unify_list_iter2 (
          fun (name1, props1, s1) (name2, props2, s2) ->
            if name1 <> name2 then raise Not_unifiable;
            if props1 <> props2 then raise Not_unifiable;
            f s1 s2
        ) l1 l2
      | DT_record _, _
      | DT_variant _, _ -> raise Not_unifiable

    let unify ~modulo_props ~free:s1 ~closed:s2 =
      let subs = ref IntMap.empty in
      let rec unify s1 s2 =
        let set k s =
          match IntMap.find_opt k !subs with
          | None -> subs := IntMap.add k s !subs
          | Some s' -> ignore (unify s s')
          (* Both s and s' come from closed, but we did not check for absence
             of free variables. Perhaps, we should do it right and apply the
             substitutions here. TODO *)
        in
        match (s1, s2: Stype.t * Stype.t) with
        | _, DT_var _ ->
          raise (Invalid_argument "unify: free variable in closed")
        | DT_var k, s2 -> set k s2
        | DT_int, DT_int
        | DT_float, DT_float
        | DT_string, DT_string -> ()
        | DT_option s1, DT_option s2
        | DT_list s1, DT_list s2
        | DT_array s1, DT_array s2 -> unify s1 s2
        | DT_tuple l1, DT_tuple l2 ->
          unify_list_iter2 unify l1 l2
        | DT_node n1, DT_node n2 -> node_iter2 unify n1 n2
        | DT_arrow (n1, s1, s1'), DT_arrow (n2, s2, s2') ->
          if n1 <> n2 then raise Not_unifiable;
          unify s1  s2 ;
          unify s1' s2'
        | DT_object l1, DT_object l2 ->
          unify_list_iter2 (fun (n1,s1) (n2,s2) ->
              if n1 <> n2 then raise Not_unifiable;
              unify s1 s2
            ) l1 l2
        | DT_abstract (n1, l1), DT_abstract (n2, l2) ->
          if n1 <> n2 then raise Not_unifiable;
          unify_list_iter2 unify l1 l2
        | DT_prop (_, t1), t2 when modulo_props -> unify t1 t2
        | t1, DT_prop (_, t2) when modulo_props -> unify t1 t2
        | DT_prop (p1, t1), DT_prop (p2, t2) when p1 = p2 -> unify t1 t2
        | DT_prop _, _
        | DT_int, _
        | DT_float, _
        | DT_string, _
        | DT_option _, _
        | DT_list _, _
        | DT_array _, _
        | DT_tuple _, _
        | DT_node _, _
        | DT_arrow _, _
        | DT_object _, _
        | DT_abstract _, _ -> raise Not_unifiable
      in
      unify s1 s2; !subs
  end

  let apply : type a b. a t -> t: b Ttype.t -> b -> a =
    fun matcher ~t x ->
      let rec loop = function
        | [] -> raise Not_found
        | (free, c) :: tl ->
          let closed = Ttype.to_stype t in
          match U.unify ~modulo_props:true ~free ~closed with
          | (_subs : Stype.t IntMap.t) -> c
          | exception U.Not_unifiable -> loop tl
      in match loop matcher with
      (* This last step might equally well use magic *)
      | T c -> begin
          match Ttype.equality_modulo_props c.t t with
          | Some (TypEq.Eq) -> c.f x
          | None -> assert false
        end
      | T0 (module C : C0 with type res = a) -> begin
          let module M = Xtype.Match0 (C) in
          match M.is_t t with
          | None -> assert false
          | Some (M.Is (TypEq.Eq)) -> C.f x
        end
      | T1 (module C : C1 with type res = a) -> begin
          let module M = Xtype.Match1 (C) in
          match M.is_t t with
          | None -> assert false
          | Some (M.Is (a, TypEq.Eq)) -> C.f a x
        end
      | T2 (module C : C2 with type res = a) -> begin
          let module M = Xtype.Match2 (C) in
          match M.is_t t with
          | None -> assert false
          | Some (M.Is (a, b, TypEq.Eq)) -> C.f a b x
        end
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


