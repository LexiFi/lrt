module type T0 = sig
  type t

  val t : t Ttype.t
end

let t0 (type t) (t : t Ttype.t) =
  (module struct type nonrec t = t [@@deriving t] end : T0 with type t = t)

module type T1 = sig
  type 'a t

  val t : 'a Ttype.t -> 'a t Ttype.t
end

module type T2 = sig
  type ('a, 'b) t

  val t : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t Ttype.t
end

module type PARAM = sig
  val modulo_props : bool
end

let init ~modulo_props =
  (module struct let modulo_props = modulo_props end : PARAM)

(* unification of stypes *)

exception Not_unifiable

let rec unify_list_iter2 f l1 l2 =
  match (l1, l2) with
  | [], [] -> ()
  | [], _ | _, [] -> raise Not_unifiable
  | h1 :: t1, h2 :: t2 -> f h1 h2 ; unify_list_iter2 f t1 t2

let variant_constrs_iter2 (f : Stype.t -> Stype.t -> unit)
    (name1, props1, vargs1) (name2, props2, vargs2) =
  if name1 <> name2 then raise Not_unifiable ;
  if props1 <> props2 then raise Not_unifiable ;
  match (vargs1, vargs2) with
  | Stype.C_inline s1, Stype.C_inline s2 -> f s1 s2
  | C_tuple l1, C_tuple l2 -> unify_list_iter2 f l1 l2
  | C_inline _, _ | C_tuple _, _ -> raise Not_unifiable

(* iterate over all stypes in a node *)
(* TODO: This is a much stricter compare than the one in Matcher.
   Perhaps, one might relax this a bit and document the assumptions. *)
let node_iter2 (f : Stype.t -> Stype.t -> unit)
    ({rec_descr= descr1; rec_name= name1; rec_args= args1; _} : Stype.node)
    ({rec_descr= descr2; rec_name= name2; rec_args= args2; _} : Stype.node) =
  if name1 <> name2 then raise Not_unifiable ;
  unify_list_iter2 f args1 args2 ;
  match (descr1, descr2) with
  | ( DT_variant {variant_constrs= c1; variant_repr= r1}
    , DT_variant {variant_constrs= c2; variant_repr= r2} )
    when r1 = r2 ->
      unify_list_iter2 (variant_constrs_iter2 f) c1 c2
  | ( DT_record {record_fields= l1; record_repr= r1}
    , DT_record {record_fields= l2; record_repr= r2} )
    when r1 = r2 ->
      unify_list_iter2
        (fun (name1, props1, s1) (name2, props2, s2) ->
          if name1 <> name2 then raise Not_unifiable ;
          if props1 <> props2 then raise Not_unifiable ;
          f s1 s2 )
        l1 l2
  | DT_record _, _ | DT_variant _, _ -> raise Not_unifiable

let unify ~modulo_props nfree t1 t2 =
  let subs = Array.make nfree None
  and equal =
    if modulo_props then
      (* TODO: It would be ideal if this equality raised on DT_var *)
      Stype.equality_modulo_props
    else Stype.equality
  and s1 = Ttype.to_stype t1
  and s2 = Ttype.to_stype t2 in
  let rec unify s1 s2 =
    let set v s =
      match subs.(v) with
      | None -> subs.(v) <- Some s
      | Some s' -> if not (equal s s') then raise Not_unifiable
    in
    match ((s1, s2) : Stype.t * Stype.t) with
    | _, DT_var _ -> raise (Invalid_argument "unify: free variable in ttype")
    | DT_var k, s2 -> set k s2
    | DT_int, DT_int | DT_float, DT_float | DT_string, DT_string -> ()
    | DT_option s1, DT_option s2
     |DT_list s1, DT_list s2
     |DT_array s1, DT_array s2 ->
        unify s1 s2
    | DT_tuple l1, DT_tuple l2 -> unify_list_iter2 unify l1 l2
    | DT_node n1, DT_node n2 -> node_iter2 unify n1 n2
    | DT_arrow (n1, s1, s1'), DT_arrow (n2, s2, s2') ->
        if n1 <> n2 then raise Not_unifiable ;
        unify s1 s2 ;
        unify s1' s2'
    | DT_object l1, DT_object l2 ->
        unify_list_iter2
          (fun (n1, s1) (n2, s2) ->
            if n1 <> n2 then raise Not_unifiable ;
            unify s1 s2 )
          l1 l2
    | DT_abstract (n1, l1), DT_abstract (n2, l2) ->
        if n1 <> n2 then raise Not_unifiable ;
        unify_list_iter2 unify l1 l2
    | DT_prop (_, t1), t2 when modulo_props -> unify t1 t2
    | t1, DT_prop (_, t2) when modulo_props -> unify t1 t2
    | DT_prop (p1, t1), DT_prop (p2, t2) when p1 = p2 -> unify t1 t2
    | DT_prop _, _
     |DT_int, _
     |DT_float, _
     |DT_string, _
     |DT_option _, _
     |DT_list _, _
     |DT_array _, _
     |DT_tuple _, _
     |DT_node _, _
     |DT_arrow _, _
     |DT_object _, _
     |DT_abstract _, _ ->
        raise Not_unifiable
  in
  unify s1 s2 ; subs

(* Use stype unification & magic to satisfy interface *)

let ttype : type a. Stype.t option -> a Ttype.t = function
  | None -> Obj.magic Std.unit_t
  (* Unification succeeded, but type variable was
                                    not used. *)
  | Some s -> Obj.magic s

(* Unification succeeded by instantiating type
                             variable with stype s. *)

module U0 (P : PARAM) (A : T0) (B : T0) = struct
  include A
  include P

  type t' = B.t

  let eq : (t, t') TypEq.t =
    match unify ~modulo_props 0 t B.t with
    | [||] -> Obj.magic TypEq.refl
    | _ -> assert false
end

type var

let var i : var Ttype.t = Obj.magic (Stype.DT_var i)
let v0 = var 0
let v1 = var 1

module U1 (P : PARAM) (A : T1) (B : T0) = struct
  include A
  include P

  type t' = B.t
  type a

  let (a_t : a Ttype.t), (eq : (a t, t') TypEq.t) =
    match unify ~modulo_props 1 (t v0) B.t with
    | [|a|] -> (ttype a, Obj.magic TypEq.refl)
    | _ -> assert false
end

module U2 (P : PARAM) (A : T2) (B : T0) = struct
  include A
  include P

  type t' = B.t
  type a
  type b

  let (a_t : a Ttype.t), (b_t : b Ttype.t), (eq : ((a, b) t, t') TypEq.t) =
    match unify ~modulo_props 2 (t v0 v1) B.t with
    | [|a; b|] -> (ttype a, ttype b, Obj.magic TypEq.refl)
    | _ -> assert false
end
