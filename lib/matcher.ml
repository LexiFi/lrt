(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Pattern matching on dynamic types. *)

(** In order to gain some intuition about this module can be used, consult the
    below {!example} or the implementation of {!Json.conv}. *)

(** A matcher with a given return type. *)
module type S = sig
  (** A matcher. Cases can be added using {!add} and it can be applied using
      {!apply}. *)
  type t

  (** The return type of the matcher. Patterns are registered together with a
      value of this type. On a successful match the registered value is
      returned.
  *)
  type 'a return

  val create : modulo_props:bool -> t
  (** A fresh matcher without any registered pattern. The parameter
      [modulo_props] controls whether properties are ignored or interpreted as
      distinguishing features of runtime types.

      {4 Properties}

      The handling of properties during matching requires special attention.

      When [modulo_props:false] (default), properties are distinguishing
      features of runtime types. Two types that only differ in their outermost
      properties (e.g. one without and one with) will not unify and thus not
      match. The same is true if the properties are the same but their order is
      different. We attempt to provide associativity in the sense of
      {!Ttype.consume_outer_props}. A property is uniquely identified by its
      key, values are ignored during matching.

      When [modulo_props:true], properties are ignored for unification.
      Nevertheless, properties are preserved within the substituted runtime
      types.

      TODO:
      - Think about moving the [modulo_props] argument to {!apply}.
      - Think about automatic property handling: first check for pattern with
        property, if not registered, strip first property and retry.
  *)

  (** {3 Pattern candidates} *)

  val add : t -> t:'a Ttype.t -> 'a return -> unit
  (** [add m ~t return] adds pattern [t] with right hand side [return] to
      the matcher [m]. *)

  module type C0 = sig
    include Unify.T0

    val return : t return
    (** Is returned when {!t} is given to {!apply}. *)
  end

  val add0 : t -> (module C0) -> unit
  (** Add a case to the matcher. Equivalent to {!add}. *)

  module type C1 = sig
    include Unify.T1

    val return : 'a Ttype.t -> 'a t return
    (** Used to obtain {!M1.return} on a match. *)
  end

  val add1 : t -> (module C1) -> unit
  (** Adds a case to a matcher. One free variable.*)

  module type C2 = sig
    include Unify.T2

    val return : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t return
    (** Used to obtain {!M2.return} on a match. *)
  end

  val add2 : t -> (module C2) -> unit
  (** Adds a case to a matcher. Two free variables. *)

  (** {3 Matching results} *)

  (** A matching result without substituted variables. *)
  module type M0 = sig
    include Unify.T0

    (** The type given to {!apply}. *)
    type matched

    val eq : (t, matched) TypEq.t
    (** Proofs the equality of the {!add}ed type and the matched type
        to the type checker. *)

    val return : t return
    (** The result of the match. Corresponds to
        the right hand side of [->] in usual OCaml pattern matching. *)
  end

  (** A matching result with one substituted variable. *)
  module type M1 = sig
    include Unify.T1

    (** First type variable. *)
    type a

    (** The type given to {!apply}. *)
    type matched

    val eq : (a t, matched) TypEq.t
    (** Proofs the equality of the {!add}ed type and the matched type
        to the type checker. *)

    val return : a t return
    (** The result of the match with substituted type variable. Corresponds to
        the right hand side of [->] in usual OCaml pattern matching. *)
  end

  (** A matching result with two substituted variables. *)
  module type M2 = sig
    include Unify.T2

    (** First type variable. *)
    type a

    (** Second type variable. *)
    type b

    (** The type given to {!apply}. *)
    type matched

    val eq : ((a, b) t, matched) TypEq.t
    (** Proofs the equality of the {!add}ed type and the matched type
        to the type checker. *)

    val return : (a, b) t return
    (** The result of the match with substituted type variables. Corresponds to
        the right hand side of [->] in usual OCaml pattern matching. *)
  end

  type 'a matched =
    | M0 of (module M0 with type matched = 'a)
    | M1 of (module M1 with type matched = 'a)
    | M2 of (module M2 with type matched = 'a)

  (** {3 Application} *)

  val apply : t -> t:'a Ttype.t -> 'a matched option
  (** [apply matcher ~t] matches runtime type [t] against previously registered
      patterns in [matcher]. *)

  val apply_exn : t -> t:'a Ttype.t -> 'a matched
  (** See {!apply}. Raises {!Not_found} if no matching pattern is available. *)
end

(** Instantiate a matcher with a result type. *)
module Make (Return : sig
  type 'a t
end) : S with type 'a return = 'a Return.t = struct
  (* TODO: document, that symbol table is stored in Module, not in Matcher.t *)

  module Index = Matcher_index.Tree
  module IntMap = Ext.Int.Map

  (* TODO: How can I avoid this duplication of signatures? *)
  type 'a return = 'a Return.t

  module type C0 = sig
    include Unify.T0

    val return : t return
  end

  module type C1 = sig
    include Unify.T1

    val return : 'a Ttype.t -> 'a t return
  end

  module type C2 = sig
    include Unify.T2

    val return : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t return
  end

  module type M0 = sig
    include Unify.T0

    type matched

    val eq : (t, matched) TypEq.t
    val return : t return
  end

  module type M1 = sig
    include Unify.T1

    type matched
    type a

    val eq : (a t, matched) TypEq.t
    val return : a t return
  end

  module type M2 = sig
    include C2

    type matched
    type a
    type b

    val eq : ((a, b) t, matched) TypEq.t
    val return : (a, b) t return
  end

  type 'a matched =
    | M0 of (module M0 with type matched = 'a)
    | M1 of (module M1 with type matched = 'a)
    | M2 of (module M2 with type matched = 'a)

  type candidate = C0 of (module C0) | C1 of (module C1) | C2 of (module C2)
  type t = candidate Index.t

  let create ~modulo_props : t = Index.create ~modulo_props

  let add tree (type a) ~(t : a Ttype.t) (return : a return) =
    let c = C0 (module struct type t = a

                              let t = t
                              let return = return end) in
    Index.add tree (Ttype.to_stype t) c

  let add0 tree (module C : C0) =
    Index.add tree (Ttype.to_stype C.t) (C0 (module C))

  type var

  let var i : var Ttype.t = Obj.magic (Stype.DT_var i)
  let v0 = var 0
  let v1 = var 1

  let add1 tree (module C : C1) =
    Index.add tree (Ttype.to_stype (C.t v0)) (C1 (module C))

  let add2 tree (module C : C2) =
    Index.add tree (Ttype.to_stype (C.t v0 v1)) (C2 (module C))

  let ttype : type a. int -> Index.substitution -> a Ttype.t =
   fun i map ->
    match IntMap.find_opt i map with
    | None -> Obj.magic Std.unit_t
    (* Unification succeeded, but type variable
                                        was not used. *)
    | Some s -> Obj.magic s

  (* Unification succeeded by instantiating type
                                 variable with stype s. *)

  let[@landmark] apply : type a. t -> t:a Ttype.t -> a matched option =
   fun tree ~t ->
    let stype = Ttype.to_stype t in
    match Index.get tree stype with
    | None -> None
    | Some (C0 (module C : C0), map) ->
        assert (IntMap.cardinal map = 0) ;
        let module M : M0 with type matched = a = struct
          include C

          type matched = a

          let eq = Obj.magic TypEq.refl
        end in
        Some (M0 (module M))
    | Some (C1 (module C : C1), map) ->
        assert (IntMap.cardinal map < 2) ;
        let module M : M1 with type matched = a = struct
          include C

          type matched = a
          type a

          let eq = Obj.magic TypEq.refl
          let return = return (ttype 0 map)
        end in
        Some (M1 (module M))
    | Some (C2 (module C : C2), map) ->
        assert (IntMap.cardinal map < 3) ;
        let module M : M2 with type matched = a = struct
          include C

          type matched = a
          type a
          type b

          let eq = Obj.magic TypEq.refl
          let return = return (ttype 0 map) (ttype 1 map)
        end in
        Some (M2 (module M))

  let apply_exn tree ~t =
    match apply tree ~t with None -> raise Not_found | Some m -> m
end

(** {3 Example}

    We will match on these example types.

    {[
  type t0 = string list

  and 'a t1 = 'a array

  and ('a, 'b) t2 = ('a * 'b) option [@@deriving t]
    ]}

    The example pattern match should print the type. An appropriate result type
    is [unit -> unit].

    {[
  module Matcher = Matcher.Make (struct type 'a t = unit -> unit end)

  let m = Matcher.create ~modulo_props:true
  let pp_ty = Ttype.print
    ]}

    The different cases are registered one by one. Free variables will be
    substituted in the returned result.

    {[

  let () =
    let open Matcher in
    add m ~t:t0_t (fun () -> Format.printf "t0 = %a\n%!" pp_ty t0_t) ;
    add1 m
      ( module struct
        type 'a t = 'a t1 [@@deriving t]

        let return a_t () =
          Format.printf "%a t1 = %a\n%!" pp_ty a_t pp_ty (t1_t a_t)
      end ) ;
    add2 m
      ( module struct
        type ('a, 'b) t = ('a, 'b) t2 [@@deriving t]

        let return a_t b_t () =
          Format.printf "(%a, %a) t2 = %a\n%!" pp_ty a_t pp_ty b_t pp_ty
            (t2_t a_t b_t)
      end )
    ]}

    The handling of matcher results needs some boilerplate code.

    {[

  let apply : type a. Matcher.t -> t:a Ttype.t -> unit =
   fun matcher ~t ->
    let open Matcher in
    match apply matcher ~t with
    | None -> print_endline "Not found"
    | Some (M0 (module M : M0 with type matched = a)) -> M.return ()
    | Some (M1 (module M : M1 with type matched = a)) -> M.return ()
    | Some (M2 (module M : M2 with type matched = a)) -> M.return ()

    ]}

    Now everything is set up and the matcher is ready for application.

    {[

  let () =
    apply m ~t:[%t: t0] ;
    apply m ~t:[%t: int t1] ;
    apply m ~t:[%t: bool t1] ;
    apply m ~t:[%t: float option] ;
    apply m ~t:[%t: (float, string) t2] ;
    apply m ~t:[%t: (unit, string) t2]
    ]}

    The above example program produces the following output.

    {[
      t0 = string list
      int t1 = int array
      bool t1 = bool array
      Not found
      (float, string) t2 = (float * string) option
      (unit, string) t2 = (unit * string) option
    ]}
*)
