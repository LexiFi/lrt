(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Pattern matching on dynamic types. *)

(** {3 Example}

    TODO: example
*)

(** {3 Properties}

    TODO: describe that properties are matched by key. Values are ignored.
*)

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

module Make (Return : sig
  type 'a t
end) : S with type 'a return = 'a Return.t = struct
  (* TODO: document, that symbol table is stored in Module, not in Matcher.t *)

  module IntMap = Ext.Int.Map
  module Symbol = Matcher_symbol.Make ()
  module Tree = Matcher_index.Make (Symbol)

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
  type t = candidate Tree.t

  let create ~modulo_props : t = Tree.create ~modulo_props

  let add tree (type a) ~(t : a Ttype.t) (return : a return) =
    let c = C0 (module struct type t = a

                              let t = t
                              let return = return end) in
    Tree.add tree (Ttype.to_stype t) c

  let add0 tree (module C : C0) =
    Tree.add tree (Ttype.to_stype C.t) (C0 (module C))

  type var

  let var i : var Ttype.t = Obj.magic (Stype.DT_var i)
  let v0 = var 0
  let v1 = var 1

  let add1 tree (module C : C1) =
    Tree.add tree (Ttype.to_stype (C.t v0)) (C1 (module C))

  let add2 tree (module C : C2) =
    Tree.add tree (Ttype.to_stype (C.t v0 v1)) (C2 (module C))

  let ttype : type a. int -> Tree.substitution -> a Ttype.t =
   fun i map ->
    match IntMap.find_opt i map with
    | None -> Obj.magic Std.unit_t
    (* Unification succeeded, but type variable
                                        was not used. *)
    | Some s -> Obj.magic s

  (* Unification succeeded by instantiating type
                                 variable with stype s. *)

  let apply : type a. t -> t:a Ttype.t -> a matched option =
   fun tree ~t ->
    let stype = Ttype.to_stype t in
    match Tree.get tree stype with
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
