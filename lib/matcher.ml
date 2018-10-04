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

module Step : sig
  type t
  val compare : t -> t -> int
  val register_stype : modulo_props: bool -> Stype.t -> t * Stype.t list
  val find_stype : modulo_props: bool -> Stype.t -> t option * Stype.t list
end = struct
  type t = int
  (* Do we need the mapping to int, or is it sufficient to place distinguishing
     information in a variant and use polymorphic compare? *)

  let last = ref 0
  let fresh () = incr last; !last

  let int = fresh ()
  let float = fresh ()
  let string = fresh ()
  let array = fresh ()
  let list = fresh ()
  let option = fresh ()
  let arrow = fresh ()

  module PropMap = Map.Make (struct
      type t = Stype.properties
      let compare = compare
    end)

  module AbstractMap = Map.Make (struct
      type t = int * string
      let compare = compare
    end)

  module RecordMap = Map.Make (struct
      type t = string * Stype.record_repr * (string * Stype.properties) list
      let compare = compare
    end)

  let propMap = ref PropMap.empty
  let abstractMap = ref AbstractMap.empty
  let recordMap = ref RecordMap.empty

  let identify: type m k. (k -> m -> int option) -> (k -> int -> m -> m)
      -> m ref -> k -> int =
    fun find add m x ->
    match find x !m with
    | Some id -> id
    | None ->
      let id = fresh () in
      m := add x id !m; id

  let identify_props =
    identify PropMap.find_opt PropMap.add propMap

  let identify_abstract =
    identify AbstractMap.find_opt AbstractMap.add abstractMap

  let map_record name flds repr =
    let flds, stypes = List.fold_left (fun (flds, stypes) (name, prop, s) ->
        ((name, prop) :: flds, s :: stypes)) ([], []) flds
    in (name, repr, flds), stypes

  let identify_record =
    identify RecordMap.find_opt RecordMap.add recordMap

  let rec register_stype : modulo_props: bool -> Stype.t -> t * Stype.t list =
    fun ~modulo_props -> function
      | DT_int -> int, []
      | DT_float -> float, []
      | DT_string -> string, []
      | DT_list a -> list, [a]
      | DT_array a -> array, [a]
      | DT_option a -> option, [a]
      | DT_arrow (_, a, b) -> arrow, [a; b]
      | DT_prop (_, s) when modulo_props -> register_stype ~modulo_props s
      | DT_prop (p, s) -> (identify_props p), [s]
      | DT_tuple l -> -(List.length l), l
      | DT_abstract (name, args) ->
        identify_abstract (List.length args, name), args
      | DT_node { rec_descr = DT_record {record_fields; record_repr}
                ; rec_name; _ } ->
        let key, types = map_record rec_name record_fields record_repr in
        (* TODO: verify, that rec_args are indeed irrelevant *)
        (* TODO: The same record can be defined twice in different modules
           and pass this comparison. Solution: insert unique ids on
           [@@deriving t]. Or check what the existing rec_uid is doing.
           This would speed up comparison quite a bit, ie. only args need
           to be compared *)
        identify_record key, types
      | DT_node _ -> failwith "TODO: handle variants"
      | DT_object _ -> failwith "TODO: handle objects"
      | DT_var _i -> failwith "TODO: handle variables"

  let rec find_stype : modulo_props: bool -> Stype.t -> t option * Stype.t list
    = fun ~modulo_props -> function
      | DT_int -> Some int, []
      | DT_float -> Some float, []
      | DT_string -> Some string, []
      | DT_list a -> Some list, [a]
      | DT_array a -> Some array, [a]
      | DT_option a -> Some option, [a]
      | DT_arrow (_, a, b) -> Some arrow, [a; b]
      | DT_prop (_, s) when modulo_props -> find_stype ~modulo_props s
      | DT_prop (p, s) -> PropMap.find_opt p !propMap, [s]
      | DT_tuple l -> Some (-(List.length l)), l
      | DT_abstract (name, args) ->
        AbstractMap.find_opt (List.length args, name) !abstractMap, args
      | DT_node { rec_descr = DT_record {record_fields; record_repr}
                ; rec_name; _ } ->
        let key, types = map_record rec_name record_fields record_repr in
        RecordMap.find_opt key !recordMap, types
      | DT_node _ -> failwith "TODO: handle variants"
      | DT_object _ -> failwith "TODO: handle objects"
      | DT_var _i -> failwith "TODO: handle variables"

  let compare = compare
end

module StepMap = Map.Make(Step)

type 'a trie =
  | Node of 'a trie StepMap.t
  | Child of 'a

(* Ignore unfinished code. TODO: Continue here on Monday. *)
let _ = Step.register_stype, Step.compare, Step.find_stype,
        Node StepMap.empty, Child ()

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
