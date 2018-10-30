(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

open Std

(* Utils. *)

(* Random number generator. *)

let int_max_bound = 0x3FFFFFFF

(* 2^30 - 1 *)

module State : sig
  type t

  val create : int -> t
  val int : t -> int * int -> int * t
  val float : t -> int -> int * int -> float * t

  (* makes two independent generators from one. *)
  val split : t -> t * t
  val split_many : int -> t -> t array
  val split_bits : int -> t -> t
end = struct
  type t = int * int

  let split (i, j) = ((i + 1, (-3 * j) + 1), (i + 1, (-3 * j) + 2))
  let create i = (i, 0)

  let split_many n state =
    let a = Array.make n state in
    let current = ref state in
    for i = 0 to n - 1 do
      let st1, st2 = split !current in
      Array.unsafe_set a i st1 ;
      current := st2
    done ;
    a

  let rec split_bits_aux i bits state =
    if i = 0 then state
    else
      let st1, st2 = split state in
      split_bits_aux (i - 1) (bits lsr 1) (if bits land 1 = 0 then st1 else st2)

  let split_bits = split_bits_aux 30
  let chr i = Char.chr (i land 0xff)

  let set seed (i, j) =
    Bytes.set seed 0 (chr i) ;
    Bytes.set seed 1 (chr (i lsr (1 * 8))) ;
    Bytes.set seed 2 (chr (i lsr (2 * 8))) ;
    Bytes.set seed 3 (chr (i lsr (3 * 8))) ;
    Bytes.set seed 4 (chr j) ;
    Bytes.set seed 5 (chr (j lsr (1 * 8))) ;
    Bytes.set seed 6 (chr (j lsr (2 * 8))) ;
    Bytes.set seed 7 (chr (j lsr (3 * 8)))

  let extract d =
    Char.code d.[0]
    + (Char.code d.[1] lsl 8)
    + (Char.code d.[2] lsl 16)
    + (Char.code d.[3] lsl 24)

  let bits =
    let seed = Bytes.create 8 in
    fun state ->
      ( set seed state ;
        extract (Digest.bytes seed) land 0x3FFFFFFF
        : int )

  let rec intaux s n =
    let s1, s2 = split s in
    let r = bits s1 in
    let v = r mod n in
    if r - v > int_max_bound - n + 1 then intaux s2 n else (v, s2)

  let int s bound =
    if bound > int_max_bound || bound <= 0 then
      failwith "Check.int used with invalid range"
    else intaux s bound

  let float s =
    let s1, s2 = split s in
    (float_of_int (bits s1) /. float_of_int (succ int_max_bound), s2)

  let int st (lo, hi) =
    if hi < lo then invalid_arg "Check.State.int" ;
    let n, stp = int st (hi - lo + 1) in
    (n + lo, stp)

  let floor digits f =
    let shift = 10. ** float_of_int digits in
    floor (f *. shift) /. shift

  let float st digits (lo, hi) =
    if hi < lo then invalid_arg "Check.State.float" ;
    let m, stp = float st in
    let e, stp = int stp (lo, hi) in
    (floor (digits - e) (m *. (10. ** float_of_int e)), stp)
end

(* Generators. *)

module Gen : sig
  type 'a t

  val run : int -> State.t -> 'a t -> State.t * 'a
  val return : 'a -> 'a t
  val join : 'a t t -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val seq : ('a -> 'b) t -> 'a t -> 'b t
  val try_with : 'a t -> (exn -> 'a t) -> 'a t
  val list_sequence : 'a t list -> 'a list t
  val array_sequence : 'a t array -> 'a array t
  val bytes_sequence : int -> char t -> Bytes.t t
  val arrow : 'b t -> ('a -> 'b) t
  val sized : (int -> 'a t) -> 'a t
  val choose_int : int * int -> int t
  val choose_float : int -> int * int -> float t
end = struct
  type 'a t = int -> State.t -> 'a

  let run len (state : State.t) (g : 'a t) =
    let st1, st2 = State.split state in
    (st2, g len st1)

  let return x _ _ = x

  let bind f g len state =
    let state1, state2 = State.split state in
    let x = f len state1 in
    g x len state2

  let map f g len state = f (g len state)

  let map2 f g1 g2 len state =
    let state1, state2 = State.split state in
    let x1 = g1 len state1 in
    let x2 = g2 len state2 in
    f x1 x2

  let seq gf gx len state =
    let state1, state2 = State.split state in
    gf len state1 (gx len state2)

  let try_with f g len state = try f len state with e -> g e len state

  let list_sequence gs len state =
    let states = State.split_many (List.length gs) state in
    List.mapi (fun i g -> g len states.(i)) gs

  let array_sequence gs len state =
    let states = State.split_many (Array.length gs) state in
    Array.mapi (fun i g -> g len states.(i)) gs

  let bytes_sequence n g len state =
    let states = State.split_many n state in
    Bytes.init n (fun i -> g len states.(i))

  let join gg len state =
    let state1, state2 = State.split state in
    gg len state1 len state2

  let arrow g len state x = g len (State.split_bits (Hashtbl.hash x) state)

  let sized (gg : int -> 'a t) len state =
    assert (0 <= len) ;
    gg len len state

  let choose_int range _len st = fst (State.int st range)
  let choose_float digits range _len st = fst (State.float st digits range)
end

include Gen

type 'a gen = 'a t

let pred_size sz = max 0 (sz - 1)
let ( >>= ) = bind
let ( <$> ) = map
let ( <*> ) = seq

let elements_sized l size =
  match l with
  | [] -> failwith "Mlfi_check.elements used with empty list"
  | xs -> List.nth xs <$> choose_int (0, min size (List.length xs - 1))

let elements l = elements_sized l max_int
let oneof gs = join (elements gs)
let oneof_lazy gs = elements gs >>= Lazy.force

let elements_freq_lazy = function
  | [] -> failwith "Mlfi_check.frequency used with empty list"
  | xs0 ->
      let tot = List.fold_left (fun acc (xs, _) -> acc + xs) 0 xs0 in
      let rec pick n = function
        | (k, x) :: _ when n <= k -> Lazy.force x
        | (k, _) :: xs -> pick (n - k) xs
        | _ -> assert false
      in
      choose_int (1, tot) >>= fun n -> return (pick n xs0)

let elements_freq l =
  elements_freq_lazy (List.map (fun (n, g) -> (n, lazy g)) l)

let oneof_freq_lazy l = join (elements_freq_lazy l)
let oneof_freq l = join (elements_freq l)
let list_copy n g = list_sequence (Ext.List.copy n g)
let array_copy n g = array_sequence (Array.make n g)
let rec until p g = g >>= fun x -> if p x then return x else until p g
let unit = return ()
let bool = (function 0 -> false | _ -> true) <$> choose_int (0, 1)
let int_of_size n = choose_int (-n, n)
let int = sized int_of_size
let choose_char r = Char.chr <$> choose_int r

let char_of_size (inf, sup) n =
  let mid = (inf + sup) / 2 in
  let lo = max inf (mid - n) in
  let hi = min sup (mid + n) in
  choose_char (lo, hi)

let min_char = 32
let max_char = 127
let char = sized (char_of_size (min_char, max_char))
let lowercase_letter = sized (char_of_size (Char.code 'a', Char.code 'z'))
let uppercase_letter = sized (char_of_size (Char.code 'A', Char.code 'Z'))
let digit = sized (char_of_size (48, 57))

let float_of_size n =
  oneof_freq
    [ (* 95% regular float *) (57, choose_float n (-n, n))
    ; (1, return nan)
    ; (1, return infinity)
    ; (1, return neg_infinity) ]

let float = sized float_of_size
let option g = oneof [return None; (fun x -> Some x) <$> g]
let tuple g1 g2 = (fun x y -> (x, y)) <$> g1 <*> g2
let tuple3 g1 g2 g3 = (fun x y z -> (x, y, z)) <$> g1 <*> g2 <*> g3
let list_of_size size g = choose_int (0, size) >>= fun n -> list_copy n g
let array_of_size size g = choose_int (0, size) >>= fun n -> array_copy n g

let string_of_size size g =
  choose_int (0, size) >>= fun n -> Bytes.to_string <$> bytes_sequence n g

let list g = sized (fun n -> list_of_size n g)
let array g = sized (fun n -> array_of_size n g)

let string =
  sized (fun n -> string_of_size n (char_of_size (33, 126) (pred_size n)))

(* Utils. *)

let string_of_list l =
  let b = Buffer.create 8 in
  List.iter (Buffer.add_char b) l ;
  Buffer.contents b

let lazy_ g = g >>= fun x -> return (lazy x)

let list_of_size1 size g =
  choose_int (1, max 1 size) >>= fun n -> list_copy n g

let string_of1 g =
  choose_int (1, 10)
  >>= fun n -> list_of_size1 n g >>= fun l -> return (string_of_list l)

let string_lowercase = string_of1 lowercase_letter

(* Mlfi-specific generators. *)

let mlfi_keywords =
  [ "and"; "as"; "assert"; "asr"; "begin"; "class"; "constraint"; "do"; "done"
  ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
  ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
  ; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"
  ; "mod"; "module"; "mutable"; "new"; "object"; "of"; "open"; "or"; "private"
  ; "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"
  ; "virtual"; "when"; "while"; "with" ]

module StringSet = Set.Make (struct
  type t = string

  let compare = Pervasives.compare
end)

let mlfi_keywords = StringSet.of_list mlfi_keywords

let uniq proj l =
  let tbl = Hashtbl.create 5 in
  let seen x = Hashtbl.mem tbl (proj x) in
  let mark x =
    Hashtbl.add tbl (proj x) () ;
    true
  in
  List.filter (fun x -> (not (seen x)) && mark x) l

let all_different proj gen =
  choose_int (1, 5) >>= fun n -> uniq proj <$> list_copy n gen

let ident first_letter =
  choose_int (1, 7)
  >>= fun n ->
  first_letter
  >>= fun c ->
  Bytes.make n
  <$> oneof [lowercase_letter; uppercase_letter; digit; return '_']
  >>= fun s ->
  Bytes.set s 0 c ;
  return (Bytes.to_string s)

let lident =
  until
    (fun ident -> not (StringSet.mem ident mlfi_keywords))
    (ident lowercase_letter)

let uident = ident uppercase_letter

module UGen = struct
  type t = Gen : 'a Ttype.t * (int -> 'a gen) -> t

  let create ~t f = Gen (t, f)
end

let rec find_custom : type a. UGen.t list -> a Ttype.t -> (int -> a gen) option
    =
 fun l t ->
  match l with
  | [] -> None
  | UGen.Gen (u, fu) :: tl -> (
    match Ttype.equality t u with
    | Some TypEq.Eq -> Some fu
    | None -> find_custom tl t )

class type custom_of_type =
  object
    method apply : 'a. 'a Ttype.t -> (int -> 'a gen) option
  end

let of_list l =
  object (_ : custom_of_type)
    method apply t = find_custom l t
  end

module H = Hashtbl.Make (struct
  type t = Stype.t

  let equal = Stype.strict_equality
  let hash = Stype.Internal.hash0
end)

let fields_of_record_fields flds = List.map (function _, f -> f) flds

let is_leaf : type a. a Ttype.t -> bool =
 fun tty ->
  let open Xtype in
  let seen = H.create 12 in
  let replace t = H.replace seen (Ttype.to_stype t.t) () in
  let rec really_loop : type a. a Xtype.t -> bool =
   fun t ->
    match Lazy.force t.xt with
    | Unit -> true
    | Bool -> true
    | Char -> true
    | Int -> true
    | Int32 -> true
    | Int64 -> true
    | Nativeint -> true
    | Float -> true
    | String -> true
    | Option t -> loop t
    | List t -> loop t
    | Array t -> loop t
    | Function a -> loop a.res_t
    | Sum sum ->
        replace t ;
        List.for_all
          Xtype.(
            function
            | Constant _ -> true
            | Regular c -> loop_fields c.rc_flds
            | Inlined c -> loop_fields (fields_of_record_fields c.ic_flds))
          sum.s_cstrs
    | Tuple tup -> replace t ; loop_fields tup.t_flds
    | Record r ->
        replace t ;
        loop_fields (fields_of_record_fields r.r_flds)
    | Lazy t -> loop t
    | Prop (_, t) -> loop t
    | Object _ -> false
    | Abstract _ -> true
  and loop_fields : type a. a Xtype.field list -> bool =
   fun fields ->
    List.for_all (function Xtype.Field field -> loop field.typ) fields
  and loop : type a. a Xtype.t -> bool =
   fun t -> (not (H.mem seen (Ttype.to_stype t.t))) && really_loop t
  in
  loop (Xtype.of_ttype tty)

module Test : sig end = struct
  [@@@warning "-37"]

  type il = Nil | Cons of int * il [@@deriving t]
  type tata = Empty | Node of tata * int * tata [@@deriving t]
  type toto = A | B [@@deriving t]

  type tree = Leaf of int | Node of forest

  and forest = Nil | Cons of tree * forest [@@deriving t]

  type point = {x: int; y: toto} [@@deriving t]

  let%test _ = not (is_leaf il_t)
  let%test _ = not (is_leaf tata_t)
  let%test _ = is_leaf toto_t
  let%test _ = not (is_leaf tree_t)
  let%test _ = not (is_leaf forest_t)
  let%test _ = is_leaf point_t
end

type ('a, 'b) mk = ('a Xtype.Make.t -> unit) -> 'b

let of_type_gen_sized : type a. UGen.t list -> t:a Ttype.t -> int -> a gen =
 fun l ~t sz ->
  let find_custom = of_list l in
  let rec of_type_default_sized : type a. t:a Ttype.t -> int -> a gen =
   fun ~t sz ->
    let szp = pred_size sz in
    let fields : type a b. (a, b) mk -> a Xtype.field list -> int -> b gen =
     fun mk tup sz ->
      let f (Xtype.Field f) =
        of_type_sized ~t:f.typ.t sz
        >>= fun x -> return (fun b -> Xtype.Make.set b f x)
      in
      list_sequence (List.map f tup)
      >>= fun l -> return (mk (fun b -> List.iter (fun f -> f b) l))
    in
    let open Xtype in
    match Xtype.xtype_of_ttype t with
    | Unit -> unit
    | Bool -> bool
    | Int -> int_of_size szp
    | Float -> float_of_size szp
    | String -> string_of_size (sz / 2) (char_of_size (min_char, max_char) szp)
    | Option {t; _} -> option (of_type_sized ~t szp)
    | List {t; _} -> list_of_size (sz / 2) (of_type_sized ~t szp)
    | Array {t; _} -> array_of_size (sz / 2) (of_type_sized ~t szp)
    | Function a -> arrow (of_type_sized ~t:a.res_t.t szp)
    | Tuple t -> fields (Xtype.Make.tuple t) t.t_flds szp
    | Record r ->
        fields (Xtype.Make.record r) (fields_of_record_fields r.r_flds) szp
    | Sum sum ->
        let open Xtype in
        let f (type a) (c : a constructor) : a gen Lazy.t option =
          if sz > 0 || is_leaf t then
            Some
              ( lazy
                ( match c with
                | Constant c -> return (Builder.constant_constructor c)
                | Regular c ->
                    fields (Make.regular_constructor c) c.rc_flds (sz / 2)
                | Inlined c ->
                    fields
                      (Make.inlined_constructor c)
                      (fields_of_record_fields c.ic_flds)
                      (sz / 2) ) )
          else None
        in
        oneof_lazy (Ext.List.choose f sum.s_cstrs)
    | Lazy {t; _} -> lazy_ (of_type_sized ~t szp)
    | Prop (_, {t; _}) -> of_type_sized ~t szp
    | Object _ -> failwith "Check.f: reached Object"
    | Abstract _ -> failwith "Check.f: reached Abstract"
    | Char -> failwith "Check.f: reached Char"
    | Int32 -> failwith "Check.f: reached Int32"
    | Int64 -> failwith "Check.f: reached Int64"
    | Nativeint -> failwith "Check.f: reached Nativeint"
  and of_type_sized : type a. t:a Ttype.t -> int -> a gen =
   fun ~t sz ->
    match find_custom#apply t with
    | None -> of_type_default_sized ~t sz
    | Some fu -> fu sz
  in
  of_type_sized ~t sz

let of_type_gen ?size gg ~t =
  match size with
  | Some i -> of_type_gen_sized gg ~t i
  | None -> sized (fun n -> of_type_gen_sized gg ~t n)

let dt_unit : Stype.t gen = unit_t |> Ttype.to_stype |> return
let dt_bool : Stype.t gen = bool_t |> Ttype.to_stype |> return
let dt_int : Stype.t gen = int_t |> Ttype.to_stype |> return
let dt_float : Stype.t gen = float_t |> Ttype.to_stype |> return
let dt_string : Stype.t gen = string_t |> Ttype.to_stype |> return
let dt_list t = (fun t -> Stype.DT_list t) <$> t
let dt_array t = (fun t -> Stype.DT_array t) <$> t
let dt_option t = (fun t -> Stype.DT_option t) <$> t
let dt_tuple tl = (fun tl -> Stype.DT_tuple tl) <$> tl

let rec stype () = join (stype_gen ())

and dt_constructor const_type =
  string_lowercase
  >>= fun name ->
  match const_type with
  | `C_tuple ->
      all_different (fun x -> x) uident
      >>= fun names ->
      list_copy (List.length names) (list (stype ()))
      >>= fun types ->
      return
        (Stype.Internal.create_variant_type name [] (fun _ ->
             ( List.map2
                 (fun name types -> (name, [], Stype.C_tuple types))
                 names types
             , Variant_regular ) ))
  | `C_inline ->
      uident
      >>= fun c_name ->
      dt_record ~inline:true ()
      >>= fun inline_type ->
      return
        (Stype.Internal.create_variant_type name [] (fun _ ->
             ([(c_name, [], C_inline inline_type)], Variant_regular) ))

and dt_record ?(inline = false) () =
  let field = tuple3 lident (return []) (stype ()) in
  all_different (fun (x, _, _) -> x) field
  >>= fun fields ->
  string_lowercase
  >>= fun type_name ->
  let fields = List.map (fun (f, x, y) -> (f, x, y)) fields in
  (* TODO: What happens when all fields are floats? *)
  let repr = if inline then Stype.Record_inline 0 else Record_regular in
  return
    (Stype.Internal.create_record_type type_name [] (fun _ -> (fields, repr)))

and stype_gen () =
  elements_freq_lazy
    [ (1, lazy dt_unit)
    ; (6, lazy dt_bool)
    ; (6, lazy dt_int)
    ; (6, lazy dt_float)
    ; (6, lazy dt_string)
    ; ( 1
      , lazy (choose_int (2, 4) >>= fun n -> dt_tuple (list_copy n (stype ())))
      )
    ; (1, lazy (stype_gen () >>= fun e -> dt_list e))
    ; (1, lazy (stype_gen () >>= fun e -> dt_array e))
    ; (1, lazy (stype_gen () >>= fun e -> dt_option e))
    ; (1, lazy (dt_record ()))
    ; (1, lazy (dt_constructor `C_tuple))
    ; (1, lazy (dt_constructor `C_inline)) ]

let stype = stype ()

let dynamic ?size l =
  let open Ttype in
  stype
  >>= fun s ->
  let (Ttype t) = of_stype s in
  let valgen = of_type_gen ?size l ~t in
  map (fun v -> Dyn (t, v)) valgen

(* Shrinkers *)

type 'a shrink = 'a -> 'a list

module Shrink = struct
  (* Utils. *)

  let string_of_list l =
    let b = Buffer.create 8 in
    List.iter (Buffer.add_char b) l ;
    Buffer.contents b

  (* Shrinking *)
  
  (* Shrinking basic types. *)

  let bool = function true -> [false] | _ -> []
  let char _ = []
  let int = function 0 -> [] | 1 -> [0] | _ -> [0; 1]
  let float = function 0. -> [] | 1. -> [0.] | _ -> [0.; 1.]

  let option shrink = function
    | Some x -> None :: List.map (fun x' -> Some x') (shrink x)
    | None -> []

  let rec removes_of_size k n xs =
    let xs1, xs2 = Ext.List.fst_n k xs in
    if k > n then []
    else if xs2 = [] then [[]]
    else xs2 :: List.map (( @ ) xs1) (removes_of_size k (n - k) xs2)

  let removes n l =
    let rec go k = if k > 0 then removes_of_size k n l :: go (k / 2) else [] in
    let ls = List.concat (go (n / 2)) in
    if n <> 0 then [] :: ls else ls

  (* XXX: try to remove this hack *)

  let rec shrink_one shrink = function
    | [] -> []
    | x :: xs ->
        List.map (fun x' -> x' :: xs) (shrink x)
        @ List.map (fun xs' -> x :: xs') (shrink_one shrink xs)

  let list shrink l =
    let n = List.length l in
    removes n l @ shrink_one shrink l

  let array shrink a = List.map Array.of_list (list shrink (Array.to_list a))

  let to_list s =
    let len = String.length s in
    let l = ref [] in
    for i = len - 1 downto 0 do
      l := s.[i] :: !l
    done ;
    !l

  let string s =
    let l = to_list s in
    List.map string_of_list (list char l)

  let tuple sa sb (a, b) =
    List.map (fun a' -> (a', b)) (sa a) @ List.map (fun b' -> (a, b')) (sb b)

  let tuple3 sa sb sc (a, b, c) =
    let l = tuple sa (tuple sb sc) (a, (b, c)) in
    List.map (fun (a', (b', c')) -> (a', b', c')) l

  let tuple4 sa sb sc sd (a, b, c, d) =
    let l = tuple sa (tuple3 sb sc sd) (a, (b, c, d)) in
    List.map (fun (a', (b', c', d')) -> (a', b', c', d')) l

  let tuple5 sa sb sc sd se (a, b, c, d, e) =
    let l = tuple sa (tuple4 sb sc sd se) (a, (b, c, d, e)) in
    List.map (fun (a', (b', c', d', e')) -> (a', b', c', d', e')) l

  let tuple6 sa sb sc sd se sf (a, b, c, d, e, f) =
    let l = tuple sa (tuple5 sb sc sd se sf) (a, (b, c, d, e, f)) in
    List.map (fun (a', (b', c', d', e', f')) -> (a', b', c', d', e', f')) l

  let tuple7 sa sb sc sd se sf sg (a, b, c, d, e, f, g) =
    let l = tuple sa (tuple6 sb sc sd se sf sg) (a, (b, c, d, e, f, g)) in
    List.map
      (fun (a', (b', c', d', e', f', g')) -> (a', b', c', d', e', f', g'))
      l

  (* let lazy_ shrink (lazy x) = List.map (fun x -> lazy x) (shrink x) *)

  let rec shrink_one2 shrink = function
    | [] | [_] -> assert false
    | [x1; x2] ->
        List.map (fun x1' -> [x1'; x2]) (shrink x1)
        @ List.map (fun x2' -> [x1; x2']) (shrink x2)
    | x :: xs ->
        List.map (fun x' -> x' :: xs) (shrink x)
        @ List.map (fun xs' -> x :: xs') (shrink_one2 shrink xs)

  let list2 shrink l =
    let n = List.length l in
    if n > 2 then removes n l else [] @ shrink_one2 shrink l

  (* Shrinking MLFi-specific types. *)
  
  (* module T = Xtype

     type 's elem = E : 'a list * ('s T.record_builder -> 'a -> unit) -> 's elem

     let rec shrink_record: type a. a T.Record.t -> a shrink = fun record x ->
     let fields = Xtype.Record.fields record in
     let make = Xtype.Record.make record in
     let elems =
      List.map
        (fun (T.Field field) ->
           E (of_type ~t:(T.RecordField.ttype field) (T.RecordField.get field x), T.RecordField.set field)
        )
        fields
     in
     let rec loop = function
      | [] ->
          [fun _ -> ()]
      | E (shrinks, set) :: elems ->
          Ext.List.flatten_map (fun x -> List.map (fun f b -> set b x; f b) (loop elems)) shrinks
     in
     List.map make (loop elems)

     and of_type: type a. t:a ttype -> a shrink = fun ~t ->
     match T.xtype_of_ttype t with
     | T.Unit -> fun () -> []
     | T.Bool -> bool
     | T.Int -> int
     | T.Float -> float
     | T.String -> string
     | T.Option (t, _) -> option (of_type ~t)
     | T.List (t, _) -> list (of_type ~t)
     | T.Array (t, _) -> array (of_type ~t)
     | T.Function _ -> fun _ -> []
     | T.Sum sum ->
        fun x ->
          let T.Constructor c = T.Sum.constructor sum x in
          begin match T.Constructor.project c x with
          | None -> assert false
          | Some y ->
              List.map (T.Constructor.inject c) (of_type ~t:(T.Constructor.ttype c) y)
          end
     | T.Tuple r ->
        shrink_record r
     | T.Record r ->
        shrink_record r
     | T.Lazy (t, _) ->
        lazy_ (of_type ~t)
     | T.Prop (_, t, _) ->
        of_type ~t
     | T.Abstract _ ->
        (fun _ -> [])
     | T.Object _ ->
        (fun _ -> [])
     | T.Char ->
        (fun _ -> [])
     | T.Nativeint ->
        (fun _ -> [])
     | T.Int32 ->
        (fun _ -> [])
     | T.Int64 ->
        (fun _ -> []) *)
end

(* Checking. *)

type 'a test_result =
  | Succeed of {name: string option; test_run: int}
  | Fail of
      { name: string option
      ; test_run: int
      ; seed: int
      ; test_case: 'a
      ; shrink_count: int option }
  | Throw of
      { name: string option
      ; test_run: int
      ; seed: int
      ; test_case: 'a
      ; shrink_count: int option
      ; backtrace: string }

let depthmax = 17

let test num ~seed ?name ~generator ?(depthmax = depthmax) ?shrink prop =
  let state = State.create seed in
  let rec minimize prop shrink x n =
    let xs = shrink x in
    match List.find (fun x -> not (prop x)) xs with
    | x' -> minimize prop shrink x' (n + 1)
    | exception Not_found -> (x, n)
  in
  let rec go n state =
    let len = depthmax * (num - n) / num in
    (* Printf.fprintf stderr "Mlfi_check.test %d/%d len=%d\n%!" (num - n) num len; *)
    if n = 0 then Succeed {name; test_run= num}
    else
      let state, x = run len state generator in
      let prop_holds, backtrace_opt, prop =
        try
          let r = prop x in
          (r, None, prop)
        with e -> (
          let backtrace =
            Printf.sprintf "%s\n%s" (Printexc.to_string e)
              (Printexc.get_backtrace ())
          in
          (false, Some backtrace, fun x -> try prop x with _ -> false) )
      in
      if prop_holds then go (n - 1) state
      else
        let test_case, shrink_count =
          match shrink with
          | Some f ->
              let x', n = minimize prop f x 0 in
              (x', Some n)
          | None -> (x, None)
        in
        let _ = prop test_case in
        (* To print debug output a last time. *)
        match backtrace_opt with
        | Some backtrace ->
            Throw
              { name
              ; test_run= num - n + 1
              ; seed
              ; test_case
              ; shrink_count
              ; backtrace }
        | None ->
            Fail {name; test_run= num - n + 1; seed; test_case; shrink_count}
  in
  go num state

(* Property combinators. *)

let ( => ) a b = if a then b else true
let ( ==> ) a b = if a then b () else true
