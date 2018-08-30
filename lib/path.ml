open Dynt_core

(***************************************************************************)
(*  Copyright (C) 2000-2018 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

module Internal =
  struct
    (* keep in sync with translcore.ml. In particular, we currently assume Constructor has tag 1. *)
    type step =
      | Field of string
      | Constructor of string * int
      | Tuple_nth of int
      | List_nth of int
      | Array_nth of int

    let print_step ppf = function
      | Field s -> Format.fprintf ppf ".%s" s
      | Constructor (s, _) -> Format.fprintf ppf ".%s" s
      | Tuple_nth n -> Format.fprintf ppf ".(%i)" n
      | List_nth n -> Format.fprintf ppf ".[%i]" n
      | Array_nth n -> Format.fprintf ppf ".[|%i|]" n

    let print_steps ppf l =
      List.iter (print_step ppf) l
  end

open Internal

type kind = [`Root|`Constructor|`Field|`List|`Array|`Tuple]
[@@abstract]
[@@deriving t]

type ('a, 'b, +'c) t = step list [@@deriving t][@@abstract]
type ('a, 'b) field = ('a, 'b, [`Field]) t
type ('a, 'b) constructor = ('a, 'b, [`Constructor]) t
type ('a, 'b) tuple = ('a, 'b, [`Tuple]) t
type ('a, 'b) composed = ('a, 'b, kind) t
type ('a, 'b) list_step = ('a, 'b, [`List]) t
type ('a, 'b) array_step = ('a, 'b, [`Array]) t
type ('a, 'b) root = ('a, 'b, [`Root]) t

let steps_of_path x = x

let ( ^^ ) = ( @ )
let root = []

let type_path_get_list_nth i =
  [List_nth i]

let is_list_prefix = function
  | List_nth i :: rest -> Some (i, rest)
  | _ -> None

let type_path_get_array_nth i =
  [Array_nth i]

let field_name = function
  | [Field s] -> s
  | _ -> assert false

let constructor_name = function
  | [Constructor(n, _)] -> n
  | _ -> assert false

type boxed = Boxed | Unboxed

let extract_field_info ~t p find =
  match remove_first_props (stype_of_ttype t) with
  | DT_node{rec_descr = DT_record{record_fields; record_repr}; _} ->
    let unboxed = match record_repr with
      | Record_unboxed -> Unboxed | _ -> Boxed
    in
    begin
      let field = field_name p in
      match find (fun (s, _, _) -> s = field) record_fields with
      | x -> x , unboxed
      | exception Not_found -> assert false
    end
  | s -> Format.eprintf "%a@." print_stype s; assert false

let extract_field ~(t: 'a ttype) (p: ('a, 'b) field): 'a -> 'b =
  match extract_field_info ~t p Ext.List.findi with
  | nth, Boxed ->
    fun x -> Obj.obj (Obj.field (Obj.repr x) nth)
  | _, Unboxed ->
    fun x -> Obj.magic x

let set_field ~(t: 'a ttype) (p: ('a, 'b) field): 'a -> 'b -> 'a =
  match extract_field_info ~t p Ext.List.findi with
  | nth, Boxed ->
    fun x v ->
      let x = Obj.dup (Obj.repr x) in
      Obj.set_field x nth (Obj.repr v);
      Obj.obj x
  | _, Unboxed ->
    fun x -> Obj.magic x

let unsafe_ttype (t: stype): _ ttype =
  Obj.magic t

let extract_field_type ~t p: _ ttype =
  let (_ , _, (t: stype)) , _ =
    extract_field_info ~t p List.find
  in
  unsafe_ttype t

let is_empty: ('a, 'b, 'c) t -> ('a, 'b) TypEq.t option = fun path ->
  match steps_of_path path with
  | [] -> Some (Obj.magic TypEq.refl)
  | _ -> None

let non_constant_constructor_info ~t name =
  match remove_first_props (stype_of_ttype t) with
  | DT_node{rec_descr = DT_variant{variant_repr = _; variant_constrs}; _} ->
      let rec aux id = function
        | [] -> assert false
        | (_, _, C_tuple []) :: constructors -> aux id constructors
        | (constructor_name, props, parameters) :: _ when constructor_name = name -> id, parameters, props
        | _ :: constructors -> aux (id + 1) constructors
      in
      aux 0 variant_constrs
  | DT_option t -> assert(name = "Some"); 0, C_tuple [t], []
  | _ -> assert false

let extract_non_constant_constructor_type ~t name n =
  let _, types, _ = non_constant_constructor_info ~t name in
  match types, n with
  | C_tuple types, n -> `Regular (unsafe_ttype (List.nth types n))
  | C_inline typ, 1 -> `Inline (unsafe_ttype typ)
  | C_inline _, _ -> assert false

let constant_constructor_info ~t name =
  match remove_first_props (stype_of_ttype t) with
  | DT_node{rec_descr = DT_variant{variant_repr = _; variant_constrs}; _} ->
      let rec aux id = function
        | [] -> assert false
        | (constructor_name, props, C_tuple []) :: _ when constructor_name = name -> id, props
        | (_, _, C_tuple []) :: constructors -> aux (id + 1) constructors
        | _ :: constructors -> aux id constructors
      in
      aux 0 variant_constrs
  | DT_option _ -> assert(name = "None"); 0, []
  | _ -> assert false

let apply_constructor ~t = function
  | [ Constructor (name, 0) ] ->
      let tag, _ = constant_constructor_info ~t name in
      let x = Obj.magic tag in
      (fun _ -> x)
  | [ Constructor (name, 1) ] ->
      let tag, _, _ = non_constant_constructor_info ~t name in
      (fun x ->
        let o = Obj.new_block tag 1 in
        Obj.set_field o 0 (Obj.repr x);
        Obj.magic o
      )
  | [ Constructor (name, n) ] ->
      let tag, _, _ = non_constant_constructor_info ~t name in
      (fun x ->
        let x = Obj.repr x in
        let o = Obj.new_block tag n in
        for i = 0 to n - 1 do
          Obj.set_field o i (Obj.field x i);
        done;
        Obj.magic o
      )
  | _ -> assert false

let tuple_info ~t n =
  match remove_first_props (stype_of_ttype t) with
  | DT_tuple l -> List.nth l n
  | _ -> assert false

let extract_tuple_type ~t n =
  let (t: stype) = tuple_info ~t n  in
  unsafe_ttype t

let list_array_info ~t =
  match remove_first_props (stype_of_ttype t) with
  | DT_list t
  | DT_array t -> t
  | _ -> assert false

let dup_with_tag ~tag ~length x =
  let b = Obj.new_block tag length in
  for i = 0 to length - 1 do
    Obj.set_field b i (Obj.field x i)
  done;
  b

let rec patch (type t) ~(t : t ttype) path x f =
  match path with
  | [] -> f x
  | (Field _ as p) :: rest -> begin
      let t = extract_field_type ~t [p] in
      match extract_field_info ~t [p] Ext.List.findi with
      | nth, Boxed ->
        let v = patch ~t rest (Obj.field x nth) f in
        let x = Obj.dup (Obj.repr x) in
        Obj.set_field x nth (Obj.repr v);
        x
      | _, Unboxed -> patch ~t rest (Obj.magic x) f
    end
  | Tuple_nth n :: rest ->
      let t = extract_tuple_type ~t n in
      let v = patch ~t rest (Obj.field x n) f in
      let x = Obj.dup (Obj.repr x) in
      Obj.set_field x n (Obj.repr v);
      x
  | Constructor (name, 1) :: rest ->
      let t = extract_non_constant_constructor_type ~t name 0 in
      begin match t with
      | `Regular t ->
        let v = patch ~t rest (Obj.field x 0) f in
        let x = Obj.dup (Obj.repr x) in
        Obj.set_field x 0 (Obj.repr v);
        x
      | `Inline t ->
        let v = patch ~t rest (Obj.obj x) f in
        Obj.dup (Obj.repr v)
      end
  | Constructor (name, n) :: rest when n <> 0 ->
      let _, types, _ = non_constant_constructor_info ~t name in
      let t = unsafe_ttype (DT_tuple (uninline types)) in (* safe uninline since n <> 0 *)
      let tag = Obj.tag x in
      let tuple = dup_with_tag ~tag: 0 ~length: n x in
      let v = patch ~t rest tuple f in
      dup_with_tag ~tag ~length: n (Obj.repr v)
  | _x -> (* Mlfi_isdatypes.debug x; *) invalid_arg "Mlfi_type_path.patch"

let patch (type s) ~(t :s ttype) (path : (s, 'b, _) t) (x : s) (f : 'b -> 'b) : s =
  Obj.magic (patch ~t:(Obj.magic t) path (Obj.magic x) (Obj.magic f))

let extract_step (type t) ~(t: t ttype) step x =
  match step with
  | Field _ -> stype_of_ttype (extract_field_type ~t [step]), extract_field ~t [step] (Obj.obj x), []
  | Constructor(_, 0) -> stype_of_ttype unit_t, Obj.repr (), []
  | Constructor(name, _) ->
      let id, parameters, props = non_constant_constructor_info ~t name in
      if id <> Obj.tag x then failwith (Printf.sprintf "Extract step: bad constructor (not %S %i %i)" name id (Obj.tag x));
      begin
        match parameters with
        | C_tuple [t] -> t, Obj.field x 0, props
        | C_inline t -> t, x, props (* TODO : should we change the tag ? *)
        | C_tuple parameters ->
            let b = Obj.new_block 0 (List.length parameters) in
            List.iteri
              (fun i _ ->
                Obj.set_field b i (Obj.field x i)
              )
              parameters;
            DT_tuple parameters, b, props
      end
  | Tuple_nth n ->
      let t = tuple_info ~t n in
      t, Obj.field x n, []
  | List_nth n ->
      let rec aux n cons =
        if n = 0 then
          Obj.field cons 0
        else
          let next_obj = Obj.field cons 1 in
          if Obj.is_block next_obj then
            aux (n - 1) next_obj
          else
            failwith "Extract step: too short list"
      in
      let t = list_array_info ~t in
      t, aux n x, []
  | Array_nth n ->
      let length = Obj.size x in
      if length <= n then failwith "Extract step: too short array";
      let t = list_array_info ~t in
      t, Obj.field x n, []

let rec extract ~t path x =
  match path with
  | [] -> Obj.magic t, Obj.magic x
  | step :: path ->
      let t, x, _ = extract_step ~t step (Obj.repr x) in
      let t = unsafe_ttype t in
      let x = Obj.obj x in
      extract ~t path x

let extract_type_step (type t) ~(t: t ttype) = function
  | Field _ as step -> stype_of_ttype (extract_field_type ~t [step]), []
  | Constructor(name, 0) ->
      let _, props = constant_constructor_info ~t name in
      stype_of_ttype unit_t, props
  | Constructor(name, _) ->
      let _, parameters, props = non_constant_constructor_info ~t name in
      begin match uninline parameters with
      | [t] -> t, props
      | l -> DT_tuple l, props
      end
  | Tuple_nth n ->
      tuple_info ~t n, []
  | List_nth _ ->
      list_array_info ~t, []
  | Array_nth _ ->
      list_array_info ~t, []

let rec extract_type ~t props = function
  | [] -> unsafe_ttype (stype_of_ttype t), props
  | step :: path ->
      let t, props = extract_type_step ~t step in
      let t = unsafe_ttype t in
      extract_type ~t props path

let extract_type ~t path = extract_type ~t [] path
let constructor_info ~t path =
  match path with
  | [Constructor(name, _)] ->
      let t, props = extract_type ~t path in
      name, props, t
  | _ -> assert false

let extract_type ~t path = fst(extract_type ~t path)

let has_shape ~t path x =
  try
    ignore(extract ~t path x);
    true
  with
  | Failure _ -> false

let rec is_prefix prefix p =
  match prefix, p with
  | [], _ -> Some p
  | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> is_prefix tl1 tl2
  | _ -> None

type 'a rooted_path = TypePath: ('a, 'b, 'c) t -> 'a rooted_path

let prefix_rooted_path prefix (TypePath path) =
  TypePath (prefix ^^ path)

let rooted_root  = TypePath []

let composed x = x
