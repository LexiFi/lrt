(** The [\[%path . \]] syntax extension. *)

(**/**)

open Ppxlib
open Ast_builder.Default

let raise_errorf ~loc =
  Format.ksprintf (Location.raise_errorf ~loc "%s %s" "[%path .]")

class tyvar = object
  val mutable i = 0

  method pair loc =
    i <- i + 1 ;
    ( Printf.sprintf "t%d" (i-1) |> ptyp_var ~loc
    , Printf.sprintf "t%d" i |> ptyp_var ~loc)
end

type step =
  | Constructor of label loc * int * int
  | Field of label loc
  | Tuple of int * int
  | List of int
  | Array of int

type lst =
  | Cons of pattern * pattern
  | Nil

module Pat = struct
  open Ast_pattern

  exception Invalid_tuple of string loc

  let nth_and_arity ~loc lst =
    match List.fold_left (fun (i, nth) e -> match e, nth with
        | true, None -> (i+1, Some i)
        | true, _ -> raise (Invalid_tuple {txt="Too much []";loc})
        | false, x -> (i+1, x)) (0, None) lst with
    | arity, Some nth -> nth, arity
    | _ -> raise (Invalid_tuple {txt="No []";loc})

  let tuple =
    ppat_tuple (many (
        (ppat_any |> map0 ~f:false) |||
        (ppat_construct (lident (string "[]")) none |> map0 ~f:true)))
    |> map1' ~f:(fun loc -> nth_and_arity ~loc)

  let step =
    (ppat_var __' |> map1 ~f:(fun x -> Field x))
    |||
    (ppat_array (pint __ ^:: nil) |> map1 ~f:(fun x -> Array x))
    |||
    (ppat_construct (lident (string "::"))
       (some (ppat_tuple (pint __ ^::
                          ppat_construct (lident (string "[]")) none
                          ^:: nil)))
     |> map1 ~f:(fun x -> List x))
    |||
    (ppat_construct (lident __')
       (some tuple) |> map2 ~f:(fun s (nth,arity) ->
        Constructor (s, nth, arity)))
    |||
    (ppat_construct (lident __')
       none |> map1 ~f:(fun s -> Constructor (s, 0, 1)))
    |||
    (tuple |> map1 ~f:(fun (nth,arity) -> Tuple (nth,arity)))

  let lst =
    (ppat_construct (lident (string "::"))
       (some (ppat_tuple (__ ^:: __ ^:: nil)))
     |> map2 ~f:(fun a b -> Cons (a,b)))
    |||
    (ppat_construct (lident (string "[]")) none |> map0 ~f:Nil)

end

let lid_loc_of_label_loc = Loc.map ~f:(fun x -> Lident x)

let tuple_pat ~loc nth arity =
    let arr = Array.make arity (ppat_any ~loc) in
    let () = Array.set arr nth (ppat_var ~loc {txt="x";loc}) in
    ppat_tuple ~loc (Array.to_list arr)

let rec expand_step ~loc v x =
  match Ast_pattern.parse Pat.step loc x (fun x -> x) with
  | Constructor (label, nth, arity) ->
    let p = Some (tuple_pat ~loc nth arity) in
    let c = ppat_construct ~loc (lid_loc_of_label_loc label) p in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = begin [@ocaml.warning "-11"]
        function [%p  c] -> x | _ -> assert false
      end in
      (Constructor ([%e estring ~loc label.txt], [%e eint ~loc nth])
       : ([%t f], [%t t]) step)
    ]
  | Field label ->
    let get = pexp_field ~loc (evar ~loc "x") (lid_loc_of_label_loc label) in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = fun x -> [%e get] in
      (Field [%e estring ~loc label.txt]
       : ([%t f], [%t t]) step)]
  | Tuple (nth, arity) ->
    let f, t = v#pair loc in
    let p = tuple_pat ~loc nth arity in
    [%expr
      let _ : [%t f] -> [%t t] = function [%p p] -> x in
      (Tuple_nth [%e eint ~loc nth]
       : ([%t f], [%t t]) step)]
  (* List *)
  | List nth ->
    let () = if nth < 0 then
        raise_errorf ~loc "Invalid list index" in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = fun l -> List.nth l [%e eint ~loc nth] in
      (List_nth [%e eint ~loc nth]
       : ([%t f], [%t t]) step)]
  (* array *)
  | Array nth ->
    let () = if nth < 0 then
        raise_errorf ~loc "Invalid array index" in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = fun a -> Array.get a [%e eint ~loc nth] in
      (Array_nth [%e eint ~loc nth]
       : ([%t f], [%t t]) step)]
  | exception Pat.Invalid_tuple {txt; loc} -> raise_errorf ~loc "%s" txt

and expand v acc ({ppat_loc = loc;_} as x) =
  match Ast_pattern.parse Pat.lst loc x (fun x -> x) with
  | Cons (hd, tl) -> expand v (expand_step ~loc v hd :: acc) tl
  | Nil -> acc

let expand ~loc ~path x =
  ignore path;
  let v = new tyvar in
  [%expr let open P in [%e elist ~loc (expand v [] x |> List.rev)]]

(* Register the expander *)
let () =
  let extensions =
    [ Extension.(declare "path" Context.expression Ast_pattern.(ppat __ none) expand) ] in
  Driver.register_transformation "dynt_path" ~extensions
