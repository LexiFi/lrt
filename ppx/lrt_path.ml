(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** The [\[%path? .\]] syntax extension. See {!Lrt.Path}.*)

(**/**)

open Ppxlib
open Ast_builder.Default

let raise_errorf ~loc =
  Format.ksprintf (Location.raise_errorf ~loc "%s %s" "[%path .]")

class tyvar =
  object
    val mutable i = 0

    method pair loc =
      i <- i + 1 ;
      ( Printf.sprintf "t%d" (i - 1) |> ptyp_var ~loc
      , Printf.sprintf "t%d" i |> ptyp_var ~loc )
  end

type step =
  | Constructor of label loc * int * int
  | ConstructorInline of label loc * label loc
  | Field of label loc
  | Tuple of int * int
  | List of int
  | Array of int

type lst = Cons of pattern * pattern | Nil

module Pat = struct
  open Ast_pattern

  exception Invalid_tuple of string loc

  let nth_and_arity ~loc lst =
    match
      List.fold_left
        (fun (i, nth) e ->
          match (e, nth) with
          | true, None -> (i + 1, Some i)
          | true, _ -> raise (Invalid_tuple {txt= "Too much []"; loc})
          | false, x -> (i + 1, x) )
        (0, None) lst
    with
    | arity, Some nth -> (nth, arity)
    | _ -> raise (Invalid_tuple {txt= "No []"; loc})

  let tuple =
    ppat_tuple
      (many
         ( ppat_any |> map0 ~f:false
         ||| (ppat_construct (lident (string "[]")) none |> map0 ~f:true) ))
    |> map1' ~f:(fun loc -> nth_and_arity ~loc)

  let step =
    ppat_var __'
    |> map1 ~f:(fun x -> Field x)
    ||| (ppat_array (pint __ ^:: nil) |> map1 ~f:(fun x -> Array x))
    ||| ( ppat_construct
            (lident (string "::"))
            (some
               (ppat_tuple
                  ( pint __
                  ^:: ppat_construct (lident (string "[]")) none
                  ^:: nil )))
        |> map1 ~f:(fun x -> List x) )
    ||| ( ppat_construct (lident __') (some tuple)
        |> map2 ~f:(fun s (nth, arity) -> Constructor (s, nth, arity)) )
    ||| ( ppat_construct (lident __') (some (ppat_var __'))
        |> map2 ~f:(fun name field -> ConstructorInline (name, field)) )
    ||| ( ppat_construct (lident __') none
        |> map1 ~f:(fun s -> Constructor (s, 0, 1)) )
    ||| (tuple |> map1 ~f:(fun (nth, arity) -> Tuple (nth, arity)))

  let lst =
    ppat_construct
      (lident (string "::"))
      (some (ppat_tuple (__ ^:: __ ^:: nil)))
    |> map2 ~f:(fun a b -> Cons (a, b))
    ||| (ppat_construct (lident (string "[]")) none |> map0 ~f:Nil)
end

let lid_loc_of_label_loc = Loc.map ~f:(fun x -> Lident x)

let tuple_pat ~loc nth arity =
  let arr = Array.init arity (fun i -> pvar ~loc ("_v" ^ string_of_int i)) in
  let () = arr.(nth) <- pvar ~loc "x" in
  ppat_tuple ~loc (Array.to_list arr)

let tuple_patch ~loc nth arity =
  let arr = Array.init arity (fun i -> evar ~loc ("_v" ^ string_of_int i)) in
  let () = arr.(nth) <- evar ~loc "y" in
  pexp_tuple ~loc (Array.to_list arr)

let rec expand_step ~loc x =
  match Ast_pattern.parse Pat.step loc x (fun x -> x) with
  | Constructor (label, nth, arity) ->
      let p = Some (tuple_pat ~loc nth arity) in
      let lloc = lid_loc_of_label_loc label in
      let c = ppat_construct ~loc lloc p in
      let patched =
        pexp_construct ~loc lloc (Some (tuple_patch ~loc nth arity))
      in
      [%expr
        let get x =
          match[@ocaml.warning "-11"] x with [%p c] -> Some x | _ -> None
        and set x y =
          match[@ocaml.warning "-11"] x with
          | [%p c] -> Some [%e patched]
          | _ -> None
        in
        ( {get; set}
        , constructor_regular
            ~name:[%e estring ~loc:label.loc label.txt]
            ~nth:[%e eint ~loc nth] ~arity:[%e eint ~loc arity] )]
  | ConstructorInline (name, field) ->
      let nlloc = lid_loc_of_label_loc name in
      let flloc = lid_loc_of_label_loc field in
      let c = ppat_construct ~loc nlloc (Some (pvar ~loc "x")) in
      let get = pexp_field ~loc (evar ~loc "x") flloc
      and set =
        pexp_construct ~loc nlloc
          (Some
             (pexp_record ~loc [(flloc, evar ~loc "y")] (Some (evar ~loc "x"))))
      in
      [%expr
        let get x =
          match[@ocaml.warning "-11"] x with
          | [%p c] -> Some [%e get]
          | _ -> None
        and set x y =
          match[@ocaml.warning "-11"] [@ocaml.warning "-23"] x with
          | [%p c] -> Some [%e set]
          | _ -> None
        in
        ( {get; set}
        , constructor_inline
            ~name:[%e estring ~loc:name.loc name.txt]
            ~field_name:[%e estring ~loc:field.loc field.txt] )]
  | Field label ->
      let liloc = lid_loc_of_label_loc label in
      let get = pexp_field ~loc (evar ~loc "x") liloc
      and set =
        pexp_record ~loc [(liloc, evar ~loc "y")] (Some (evar ~loc "x"))
      in
      [%expr
        let get x = Some [%e get]
        and set x y = (Some [%e set] [@ocaml.warning "-23"]) in
        ({get; set}, field ~field_name:[%e estring ~loc:label.loc label.txt])]
  | Tuple (nth, arity) ->
      let p = tuple_pat ~loc nth arity in
      let patched = tuple_patch ~loc nth arity in
      [%expr
        let get [%p p] = Some x and set [%p p] y = Some [%e patched] in
        ({get; set}, tuple ~nth:[%e eint ~loc nth] ~arity:[%e eint ~loc arity])]
  | List nth ->
      let () = if nth < 0 then raise_errorf ~loc "Invalid list index" in
      [%expr
        let get l = List.nth_opt l [%e eint ~loc nth]
        and set l y = set_nth_opt l [%e eint ~loc nth] y in
        ({get; set}, list ~nth:[%e eint ~loc nth])]
  | Array nth ->
      let () = if nth < 0 then raise_errorf ~loc "Invalid array index" in
      [%expr
        let get a =
          if [%e eint ~loc nth] < Array.length a then
            Some a.([%e eint ~loc nth])
          else None
        and set a y =
          if [%e eint ~loc nth] < Array.length a then (
            let a' = Array.copy a in
            a'.([%e eint ~loc nth]) <- y ;
            Some a' )
          else None
        in
        ({get; set}, array ~nth:[%e eint ~loc nth])]
  | exception Pat.Invalid_tuple {txt; loc} -> raise_errorf ~loc "%s" txt

and expand acc ({ppat_loc= loc; _} as x) =
  match Ast_pattern.parse Pat.lst loc x (fun x -> x) with
  | Cons (hd, tl) -> expand (expand_step ~loc hd :: acc) tl
  | Nil -> acc

let expand ~loc ~path x =
  ignore path ;
  [%expr
    let open! Lrt_ppx_runtime.Path in
    [%e elist ~loc (expand [] x |> List.rev)]]

(* Register the expander *)
let () =
  let extensions =
    [ Extension.(
        declare "path" Context.expression Ast_pattern.(ppat __ none) expand) ]
  in
  Driver.register_transformation "lrt_path" ~extensions
