(******************************************************************************)
(*  Copyright (C) 2020 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

let print_list ppf ~opn ~cls ~sep print_el l =
  let rec f = function
    | [] -> ()
    | [ hd ] -> Format.fprintf ppf "%a" print_el hd
    | hd :: tl ->
        Format.fprintf ppf "%a" print_el hd;
        Format.fprintf ppf "%s" sep;
        f tl
  in
  Format.fprintf ppf "%s" opn;
  f l;
  Format.fprintf ppf "%s" cls

type (_, _) t =
  | ( :: ) : ('a, 'b) step * ('b, 'c) t -> ('a, 'c) t
  | [] : ('a, 'a) t

and ('a, 'b) step = ('a, 'b) lens * meta

and ('a, 'b) lens = { get: 'a -> 'b option; set: 'a -> 'b -> 'a option }

and meta =
  (* private *)
  | Field of { field_name: string }
  | Constructor of { name: string; arg: argument }
  | Tuple of { nth: int; arity: int }
  | List of { nth: int }
  | Array of { nth: int }

and argument =
  | Regular of { nth: int; arity: int }
  | Inline of { field_name: string }

let rec print_step ppf = function
  | Field { field_name } -> Format.fprintf ppf "%s" field_name
  | Constructor { name; arg = Regular { nth; arity } } ->
      Format.fprintf ppf "%s %a" name print_step (Tuple { nth; arity })
  | Constructor { name; arg = Inline { field_name } } ->
      Format.fprintf ppf "%s %a" name print_step (Field { field_name })
  | Tuple { nth; arity } ->
      let a = Array.make arity "_" in
      a.(nth) <- "[]";
      if arity > 1 then Format.fprintf ppf "(";
      print_list ppf ~opn:"" ~cls:"" ~sep:","
        (fun ppf s -> Format.fprintf ppf "%s" s)
        (Array.to_list a);
      if arity > 1 then Format.fprintf ppf ")"
  | List { nth } -> Format.fprintf ppf "[%i]" nth
  | Array { nth } -> Format.fprintf ppf "[|%i|]" nth

let meta t =
  let rec fold : type a b. meta list -> (a, b) t -> meta list =
   fun acc -> function
    | [] -> List.rev acc
    | (_, hd) :: tl -> fold (hd :: acc) tl
  in
  fold [] t

let print ppf t =
  print_list ppf ~opn:"[%path? [" ~cls:"]]" ~sep:"; " print_step (meta t)

let ( >>= ) x f = match x with None -> None | Some x -> f x

let root_lens : ('a, 'a) lens =
  let set _a b = Some b and get a = Some a in
  { set; get }

let lens (t : ('a, 'b) t) : ('a, 'b) lens =
  let focus acc hd =
    let get a = acc.get a >>= hd.get
    and set a c = acc.get a >>= fun b -> hd.set b c >>= acc.set a in
    { get; set }
  in
  let rec fold : type a b c. (a, b) lens -> (b, c) t -> (a, c) lens =
   fun acc -> function [] -> acc | (hd, _) :: tl -> fold (focus acc hd) tl
  in
  fold root_lens t

let rec ( @ ) : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
 fun p1 p2 -> match p1 with hd :: tl -> hd :: (tl @ p2) | [] -> p2

module Unsafe = struct
  let is_prefix : type a b c. (a, b) t -> (a, c) t -> (b, c) t option =
   fun prefix t ->
    let pmeta, tmeta = (meta prefix, meta t) in
    let rec check (l : meta list) (r : meta list) (p : (_, _) t) =
      match (l, r, p) with
      | [], _, p -> Some p
      | hl :: tl, hr :: tr, _ :: tp when hl = hr -> check tl tr (Obj.magic tp)
      | _, _ :: _, [] | _, [], _ :: _ -> assert false
      | _ -> None
    in
    (* TODO: We are comparing the path based on the untyped meta information.
         Can we do better? *)
    Obj.magic (check pmeta tmeta (Obj.magic t))

  let is_equal a b = match is_prefix a b with Some [] -> true | _ -> false
end

module Internal = struct
  let list ~nth = List { nth }

  let array ~nth = Array { nth }

  let field ~field_name = Field { field_name }

  let tuple ~nth ~arity = Tuple { nth; arity }

  let constructor_regular ~name ~nth ~arity =
    Constructor { name; arg = Regular { nth; arity } }

  let constructor_inline ~name ~field_name =
    Constructor { name; arg = Inline { field_name } }
end
