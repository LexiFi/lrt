let print_list ppf ~opn ~cls ~sep print_el l =
  let rec f = function
    | [] -> ()
    | hd :: [] -> Format.fprintf ppf "%a" print_el hd
    | hd :: tl ->
      Format.fprintf ppf "%a" print_el hd ;
      Format.fprintf ppf "%s" sep;
      f tl
  in
  Format.fprintf ppf "%s" opn;
  f l;
  Format.fprintf ppf "%s" cls


type (_,_) t =
  | (::) : ('a,'b) step * ('b,'c) t -> ('a,'c) t
  | [] : ('a, 'a) t
  | Composed : ('a,'b) t * ('b,'c) t -> ('a, 'c) t

and ('a,'b) step = ('a,'b) lens * meta

and ('a,'b) lens =
  { get : 'a -> 'b option
  ; set : 'a -> 'b -> 'a option
  }

and meta = (* private *)
  | Field of {name: string}
  | Constructor of {name: string; arg: constructor_argument}
  | Tuple of {nth: int; arity: int}
  | List of {nth: int}
  | Array of {nth: int}

and constructor_argument =
  | Regular of {nth: int; arity: int}
  | Inline of {field: string}

let rec print_step ppf = function
  | Field {name} -> Format.fprintf ppf "%s" name
  | Constructor {name; arg = Regular {nth;arity}} ->
    Format.fprintf ppf "%s %a" name print_step (Tuple {nth;arity})
  | Constructor {name; arg = Inline {field}} ->
    Format.fprintf ppf "%s %a" name print_step (Field {name=field})
  | Tuple {nth; arity}->
    let a = Array.make arity "_" in
    Array.set a nth "[]";
    if arity > 1 then Format.fprintf ppf "(" ;
    print_list ppf ~opn:"" ~cls:"" ~sep:","
      (fun ppf s -> Format.fprintf ppf "%s" s) (Array.to_list a);
    if arity > 1 then Format.fprintf ppf ")";
  | List {nth} -> Format.fprintf ppf "[%i]" nth
  | Array {nth} -> Format.fprintf ppf "[|%i|]" nth

let meta_list t =
  let rec fold : type a b.
    meta list -> (a,b) t -> meta list =
    fun acc -> function
      | [] -> List.rev acc
      | (_, hd) :: tl -> fold (hd :: acc) tl
      | Composed ([], tl) -> fold acc tl
      | Composed ((_, hd) :: tl, r) -> fold (hd :: acc) (Composed (tl, r))
      | Composed (Composed(a,b), c) -> fold acc (Composed (a, Composed(b,c)))
  in
  fold [] t

let print ppf t =
  print_list ppf ~opn:"[%path? [" ~cls:"]]" ~sep:"; "
    print_step (meta_list t)

let (>>=) x f =
  match x with
  | None -> None
  | Some x -> f x

let root_lens : ('a,'a) lens =
  let set _a b = Some b
  and get a = Some a
  in { set; get }

let lens (t : ('a,'b) t) : ('a,'b) lens =
  let focus acc hd =
    let get a = acc.get a >>= hd.get
    and set a c = acc.get a >>= fun b -> hd.set b c >>= acc.set a
    in { get ; set }
  in
  let rec fold : type a b c.
    (a,b) lens -> (b,c) t -> (a,c) lens =
    fun acc -> function
      | [] -> acc
      | (hd, _) :: tl -> fold (focus acc hd) tl
      | Composed ([],tl) -> fold acc tl
      | Composed ((hd, _) :: tl, r) -> fold (focus acc hd) (Composed (tl,r))
      | Composed (Composed (a,b), c) -> fold acc (Composed (a, Composed(b,c)))
  in fold root_lens t

let (@) p1 p2 = Composed (p1,p2)

module Unsafe = struct
  let is_prefix : type a b c. (a,b) t -> (a,c) t -> (b,c) t option =
    fun prefix t ->
      let pmeta, tmeta = meta_list prefix, meta_list t in
      let rec check (l: meta list) (r: meta list) (p: (_,_) t) =
        match l, r, p with
        | [], _, p -> Some p
        | hl :: tl, hr :: tr, _ :: tp when hl = hr -> check tl tr (Obj.magic tp)
        | _, _ :: _, [] | _, [], _ :: _ -> assert false
        | _ -> None
      in
      (* TODO: We are comparing the path based on the untyped meta information.
         Can we do better? *)
      Obj.magic (check pmeta tmeta (Obj.magic t))

  let is_equal a b =
    match is_prefix a b with
    | Some [] -> true
    | _ -> false
end

module Internal = struct
  let list ~nth = List {nth}
  let array ~nth = Array {nth}
  let field ~name = Field {name}
  let tuple ~nth ~arity = Tuple {nth; arity}
  let constructor_regular ~name ~nth ~arity =
    Constructor {name; arg = Regular {nth; arity}}
  let constructor_inline ~name ~field =
    Constructor {name; arg = Inline {field}}
end
