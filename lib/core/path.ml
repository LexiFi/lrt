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
  in
  fold [] t

let print ppf t =
  print_list ppf ~opn:"[%path? [" ~cls:"]]" ~sep:"; "
    print_step (meta_list t)

let lens (t : ('a,'b) t) : ('a,'b) lens =
  let root : ('a,'a) lens =
    let set _a b = Some b
    and get a = Some a
    in { set; get }
  in
  let rec fold : type a b c.
    (a,b) lens -> (b,c) t -> (a,c) lens =
    fun acc -> function
      | [] -> acc
      | (hd, _) :: tl ->
        let get a =
          match acc.get a with
          | None -> None
          | Some x -> hd.get x
        in
        let set a c =
          match acc.get a with
          | None -> None
          | Some b ->
            match hd.set b c with
            | None -> None
            | Some b -> acc.set a b
        in
        fold {get; set} tl
  in fold root t

module Unsafe = struct
  let list ~nth = List {nth}
  let array ~nth = Array {nth}
  let field ~name = Field {name}
  let tuple ~nth ~arity = Tuple {nth; arity}
  let constructor_regular ~name ~nth ~arity =
    Constructor {name; arg = Regular {nth; arity}}
  let constructor_inline ~name ~field =
    Constructor {name; arg = Inline {field}}
end
