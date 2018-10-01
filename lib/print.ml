open Std

type 'a printer = Format.formatter -> 'a -> unit

module type PRINTABLE_0 = sig
  include Xtype.T0
  val printer: t printer
end

module type PRINTABLE_1 = sig
  include Xtype.T1
  val printer: 'a printer -> 'a t printer
end

module type PRINTABLE_2 = sig
  include Xtype.T2
  val printer: 'a printer -> 'b printer -> ('a, 'b) t printer
end

module type PMATCHER_0 = sig
  include PRINTABLE_0
  include Xtype.MATCH0 with type t := t
end

module type PMATCHER_1 = sig
  include PRINTABLE_1
  include Xtype.MATCH1 with type 'a t := 'a t
end

module type PMATCHER_2 = sig
  include PRINTABLE_2
  include Xtype.MATCH2 with type ('a,'b) t := ('a,'b) t
end

type printable =
  | Zero of (module PMATCHER_0)
  | One  of (module PMATCHER_1)
  | Two  of (module PMATCHER_2)

let abstract_printers : (string, printable) Hashtbl.t = Hashtbl.create 17

let add_abstract_0 (module P : PRINTABLE_0) =
  let module T = struct
    include Xtype.Match0(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (Zero (module T))
  | _ -> raise (Invalid_argument "add_abstract: received non abstract type")

let add_abstract_1 (module P : PRINTABLE_1) =
  let module T = struct
    include Xtype.Match1(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (One (module T))
  | _ -> raise (Invalid_argument "add_abstract: received non abstract type")

let add_abstract_2 (module P : PRINTABLE_2) =
  let module T = struct
    include Xtype.Match2(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (Two (module T))
  | _ -> raise (Invalid_argument "add_abstract: received non abstract type")

module type UNSAFE_ABSTRACT_PRINTABLE_1 = sig
  type 'a t
  val name: string
  val printer: 'a printer -> 'a t printer
end

module type UNSAFE_ABSTRACT_PRINTABLE_2 = sig
  type ('a, 'b) t
  val name: string
  val printer: 'a printer -> 'b printer-> ('a, 'b) t printer
end

let add_unsafe_abstract_0 ~name
    (printer: Format.formatter -> 'a printer ) =
  add_abstract_0 ( module struct
    type t = Obj.t
    let t : t Ttype.t = Obj.magic (Stype.DT_abstract (name, []))
    let printer = Obj.magic printer
  end )

let add_unsafe_abstract_1
    (module P : UNSAFE_ABSTRACT_PRINTABLE_1) =
  add_abstract_1 ( module struct
    type 'a t = Obj.t
    let t (a : 'a Ttype.t) : 'a t Ttype.t =
      Obj.magic (Stype.DT_abstract (P.name, [Ttype.to_stype a]))
    let printer = Obj.magic P.printer
  end )

let add_unsafe_abstract_2
    (module P : UNSAFE_ABSTRACT_PRINTABLE_2) =
  add_abstract_2 ( module struct
    type ('a, 'b) t = Obj.t
    let t (a : 'a Ttype.t) (b : 'b Ttype.t)  : ('a, 'b) t Ttype.t =
      Obj.magic (Stype.DT_abstract (P.name,
                                    [Ttype.to_stype a; Ttype.to_stype b]))
    let printer = Obj.magic P.printer
  end )

let pp_may_left_paren ppf parens =
  Format.pp_open_box ppf 1;
  if parens then Format.pp_print_char ppf '('

let pp_may_right_paren ppf parens =
  if parens then Format.pp_print_char ppf ')';
  Format.pp_close_box ppf ()

let pp_par_when_neg ppf parens abs print x =
  if abs x <> x then begin
    pp_may_left_paren ppf parens; print ppf x; pp_may_right_paren ppf parens
  end else
    print ppf x

let pp_print_int32 ppf x = Format.pp_print_string ppf (Int32.to_string x)
let pp_print_int64 ppf x = Format.pp_print_string ppf (Int64.to_string x)
let pp_print_nativeint ppf x =
  Format.pp_print_string ppf (Nativeint.to_string x)

let print_dynamic fmt (t, x) =
  let open Format in
  let open Xtype in
  let rec print_list : type a. pre:_ -> pst:_ -> sep:_ ->
    (a -> unit) -> a list -> unit =
    fun ~pre ~pst ~sep print_el lst ->
      let rec f = function
        | [] -> ()
        | [e] -> print_el e
        | e :: rest ->
          print_el e;
          pp_print_string fmt sep;
          pp_print_space fmt ();
          f rest
      in
      pp_print_string fmt pre; f lst; pp_print_string fmt pst;
  and print_dynamic : type t. t Ttype.t -> bool -> t -> unit =
    fun t parens x ->
      match xtype_of_ttype t with
      | Unit -> pp_print_string fmt "()"
      | Bool -> pp_print_bool fmt x
      | Char -> pp_print_char fmt x
      | Int -> pp_par_when_neg fmt parens abs pp_print_int x
      | Int32 -> pp_par_when_neg fmt parens Int32.abs pp_print_int32 x
      | Int64 -> pp_par_when_neg fmt parens Int64.abs pp_print_int64 x
      | Float -> pp_par_when_neg fmt parens abs_float Ext.Float.pp_repres x
      | Nativeint ->
        pp_par_when_neg fmt parens Nativeint.abs pp_print_nativeint x
      | String ->
        pp_print_char fmt '\"';
        pp_print_string fmt (String.escaped x);
        pp_print_char fmt '\"'
      | Option {t;_} -> begin
          match x with
          | None -> pp_print_string fmt "None"
          | Some next_x ->
            pp_may_left_paren fmt parens;
            pp_print_string fmt "Some";
            pp_print_space fmt ();
            print_dynamic t true next_x ;
            pp_may_right_paren fmt parens
        end
      | List _ when x = [] -> pp_print_string fmt "[]"
      | List {t;_} ->
        pp_open_box fmt 2;
        print_list ~pre:"[" ~sep:";" ~pst:"]" (print_dynamic t false) x;
        pp_close_box fmt ()
      | Array {t;_} ->
        pp_open_box fmt 2; pp_print_string fmt "[|";
        for i = 0 to Array.length x - 1 do
          if i > 0 then begin pp_print_char fmt ';'; pp_print_space fmt () end;
          print_dynamic t false x.(i)
        done;
        pp_print_string fmt "|]"; pp_close_box fmt ()
      | Tuple tup ->
        let print_el (Field e) =
          print_dynamic e.typ.t false (Fields.tuple tup e x)
        in
        pp_open_box fmt 1;
        print_list ~pre:"(" ~sep:"," ~pst:")" print_el tup.t_flds;
        pp_close_box fmt ()
      | Record r->
        let print_el ((name,_), Field e) =
          pp_open_box fmt 1;
          pp_print_string fmt name; pp_print_string fmt " =";
          pp_print_space fmt ();
          print_dynamic e.typ.t false (Fields.record r e x);
          pp_close_box fmt ();
        in
        pp_open_hvbox fmt 1;
        print_list ~pre:"{" ~sep:";" ~pst:"}" print_el r.r_flds;
        pp_close_box fmt ()
      | Sum s ->
        pp_may_left_paren fmt parens;
        ( match constructor_by_value s x with
          | Constant c -> pp_print_string fmt (fst c.cc_label);
          | Regular ({rc_flds=[Field e];_} as c)->
            pp_open_box fmt 1;
            pp_print_string fmt (fst c.rc_label);
            pp_print_space fmt () ;
            print_dynamic e.typ.t true (
              Fields.regular_constructor c e x |> Ext.Option.value_exn );
            pp_close_box fmt ()
          | Regular c ->
            let print_el (Field e) =
              pp_open_box fmt 1;
              print_dynamic e.typ.t false (
                Fields.regular_constructor c e x |> Ext.Option.value_exn );
              pp_close_box fmt ();
            in
            pp_open_box fmt 1;
            pp_print_string fmt (fst c.rc_label);
            pp_print_space fmt () ;
            print_list ~pre:"(" ~sep:"," ~pst:")" print_el c.rc_flds;
            pp_close_box fmt ()
          | Inlined c ->
            let print_el ((name,_), Field e) =
              pp_open_box fmt 1;
              pp_print_string fmt name; pp_print_string fmt " =";
              pp_print_space fmt ();
              print_dynamic e.typ.t false (
                Fields.inlined_constructor c e x |> Ext.Option.value_exn );
              pp_close_box fmt ();
            in
            pp_open_hvbox fmt 1;
            pp_print_string fmt (fst c.ic_label) ;
            pp_print_space fmt () ;
            print_list ~pre:"{" ~sep:";" ~pst:"}" print_el c.ic_flds;
            pp_close_box fmt ()
        ) ;
        pp_may_right_paren fmt parens
      | Lazy {t;_} ->
        pp_may_left_paren fmt parens;
        pp_print_string fmt "lazy ";
        begin match (Lazy.force x) with
          | exception exn ->
            pp_print_string fmt "(raise ";
            pp_print_string fmt (Printexc.to_string exn);
            pp_print_char fmt ')'
          | x -> print_dynamic t true x
        end;
        pp_may_right_paren fmt parens
      | Function _ -> pp_print_string fmt "<fun>"
      | Object _ -> pp_print_string fmt "<object>"
      | Prop (_, {t;_}) -> print_dynamic t parens x
      | Abstract (name, _) ->
        let rec use_first = function
          | [] ->
            pp_print_string fmt "<abstract: ";
            pp_print_string fmt name;
            pp_print_string fmt ">";
          | hd :: tl -> begin
              match hd with
              | Zero (module T) -> begin
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is TypEq.Eq) ->
                    T.printer fmt x
                end
              | One (module T) -> begin
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t, TypEq.Eq)) ->
                    let pr _fmt x = print_dynamic t false x in
                    T.printer pr fmt x
                end
              | Two (module T) -> begin
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t1, t2, TypEq.Eq)) ->
                    let pr1 _fmt x = print_dynamic t1 false x in
                    let pr2 _fmt x = print_dynamic t2 false x in
                    T.printer pr1 pr2 fmt x
                end
            end
        in
        pp_open_box fmt 0;
        use_first (Hashtbl.find_all abstract_printers name) ;
        pp_close_box fmt ()
  in
  print_dynamic t false x

let print ~t ppf x = print_dynamic ppf (t, x)
let show ~t x = Format.printf "%a\n%!" print_dynamic (t, x)

module Hashtbl_printer = struct
  open Format

  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  let t (type a) (type b) (a: a Ttype.t) (b: b Ttype.t) = hashtbl_t a b

  let printer (print1 : 'a printer) (print2 : 'b printer) ppf
      (h : ('a, 'b) Hashtbl.t) =
    let first = ref true in
    let print_el key value =
      if !first then first := false
      else (pp_print_char ppf ';'; pp_print_space ppf ());
      pp_open_box ppf 1; pp_print_char ppf '(';
      print1 ppf key;
      pp_print_char ppf ','; pp_print_space ppf ();
      print2 ppf value;
      pp_print_char ppf ')'; pp_close_box ppf ()
    in
    if Hashtbl.length h = 0 then
      pp_print_string ppf "[]"
    else begin
      pp_open_box ppf 1; pp_print_char ppf '[';
      Hashtbl.iter print_el h ;
      pp_print_char ppf ']'; pp_close_box ppf ()
    end
end
let () = add_abstract_2 (module Hashtbl_printer)

let () =
  add_abstract_0 (module struct
    type t = unit
    let t = unit_t
    let printer ppf () =  Format.pp_print_string ppf "()"
  end) ;
  add_abstract_1 (module struct
    type 'a t = 'a Ttype.t
    let t (type a) (a: a Ttype.t) = Ttype.t a
    let printer _pp_a ppf t = Stype.print ppf (Ttype.to_stype t)
  end)

module Test = struct
  [@@@warning "-37"]

  let show t x = show ~t x

  let ht = Hashtbl.create 5
  let () =
    Hashtbl.add ht "a" 5 ;
    Hashtbl.add ht "b" 7 ;
    Hashtbl.add ht "c" 13

  type sum =
    | Tpl of int * int
    | Atm of int
  [@@deriving t]

  let%expect_test _ =
    show [%t: unit] ();
    show [%t: int] (-2);
    show [%t: int] 40;
    show int64_t (Int64.of_int 41);
    show string_t "a string";
    show [%t: int -> int] (fun x -> x + 1);
    show [%t: int list] [1; 2; 3];
    show [%t: string array] [|"a"; "b"; "c"|];
    show (hashtbl_t string_t int_t) ht;
    show [%t: sum * sum] (Tpl (0,0), Atm 0);
    show int32_t (Int32.of_int 42);
    show nativeint_t (Nativeint.of_int 43);
    [%expect {|
    ()
    -2
    40
    41
    "a string"
    <fun>
    [1; 2; 3]
    [|"a"; "b"; "c"|]
    [("a", 5); ("b", 7); ("c", 13)]
    (Tpl (0, 0), Atm 0)
    42
    43 |}]

  type tt =
    | Inl of { x: int; y: bool; z: string}
    | Empty
    | Tupl of int * bool * string
  [@@deriving t]

  let%expect_test _ =
    print_endline "ttype:";
    show (Ttype.t tt_t) tt_t;
    [%expect {|
      ttype:
      (tt =
         | Inl of
          (tt.Inl =
             {
               x: int;
               y: bool;
               z: string;
             })
         | Empty
         | Tupl of (int * bool * string)) |}]
end
