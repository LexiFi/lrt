open Dynt_core.Stype
open Dynt_core.Ttype
open Dynt_core.Std

type 'a printer = 'a -> unit

module type ABSTRACT_PRINTABLE_0 = sig
  include Xtype.TYPE_0
  val printer: Format.formatter -> t printer
end

module type ABSTRACT_PRINTABLE_1 = sig
  include Xtype.TYPE_1
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type ABSTRACT_PRINTABLE_2 = sig
  include Xtype.TYPE_2
  val printer: Format.formatter -> 'a printer -> 'b printer -> ('a, 'b) t printer
end

module type PRINTABLE_MATCHER_0 = sig
  include Xtype.MATCHER_0
  val printer: Format.formatter -> t printer
end

module type PRINTABLE_MATCHER_1 = sig
  include Xtype.MATCHER_1
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type PRINTABLE_MATCHER_2 = sig
  include Xtype.MATCHER_2
  val printer: Format.formatter -> 'a printer -> 'b printer -> ('a, 'b) t printer
end

type printable =
  | Zero of (module PRINTABLE_MATCHER_0)
  | One  of (module PRINTABLE_MATCHER_1)
  | Two  of (module PRINTABLE_MATCHER_2)

let abstract_printers : (string, printable) Hashtbl.t = Hashtbl.create 17

let add_abstract_type_dynamic_print_0 (module P : ABSTRACT_PRINTABLE_0) =
  let module T = struct
    include Xtype.Matcher_0(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (Zero (module T))
  | None ->
      raise (Invalid_argument "add_abstract_type_dynamic_print: received non abstract type")

let add_abstract_type_dynamic_print_1 (module P : ABSTRACT_PRINTABLE_1) =
  let module T = struct
    include Xtype.Matcher_1(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (One (module T))
  | _ ->
      let stype =
        stype_of_ttype (T.t (Obj.magic (DT_var 0)))
      in
      Format.printf "%a\n%!" print_stype stype;
      Printexc.print_raw_backtrace stdout (Printexc.get_callstack 10);
      raise (Invalid_argument "add_abstract_type_dynamic_print: received non abstract type")

let add_abstract_type_dynamic_print_2 (module P : ABSTRACT_PRINTABLE_2) =
  let module T = struct
    include Xtype.Matcher_2(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (Two (module T))
  | _ -> raise (Invalid_argument "add_abstract_type_dynamic_print: received non abstract type")

module type UNSAFE_ABSTRACT_PRINTABLE_1 = sig
  type 'a t
  val name: string
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type UNSAFE_ABSTRACT_PRINTABLE_2 = sig
  type ('a, 'b) t
  val name: string
  val printer: Format.formatter -> 'a printer -> 'b printer-> ('a, 'b) t printer
end

let add_unsafe_abstract_type_dynamic_print_0 ~name
    (printer: Format.formatter -> 'a printer ) =
  add_abstract_type_dynamic_print_0 ( module struct
    type t = Obj.t
    let t : t ttype = Obj.magic (DT_abstract (name, []))
    let printer = Obj.magic printer
  end )

let add_unsafe_abstract_type_dynamic_print_1
    (module P : UNSAFE_ABSTRACT_PRINTABLE_1) =
  add_abstract_type_dynamic_print_1 ( module struct
    type 'a t = Obj.t
    let t (a : 'a ttype) : 'a t ttype =
      Obj.magic (DT_abstract (P.name, [stype_of_ttype a]))
    let printer = Obj.magic P.printer
  end )

let add_unsafe_abstract_type_dynamic_print_2
    (module P : UNSAFE_ABSTRACT_PRINTABLE_2) =
  add_abstract_type_dynamic_print_2 ( module struct
    type ('a, 'b) t = Obj.t
    let t (a : 'a ttype) (b : 'b ttype)  : ('a, 'b) t ttype =
      Obj.magic (DT_abstract (P.name, [stype_of_ttype a; stype_of_ttype b]))
    let printer = Obj.magic P.printer
  end )

let pp_may_left_paren ppf parens =
  Format.pp_open_box ppf 1;
  if parens then Format.pp_print_char ppf '('

let pp_may_right_paren ppf parens =
  if parens then Format.pp_print_char ppf ')';
  Format.pp_close_box ppf ()

let pp_par_when_neg ppf abs print x =
  let open Format in
  if abs x <> x then begin
    pp_print_char ppf '('; print ppf x; pp_print_char ppf ')'
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
  and print_dynamic : type t. t ttype -> bool -> t -> unit =
    fun t parens x ->
      match xtype_of_ttype t with
      | Unit -> pp_print_string fmt "()"
      | Bool -> pp_print_bool fmt x
      | Char -> pp_print_char fmt x
      | Int -> pp_par_when_neg fmt abs pp_print_int x
      | Int32 -> pp_par_when_neg fmt Int32.abs pp_print_int32 x
      | Int64 -> pp_par_when_neg fmt Int64.abs pp_print_int64 x
      | Nativeint -> pp_par_when_neg fmt Nativeint.abs pp_print_nativeint x
      | Float -> pp_par_when_neg fmt Float.abs pp_print_float x
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
              | Zero m -> begin
                  let module T = (val m) in
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is TypEq.Eq) ->
                    T.printer fmt x
                end
              | One m -> begin
                  let module T = (val m) in
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t, TypEq.Eq)) ->
                    let pr x = print_dynamic t false x in
                    let printer = T.printer fmt pr in
                    printer x
                end
              | Two m -> begin
                  let module T = (val m) in
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t1, t2, TypEq.Eq)) ->
                    let pr1 x = print_dynamic t1 false x in
                    let pr2 x = print_dynamic t2 false x in
                    let printer = T.printer fmt pr1 pr2 in
                    printer x
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
  let t (type a) (type b) (a: a ttype) (b: b ttype) = hashtbl_t a b

  let printer ppf (print1 : 'a printer) (print2 : 'b printer)
      (h : ('a, 'b) Hashtbl.t) =
    let first = ref true in
    let print_el key value =
      if !first then first := false
      else (pp_print_char ppf ';'; pp_print_space ppf ());
      pp_open_box ppf 1; pp_print_char ppf '(';
      print1 key;
      pp_print_char ppf ','; pp_print_space ppf ();
      print2 value;
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
let () = add_abstract_type_dynamic_print_2 (module Hashtbl_printer)

let () =
  add_abstract_type_dynamic_print_0 (module struct
    type t = unit
    let t = unit_t
    let printer ppf _t =  Format.fprintf ppf "()"
  end) ;
  add_abstract_type_dynamic_print_1 (module struct
    type 'a t = 'a ttype
    let t (type a) (a: a ttype) = ttype_t a
    let printer ppf _ t = print_stype ppf (stype_of_ttype t)
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
    (-2)
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
    show [%t: tt ttype] tt_t;
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
