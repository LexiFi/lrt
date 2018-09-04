open Dynt_core.Stype
open Dynt_core.Ttype
open Dynt_core.Std

type 'a printer = 'a -> unit

module type ABSTRACT_PRINTABLE_0 = sig
  include Xtypes.TYPE_0
  val printer: Format.formatter -> t printer
end

module type ABSTRACT_PRINTABLE_1 = sig
  include Xtypes.TYPE_1
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type ABSTRACT_PRINTABLE_2 = sig
  include Xtypes.TYPE_2
  val printer: Format.formatter -> 'a printer -> 'b printer -> ('a, 'b) t printer
end

module type PRINTABLE_MATCHER_0 = sig
  include Xtypes.MATCHER_0
  val printer: Format.formatter -> t printer
end

module type PRINTABLE_MATCHER_1 = sig
  include Xtypes.MATCHER_1
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type PRINTABLE_MATCHER_2 = sig
  include Xtypes.MATCHER_2
  val printer: Format.formatter -> 'a printer -> 'b printer -> ('a, 'b) t printer
end

type printable =
  | Zero of (module PRINTABLE_MATCHER_0)
  | One  of (module PRINTABLE_MATCHER_1)
  | Two  of (module PRINTABLE_MATCHER_2)

let abstract_printers : (string, printable) Hashtbl.t = Hashtbl.create 17

let add_abstract_type_dynamic_print_0 (module P : ABSTRACT_PRINTABLE_0) =
  let module T = struct
    include Xtypes.Matcher_0(P)
    let printer = P.printer
  end in
  match T.is_abstract with
  | Some name ->
      Hashtbl.add abstract_printers name (Zero (module T))
  | None ->
      raise (Invalid_argument "add_abstract_type_dynamic_print: received non abstract type")

let add_abstract_type_dynamic_print_1 (module P : ABSTRACT_PRINTABLE_1) =
  let module T = struct
    include Xtypes.Matcher_1(P)
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
    include Xtypes.Matcher_2(P)
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
    pp_print_char ppf '('; print x; pp_print_char ppf ')'
  end else
    print x

let print_dynamic ppf (t, x) =
  let open Format in
  let open Xtypes in
  let rec print_dynamic : type t. t ttype -> bool -> t -> unit =
    fun t parens x ->
      match xtype_of_ttype t with
      | Unit -> pp_print_string ppf "()"
      | Bool -> pp_print_bool ppf x
      | Char -> pp_print_char ppf x
      | Int -> pp_par_when_neg ppf abs (pp_print_int ppf) x
      | Int32 -> pp_par_when_neg ppf Int32.abs (fun x ->
          pp_print_string ppf (Int32.to_string x)) x
      | Int64 -> pp_par_when_neg ppf Int64.abs (fun x ->
          pp_print_string ppf (Int64.to_string x)) x
      | Nativeint -> pp_par_when_neg ppf Nativeint.abs (fun x ->
          pp_print_string ppf (Nativeint.to_string x)) x
      | Float -> pp_par_when_neg ppf Float.abs (fun x ->
          pp_print_string ppf (Float.to_string x)) x
      | String -> pp_print_char ppf '\"';
        pp_print_string ppf (String.escaped x); pp_print_char ppf '\"'
      (* | Date -> pp_print_string ppf (string_of_date x) *)
      | Option (t, _) -> begin
          match x with
          | None -> pp_print_string ppf "None"
          | Some next_x ->
            pp_may_left_paren ppf parens;
            pp_print_string ppf "Some"; pp_print_space ppf ();
            print_dynamic t true next_x ;
            pp_may_right_paren ppf parens
        end
      | List (_, _) when x = [] -> pp_print_string ppf "[]"
      | List (t, _) ->
        let rec p_list = function
          | [] -> ()
          | [e] -> print_dynamic t false e
          | e :: rest ->
            print_dynamic t false e; pp_print_char ppf ';';
            pp_print_space ppf (); p_list rest
        in
        pp_open_box ppf 1; pp_print_char ppf '['; p_list x;
        pp_print_char ppf ']'; pp_close_box ppf ()
      | Array (t,_) ->
        pp_open_box ppf 2; pp_print_string ppf "[|";
        for i = 0 to Array.length x - 1 do
          if i > 0 then begin pp_print_char ppf ';'; pp_print_space ppf () end;
          print_dynamic t false x.(i)
        done;
        pp_print_string ppf "|]"; pp_close_box ppf ()
      | Tuple record ->
        let fields = Record.fields record in
        let rec pr i = function
          | [] -> ()
          | Field hd :: tl ->
            let t = RecordField.ttype hd in
            if i > 0 then
              begin pp_print_char ppf ','; pp_print_space ppf () end;
            print_dynamic t false (RecordField.get hd x) ;
            pr (i+1) tl
        in
        pp_open_box ppf 1; pp_print_char ppf '('; pr 0 fields;
        pp_print_char ppf ')'; pp_close_box ppf ()
      | Record record ->
        let fields = Record.fields record in
        let rec pr i = function
          | [] -> ()
          | Field hd :: tl ->
            let name = RecordField.name hd in
            let t = RecordField.ttype hd in
            if i > 0 then
              begin pp_print_char ppf ';'; pp_print_space ppf () end;
            pp_open_box ppf 1;
            pp_print_string ppf name; pp_print_string ppf " =";
            pp_print_space ppf ();
            print_dynamic t false (RecordField.get hd x);
            pp_close_box ppf ();
            pr (i+1) tl
        in
        pp_open_hvbox ppf 1; pp_print_char ppf '{'; pr 0 fields;
        pp_print_char ppf '}'; pp_close_box ppf ()
      | Sum s ->
        let Constructor c = Sum.constructor s x in
        let t = Constructor.ttype c in
        let print_constr () =
          print_dynamic t false (Constructor.project_exn c x) in
        pp_may_left_paren ppf parens;
        pp_print_string ppf (Constructor.name c);
        ( match xtype_of_ttype t with
          | Unit -> ()
          | Record _
          | Tuple _ -> pp_print_cut ppf () ; print_constr ()
          | _ -> pp_print_cut ppf () ; pp_print_space ppf () ; print_constr ()
        ) ;
        pp_may_right_paren ppf parens
      | Lazy (t, _) ->
        pp_may_left_paren ppf parens;
        pp_print_string ppf "lazy ";
        begin match (Lazy.force x) with
          | exception exn ->
            pp_print_string ppf "(raise ";
            pp_print_string ppf (Printexc.to_string exn);
            pp_print_char ppf ')'
          | x -> print_dynamic t true x
        end;
        pp_may_right_paren ppf parens
      | Function _ -> pp_print_string ppf "<fun>"
      | Object _ -> pp_print_string ppf "<object>"
      | Prop (_, t, _) -> print_dynamic t parens x
      | Abstract (name, t, _l) ->
        let rec use_first = function
          | [] ->
            pp_print_string ppf "<abstract: ";
            pp_print_string ppf name;
            pp_print_string ppf ">";
          | hd :: tl -> begin
              match hd with
              | Zero m -> begin
                  let module T = (val m) in
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is TypEq.Eq) ->
                    T.printer ppf x
                end
              | One m -> begin
                  let module T = (val m) in
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t, TypEq.Eq)) ->
                    let pr x = print_dynamic t false x in
                    let printer = T.printer ppf pr in
                    printer x
                end
              | Two m -> begin
                  let module T = (val m) in
                  match T.is_t t with
                  | None -> use_first tl
                  | Some (T.Is (t1, t2, TypEq.Eq)) ->
                    let pr1 x = print_dynamic t1 false x in
                    let pr2 x = print_dynamic t2 false x in
                    let printer = T.printer ppf pr1 pr2 in
                    printer x
                end
            end
        in
        pp_open_box ppf 0;
        use_first (Hashtbl.find_all abstract_printers name) ;
        pp_close_box ppf ()
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
  add_abstract_type_dynamic_print_2 (module struct
    type ('a,'b) t = ('a,'b, Path.kind) Path.t
    let t (type a) (type b) (a_t:a ttype) (b_t:b ttype) =
      [%t: (a,b,Path.kind) Path.t]
    let printer ppf _a _b t =
      Path.steps_of_path t |> Path.Internal.print_steps ppf
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
    (Tpl(0, 0), Atm 0)
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
    print_endline "int paths:";
    List.iter (show [%t: (tt,int,Path.kind) Path.t])
      (Xtypes.all_paths ~root:tt_t ~target:int_t);
    print_endline "bool paths:";
    List.iter (show [%t: (tt,bool,Path.kind) Path.t])
      (Xtypes.all_paths ~root:tt_t ~target:bool_t);
    print_endline "string paths:";
    List.iter (show [%t: (tt,string,Path.kind) Path.t])
      (Xtypes.all_paths ~root:tt_t ~target:string_t);
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
       | Tupl of (int * bool * string))
    int paths:
    .Inl.x
    .Tupl.(0)
    bool paths:
    .Inl.y
    .Tupl.(1)
    string paths:
    .Inl.z
    .Tupl.(2) |}]
end
