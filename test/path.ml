open Dynt

let pprint p =
  Format.(printf "%a\n%!" Path.print p)

let tprint x =
  Ttype.stype_of_ttype x
  |> Format.(fprintf std_formatter "%a\n%!" Stype.print_stype)

let vprint t v =
  Format.(printf "%a\n%!" (Print.print ~t) v)

let dprint dyn =
  let open Ttype in
  let Dyn (t,v) = dyn in
  vprint t v

type t =
  | A of {b: (int array list * string)}
[@@deriving t]

let value = A { b = [[|0;1|];[|0;1;2|];[|0|]], "string" }

let assert_some = function
  | Some x -> x
  | None -> assert false

let%expect_test _ =
  let f p =
    let Path.{get;_} = Path.lens p in
    let t = Xtype.project_path t p in
    pprint p;
    tprint t;
    get value |> assert_some |> vprint t;
    print_endline "----------------"
  in
  f Path.[];
  f [%path? [A b]];
  f [%path? [A b; ([],_)]];
  f [%path? [A b; (_,[])]];
  f [%path? [A b; ([],_); [1]]];
  f [%path? [A b; ([],_); [1]; [|2|]]];
  [%expect {|
    [%path? []]
    (t =
       | A of
        (t.A =
           {
             b: (int array list * string);
           }))
    A {b = ([[|0; 1|]; [|0; 1; 2|]; [|0|]], "string")}
    ----------------
    [%path? [A b]]
    (int array list * string)
    ([[|0; 1|]; [|0; 1; 2|]; [|0|]], "string")
    ----------------
    [%path? [A b; ([],_)]]
    int array list
    [[|0; 1|]; [|0; 1; 2|]; [|0|]]
    ----------------
    [%path? [A b; (_,[])]]
    string
    "string"
    ----------------
    [%path? [A b; ([],_); [1]]]
    int array
    [|0; 1; 2|]
    ----------------
    [%path? [A b; ([],_); [1]; [|2|]]]
    int
    2
    ---------------- |}]

type y = Int of int | Bool of bool | Pair of int * string
type z = Y of {y1: y; y2: y; y3: y}
type x = { x1 : z; x2 : z}
type r = x * y
type s = r list
type f = s array
type e = { e : f }

let p = [%path? [e; [|50|]; [1]; ([],_); x1; Y y2; Pair (_,[])]]

let%expect_test _ =
  pprint p;
  [%expect {| [%path? [e; [|50|]; [1]; ([],_); x1; Y y2; Pair (_,[])]] |}]

let value = [| ["hey"; "hi"] |]
let p2 = [%path? [[|0|]; [1]]]
let l2 = Dynt.Path.lens p2

let%expect_test _ =
  print_endline (l2.get value |> assert_some);
  print_endline (l2.set value "salut" |> assert_some |> l2.get |> assert_some);
  [%expect {|
    hi
    salut |}]

(* The following stuff was an experiment before implementing the path ppx *)

module B1 = struct
  open Dynpath
  open Ttype
  module Xtype = Xtype_depr

  type steps = Internal.step list
  type 'a t = Path : {steps: steps; root: 'a ttype; target: 'b ttype} -> 'a t

  let root t : 'a t =
    Path {steps=[];root=t;target=t}

  let constr name (Path p: 'a t) : 'a t option =
    match Xtype.xtype_of_ttype p.target with
    | Sum s ->
      let i = Xtype.Sum.lookup_constructor s name in
      if i < 0 then None else
        let Xtype.Constructor c =
          Array.get (Xtype.Sum.constructors s) i
        in Some (
          Path { p with target = Xtype.Constructor.ttype c
                      ; steps = Internal.Constructor (name, -1) :: p.steps })
    | _ -> None

  let field name (Path p: 'a t) : 'a t option =
    match Xtype.xtype_of_ttype p.target with
    | Record r -> begin
      match Xtype.Record.find_field r name with
      | Some (Xtype.Field f) -> Some (
          Path { p with target = Xtype.RecordField.ttype f
                      ; steps = Internal.Field name :: p.steps })
      | None -> None
    end
    | _ -> None

  let tuple n (Path p: 'a t) : 'a t option =
    match Xtype.xtype_of_ttype p.target with
    | Tuple r -> begin
      let fields = Xtype.Record.fields r in
      if List.length fields > n then
        let Xtype.Field f = List.nth fields n in
        Some ( Path { p with target = Xtype.RecordField.ttype f
                           ; steps = Internal.Tuple_nth n :: p.steps })
      else None
    end
    | _ -> None

  let list nth (Path p: 'a t) : 'a t option =
    match Xtype.xtype_of_ttype p.target with
    | List (t,_) -> Some (
          Path { p with target = t
                      ; steps = Internal.List_nth nth :: p.steps })
    | _ -> None

  let array nth (Path p: 'a t) : 'a t option =
    match Xtype.xtype_of_ttype p.target with
    | Array (t,_) -> Some (
          Path { p with target = t
                      ; steps = Internal.Array_nth nth :: p.steps })
    | _ -> None

  let get (Path p : 'a t) (x : 'a) : dynamic option =
    match extract ~t:p.root (Obj.magic (List.rev p.steps)) x with
    | _, value -> Some (Dyn (p.target, value))
    | exception (Failure _) -> None

  let close (target: 'b ttype) (Path p : 'a t)
    : ('a, 'b, _) Dynpath.t option =
    match ttypes_equality_modulo_props target p.target with
    | Some _ -> Some (Obj.magic (List.rev p.steps))
    | None -> None

  let (|>>) x f =
    match x with
    | None -> None
    | Some y -> f y

  let (>>) x f = f (root x)
  let (||>) x t = x |>> close t

end

let xprint p value =
  match p with
  | Some p -> begin match B1.get p value with
      | None -> print_endline "Invalid value"
      | Some dyn -> dprint dyn
    end
  | None -> print_endline "Invalid path"

type sum =
  | A of int
  | B of (int * int)
[@@deriving t]

let a = A 42
let b = B (41,0)

let%expect_test _ =
  xprint B1.(sum_t >> constr "A") a ;
  xprint B1.(sum_t >> constr "B") b ;
  [%expect {|
    42
    (41, 0) |}]

type record =
  { a: int
  ; b: int * int
  ; c: sum
  ; d: string list
  ; e: bool array
  }
[@@deriving t]

let record =
  { a = 42
  ; b = (41, 0)
  ; c = B (7, 13)
  ; d = ["dynamic"; "types"; "are"; "cool"]
  ; e = [|false;true;false|]
  }

let%expect_test _ =
  xprint B1.(record_t >> field "a") record ;
  xprint B1.(record_t >> field "b") record ;
  xprint B1.(record_t >> field "c" |>> constr "B") record ;
  xprint B1.(record_t >> field "b" |>> tuple 0) record ;
  xprint B1.(record_t >> field "d" |>> list 1) record ;
  xprint B1.(record_t >> field "e" |>> array 2) record ;
  let () =
    match B1.(record_t >> field "e" |>> array 2 ||> bool_t) with
    | Some _ -> print_endline "Valid path"
    | None -> print_endline "Invalid Path"
  in
  [%expect {|
    42
    (41, 0)
    (7, 13)
    41
    "types"
    false
    Valid path |}]
