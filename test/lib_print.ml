open Dynt

let show t x = DynPrint.show ~t x

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
  List.iter (show [%t: (tt,int) path])
    (Xtypes.all_paths ~root:tt_t ~target:int_t);
  print_endline "bool paths:";
  List.iter (show [%t: (tt,bool) path])
    (Xtypes.all_paths ~root:tt_t ~target:bool_t);
  print_endline "string paths:";
  List.iter (show [%t: (tt,string) path])
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
