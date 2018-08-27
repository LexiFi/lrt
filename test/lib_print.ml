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
  show [%t: sum ttype] sum_t;
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
    43
    (sum =
       | Tpl of (int * int)
       | Atm of int) |}]
