open Dynt
open Variant

let variant_round t value =
  to_variant ~t value |> of_variant ~t

let string_round t value =
  to_variant ~t value
  |> Format.asprintf "%a" print_variant
  |> variant_of_string
  |> of_variant ~t

let round cmp t v =
  cmp v (variant_round t v) = 0 &&
  cmp v (string_round t v) = 0

let lazy_compare a b = compare (Lazy.force a) (Lazy.force b)
let lazy_round t v = round lazy_compare t v
let round t v = round compare t v

let shows t v =
  Format.printf "%a\n%!" print_variant (to_variant ~t v)

let showv t v =
  Format.printf "%a\n%!" (Print.print ~t:Variant.t) (to_variant ~t v)

type a = (int * int) [@@deriving t]

let%test _ = round a_t (-1, 3)
let%test _ = round a_t (max_int, min_int)

type b = float list [@@deriving t]

let%test _ = round b_t [0.; neg_infinity; infinity; max_float; min_float;
                        1.17e99; 1.17e-99; nan]

let%test _ = round unit_t ()

type c = bool Lazy.t [@patch lazy_t] [@@deriving t]

let%test _ =
  lazy_round c_t (lazy (Random.self_init (); Random.bool ()))

type d =
  { d1 : int [@prop {of_variant_old_name="d"}]
  ; d2 : float [@prop {of_variant_default="1e-4"}]
  }[@@deriving t]

let ht = Hashtbl.create 3
let () = Hashtbl.add ht "a" (Some {d1=1;d2=nan});
  Hashtbl.add ht "b" (Some {d1=max_int;d2=1e42});
  Hashtbl.add ht "c" None

let ht_t = hashtbl_t string_t (option_t d_t)

let%test _ = round ht_t ht

let%expect_test _ = shows ht_t ht;
  [%expect {|
    [("c", None); ("b", Some{d1 = 4611686018427387903; d2 = 1e+42});
     ("a", Some{d1 = 1; d2 = nan})] |}]

(* Test whether record fields can be reordered *)
let%test _ =
  of_variant ~t:d_t (Record ["d1", Int 0; "d2", Float 0.]) = {d2=0.; d1=0}
let%test _ =
  of_variant ~t:d_t (Record ["d2", Float 0.; "d1", Int 0]) = {d2=0.; d1=0}
(* of_variant_old_name, of_variant_default *)
let%test _ =
  of_variant ~t:d_t (Record ["d", Int 0]) = {d2=1e-4; d1=0}

type e =
  | A of string [@prop {of_variant_old_name="E1"}]
  | B of char * char [@prop {of_variant_old_name="E2"}]
  | C of {i: nativeint [@prop {of_variant_old_name="native"}]}
        [@prop {of_variant_old_name="E3"}]
[@@deriving t]

let%test _ = round e_t (A "a")
let%test _ = round e_t (B ('b','b'))
let%test _ = round e_t (C {i=Nativeint.max_int})

let%expect_test _ =
  showv e_t (A "a");
  showv e_t (B ('b','b'));
  showv e_t (C {i=Nativeint.zero});
  [%expect {|
    Constructor ("A", Some (String "a"))
    Constructor ("B", Some (Tuple [Int 98; Int 98]))
    Constructor ("C", Some (Record [("i", String "0")])) |}]

let%test _ =
  of_variant ~t:e_t (Constructor ("A", Some (Tuple [String "a"]))) = (A "a")
let%test _ =
  of_variant ~t:e_t (Constructor ("A", Some (String "a"))) = (A "a")
let%test _ =
  of_variant ~t:e_t (Constructor ("E1", Some (String "a"))) = (A "a")

let%test _ =
  of_variant ~t:e_t (Constructor ("E3", Some (Record [("native", String "0")])))
  = (C {i=Nativeint.zero})

type f = int * string [@@deriving t]

let f_flip = function
  | Tuple [String s; Int i] -> Some (Tuple [Int i; String s])
  | _ -> None

let f_custom = function
  | Int i -> Some (i, string_of_int i)
  | _ -> None

let f_i = ref 0
let f_default () = f_i := succ !f_i; (!f_i,"0")

let f_t = of_variant_mapper ~name:"Variantizer.f_flip" ~t:f_t f_flip
let f_t = of_variant_custom ~name:"Variantizer.f_custom" ~t:f_t f_custom
let f_t = of_variant_default ~name:"Variantizer.f_default" ~t:f_t f_default

let%expect_test _ =
  Format.printf "%a\n%!" Ttype.print [%t: f];
  [%expect {| (int * string) + [of_variant_custom = "0"] + [of_variant_custom = "1"] + [of_variant_custom = "2"] |}]

let of_variant_string ~t s =
  variant_of_string s |> of_variant ~t

let%test "mapper0"  = of_variant_string ~t:f_t "(1, \"1\")" = (1,"1")
let%test "mapper1"  = of_variant_string ~t:f_t "(\"1\", 1)" = (1,"1")
let%test "custom"   = of_variant_string ~t:f_t "42" = (42,"42")
let%test "default0" = of_variant_string ~t:f_t "Invalid" = (1,"0")
let%test "default1" = of_variant_string ~t:f_t "Invalid" = (2,"0")
let%test "default2" = of_variant_string ~t:f_t "Invalid" = (3,"0")
