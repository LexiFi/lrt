open Dynt

type a = (int * int)
and b = int option
and c = string list
and d = (int * (int * string))
and 'a e = 'a list
[@@deriving t]

let matcher =
  Matcher.(
    empty ~modulo_props:true
    |> add ~t:a_t ~f:(fun _ -> print_endline "a")
    |> add ~t:b_t ~f:(fun _ -> print_endline "b")
    |> add ~t:c_t ~f:(fun _ -> print_endline "c")
    |> add0 (module struct
      type res = unit
      type t = d [@@deriving t]
      let f _ = print_endline "d" end)
    |> add1 (module struct
      type res = unit
      type 'a t = 'a e [@@deriving t]
      let f _ _ = print_endline "e" end)
    |> add2 (module struct
      type res = unit
      type ('a,'b) t = ('a,'b) Hashtbl.t [@patch hashtbl_t] [@@deriving t]
      let f _ _ _ = print_endline "f" end)
  )

let%expect_test _ =
  Matcher.apply matcher ~t:a_t (1,1) ;
  Matcher.apply matcher ~t:b_t None ;
  Matcher.apply matcher ~t:c_t ["string";"list"] ;
  Matcher.apply matcher ~t:d_t (1,(1,"")) ;
  Matcher.apply matcher ~t:(e_t float_t) [1.] ;
  Matcher.apply matcher ~t:(hashtbl_t int_t float_t) (Hashtbl.create 1);
  [%expect {|
    a
    b
    c
    d
    e
    f |}]


