open Dynt

let print p =
  Format.(printf "%a\n%!" Path.print p)

type t =
  | A of {b: int array list}
[@@deriving t]

let%expect_test _ =
  let p = Path.root in
  print p;
  [%expect {| (.) |}]
