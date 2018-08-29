open Dynt

let f (t: 'a ttype) (x: 'a) : bool =
  Print.show ~t x ; true

let dynamic (f: 'a ttype -> 'a -> 'b) : 'b Check.gen =
  let open Check in
  stype >>= fun s ->
    let open Xtypes in
    let Ttype t = sttype_of_stype s in
    let valgen = of_type_gen [] ~t in
    map (f (Obj.magic t)) (Obj.magic valgen)

let%expect_test _ =
  let _ = Check.test 99 ~seed:42 ~generator:(dynamic f) (fun x -> x) in ();
  [%expect {|
    0
    true
    0.
    false
    false
    0.
    None
    0.
    0
    ()
    {l___ = 0; l____ = 0.; lMMM = (); m = ""}
    Some []
    (-0.675761338323)
    (-0.0188530888408)
    true
    (-0.067264970392)
    true
    0.184769557789
    1.54182555899
    (-0.655880186707)
    true
    0.993392728269
    "M"
    [true]
    Iqqqq
    (-2.78220768459)
    true
    [0.0739572942257]
    false
    1
    ("O", ([], true, Some (-1)), (-2),
     ((-1), (1, [QOOOOOO(0., false, 0)], false, "O"),
      {n7777 = "";
       m88888 = "";
       qiii = 0.301016207784;
       i = ();
       p5555 = {phhhhhh = false}}))
    true
    ["OL"]
    [|Some (Oqq{r5 = false})|]
    "MP"
    {jNNN = 0.433895051479}
    (-0.0110682845116)
    false
    false
    Qqq([|0|], (-0.80499282293), 1, false, (-0.352458316833), (-0.256986772642))
    1.94195456803
    false
    ""
    1.85724999011
    (-5.75120091811)
    false
    false
    0.807267816737
    false
    true
    false
    5.37888743915
    None
    "L"
    (-2)
    ()
    V______{q_ = U______(0., (Some true, "")); k___ = ""}
    (-2)
    Some true
    1
    "QRN"
    (-0.131442943588)
    (-2.71974756941)
    (-3)
    6.6916341763
    false
    "NJ"
    ((), (-7))
    false
    (-1.28235504031)
    "XX"
    "KUGY"
    8.90315711871
    [false; false; false; false; true]
    "PLMLVG"
    "DHY"
    ()
    (-11)
    true
    "JR"
    (-3.95604461432)
    (-7)
    ()
    None
    V000000(true, "RLP")
    "J[\\OSH"
    7
    (-11)
    [|true|]
    ""
    "QSFKL"
    ()
    false
    6.94656500593
    ("QZD]TMZ", (-1.26536427811), None)
    ["OIG"; "O"; "ZRV"; "DGCTTKE"; ""; ""]
    "I"
    false
    () |}]


