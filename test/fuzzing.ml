open Dynt
open Check

(* Genate random ttypes with random witnesses and throw them at our
 * printing function *)

let%expect_test _ =
  ignore (test 111 ~seed:42 ~generator:(dynamic []) (
        fun (Dyn (t,x)) -> Print.show ~t x ; true));
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
    0.
    0.
    true
    (-0.067264970392)
    true
    0.184769557789
    0.770912779495
    (-0.327940093353)
    true
    0.993392728269
    "M"
    [true]
    Ppppp
    (-1.85480512306)
    true
    [0.0739572942257]
    false
    1
    ("O", ([], true, Some 0), (-1),
     (0, (0, [], false, ""),
      {m5555 = ""; k11111 = ""; ljjj = 0.; q = (); l7777 = {pmmmmmm = false}}))
    true
    [""]
    [|Some (Oqq{r5 = false})|]
    "MP"
    {kLLL = 0.32542128861}
    (-0.00885462760925)
    false
    false
    Jmm([||], 0., 0, false)
    1.94195456803
    false
    ""
    1.54770832509
    (-4.79266743176)
    false
    false
    0.691943842918
    false
    true
    false
    4.61047494784
    None
    "J"
    (-5)
    ()
    P______{m_ = K______(0., (Some true, ""), "", 0.); i___ = ""}
    7
    Some true
    (-6)
    "GP"
    (-0.116838172078)
    (-2.41755339503)
    5
    5.94811926782
    false
    "NJ"
    ((), 4)
    false
    (-1.15411953628)
    "R"
    "TXMI"
    8.09377919883
    [false; false; false; false]
    "KQEE"
    ""
    ()
    1
    true
    "QD"
    (-3.62637422979)
    8
    ()
    None
    H000000
     (true, "PS", true, [|1.76102723368; 2.58578483574|], (-1), false,
      3.64284520596, "R", ())
    "LSTIFU"
    5
    12
    [|true; false; false|]
    "UPPREE"
    "KWMQ"
    ()
    false
    6.45038179122
    ("", (-1.08459795266), None)
    ["X[PFI"; "JXGE"; "OD"; "KD"]
    ""
    false
    ()
    9
    8.0031125471
    true
    [|(-12.5799928382); 12.3106511999; (-5.02930833586); (-9.05753706023)|]
    1.41754908115
    5
    1
    true
    inf
    "TZ\\[MI"
    (-3.64424432628)
    "SN" |}]


