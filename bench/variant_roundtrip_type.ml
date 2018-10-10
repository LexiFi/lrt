open Dynt

type rec1 =
  { rec1_f1 : string
  ; rec1_f2 : int
  ; rec1_f3 : int * string
  ; rec1_f4 : bool
  ; rec1_f5 : float list
  }
and rec2 =
  { rec2_f1 : float
  ; rec2_f2 : float
  ; rec2_f3 : float
  }
and variant =
  | R1 of rec1
  | R2 of rec2
  | V1 of bool option array
  | E1
and t = variant * variant
[@@deriving t]
