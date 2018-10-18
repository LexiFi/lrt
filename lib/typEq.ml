type (_, _) t = Eq : ('a, 'a) t

let refl = Eq
let trans (type a b c) (Eq : (a, b) t) (Eq : (b, c) t) : (a, c) t = Eq
let sym (type a b) (Eq : (a, b) t) : (b, a) t = Eq
let app (type a b) (Eq : (a, b) t) (x : a) : b = x

module Lift (T : sig
  type 'a c
end) =
struct
  let eq (type a b) (Eq : (a, b) t) : (a T.c, b T.c) t = Eq
end
