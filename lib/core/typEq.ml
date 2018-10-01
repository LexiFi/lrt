type (_, _) t = Eq: ('a, 'a) t

let refl = Eq

let trans (type a) (type b) (type c) (Eq : (a, b) t) (Eq : (b, c) t) =
  (Eq : (a, c) t)

let sym (type a) (type b) (Eq : (a, b) t) = (Eq : (b, a) t)

let app (type a) (type b) (Eq : (a, b) t) (x : a) = (x : b)

module Lift(T : sig type 'a c end) = struct
  let eq (type a) (type b) (Eq : (a, b) t) = (Eq : (a T.c, b T.c) t)
end
