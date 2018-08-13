open Types

val int_t: int ttype
val string_t: string ttype
val float_t: float ttype

val pair: 'a ttype -> 'b ttype -> ('a * 'b) ttype
val triple: 'a ttype -> 'b ttype -> 'c ttype -> ('a * 'b * 'c) ttype
val quartet: 'a ttype -> 'b ttype -> 'c ttype ->
  'd ttype -> ('a * 'b * 'c * 'd) ttype
val quintet: 'a ttype -> 'b ttype -> 'c ttype ->
  'd ttype -> 'e ttype -> ('a * 'b * 'c * 'd * 'e) ttype

val make_abstract: name:string -> 'a ttype -> 'b ttype
