open Types

val int_t: int ttype
val string_t: string ttype
val float_t: float ttype

val option_t: 'a ttype -> 'a option ttype
val list_t: 'a ttype -> 'a list ttype
