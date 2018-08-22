open Types

val unit_t: unit ttype
val bool_t: bool ttype
val int_t: int ttype
val string_t: string ttype
val float_t: float ttype
val char_t: char ttype
val nativeint_t: nativeint ttype
val int32_t: int32 ttype
val int64_t: int64 ttype

val option_t: 'a ttype -> 'a option ttype
val list_t: 'a ttype -> 'a list ttype
val array_t: 'a ttype -> 'a array ttype
val lazy_t_t: 'a ttype -> 'a lazy_t ttype
