open Dynt.Types

val pair: 'a ttype -> 'b ttype -> ('a * 'b) ttype
val triple: 'a ttype -> 'b ttype -> 'c ttype -> ('a * 'b * 'c) ttype
val quartet: 'a ttype -> 'b ttype -> 'c ttype ->
  'd ttype -> ('a * 'b * 'c * 'd) ttype
val quintet: 'a ttype -> 'b ttype -> 'c ttype ->
  'd ttype -> 'e ttype -> ('a * 'b * 'c * 'd * 'e) ttype

val make_abstract: name:string -> 'a ttype -> 'b ttype

type variant_constructor
val make_variant_constructor: name:string -> stype list -> variant_constructor

val make_variant: name:string -> stype list ->
  (stype -> variant_constructor list) -> stype

type record_field
val make_record_field: name:string -> stype -> record_field

val make_record: name:string -> stype list ->
  (stype -> record_field list) -> stype
