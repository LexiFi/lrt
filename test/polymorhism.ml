open Dynt
open Types
open Internal

(* module T : sig
  type 'a t
  val test: 'a t -> 'a t end = struct
  type 'a t = 'a list
  let test =
    let a =
        if true then 42 else ((fun x -> x) 42)
    in
    ignore a;
    let id (type s) (x : s t) : s t = x in
    ignore (id [5], id ["a"]); id
end*)

module M : sig

  type 'num rectangle = { a: 'num ; b: 'num }

  val rectangle_t : 'num ttype -> 'num rectangle ttype

end = struct

  type 'num rectangle = { a: 'num ; b: 'num }

  let rectangle_t =
    (Internal.create_record_type "rectangle" []
       (fun __rec_stype  ->
          ([("a", [], DT_var 0);
            ("b", [], DT_var 0)],
           Record_regular)))

  let rectangle_t (type num) (num : num ttype) : num rectangle ttype =
    substitute [|(stype_of_ttype num)|] (Obj.magic rectangle_t)
    |> Obj.magic
end

module N : sig

  type 'num rectangle = { a: 'num ; b: 'num }

  val rectangle_t : 'num ttype -> 'num rectangle ttype

end = struct

  type 'num rectangle = { a: 'num ; b: 'num }

  include (
  struct
    let rectangle =
      (Internal.create_record_type "rectangle" []
         (fun __rec_stype  ->
            ([("a", [], DT_var 0);
              ("b", [], DT_var 0)],
             Record_regular)))

    let rectangle_t num =
      substitute [|(stype_of_ttype num)|] (Obj.magic rectangle)
      |> Obj.magic
  end : sig
    val rectangle_t : 'num ttype -> 'num rectangle ttype
  end )

end
