module List_ = struct

  let findi prop lst =
    let i = ref 0 in
    let rec f = function
      | [] -> raise Not_found
      | hd :: tl -> if prop hd then !i else (incr i; f tl)
    in f lst

end

module Array = struct

  let map_to_list f t =
    Array.fold_right (fun el acc -> f el :: acc) t []

  let of_list_rev = function
    | [] -> [||]
    | (hd :: _) as l ->
      let i = ref (List.length l) in
      let t = Array.make !i hd in
      List.iter (fun e -> decr i; t.(!i) <- e) l;
      t
end

module String = struct

  module Tbl = struct
    type t = (string, int) Hashtbl.t

    let prepare l =
      let t = Hashtbl.create (List.length l) in
      let i = ref 0 in
      List.iter (fun e -> Hashtbl.replace t e !i ; incr i) l ;
      t

    let lookup t s = Hashtbl.find t s

  end
end

module List = List_
