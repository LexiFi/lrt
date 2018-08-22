module List_ = struct

  let range n m =
    let rec range acc m =
      if n >= m then acc else
        let m = pred m in
        range (m :: acc) m
    in
    range [] m

  let findi prop lst =
    let i = ref 0 in
    let rec f = function
      | [] -> raise Not_found
      | hd :: tl -> if prop hd then !i else (incr i; f tl)
    in f lst

end

module Array_ = struct

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
    (* TODO: heuristic to delay building the dispatch table until N lookups occured
       (use simple list search before)? *)

    (* A slightly faster, but much less readable, implementation is in public/tests/strtbl.mf. *)

    type tree =
      | Node of {pos: int; first: int; sub: tree array}
      | Leaf of int * string
      | Fail
      (* Decision tree (pos is the index to look up). *)

    type t = tree array  (* dispatch on the string's length *)

    (* Dispatching *)

    let rec eval_tree s = function
      | Fail -> (-1)
      | Leaf (i, s2) -> if s = s2 then i else (-1)
      | Node {pos; first; sub} ->
          let c = Char.code (String.unsafe_get s pos) in
          if c < first then (-1)
          else let i = c - first in
            if i >= Array.length sub then (-1)
            else eval_tree s (Array.unsafe_get sub i)

    let lookup trees s =
      let len = String.length s in
      if len >= Array.length trees then (-1)
      else eval_tree s (Array.unsafe_get trees len)


    (* Preparation *)

    let split_at strings i =
      let buckets = Array.make 256 [] in
      let min_char = ref 256 in
      let max_char = ref (-1) in
      let rec loop = function
        | ((_, s) as x) :: tl ->
            let c = Char.code s.[i] in
            if c > !max_char then max_char := c;
            if c < !min_char then min_char := c;
            buckets.(c) <- x :: buckets.(c);
            loop tl
        | [] ->
            ()
      in
      loop strings;
      (!min_char, !max_char, buckets)

    let score (min_char, max_char, buckets) =
      let max_len = ref 0 in
      for i = min_char to max_char do
        let l = List.length buckets.(i) in
        if l > !max_len then max_len := l
      done;
      !max_len

    let rec split idxs = function
      | [i, s] -> Leaf (i, s)
      | [] -> Fail
      | strings ->
          let best_score = ref max_int in
          let best_idx = ref (-1) in
          let best_split = ref (0, 0, [||]) in
          let rec loop = function
            | i :: rest ->
                let res = split_at strings i in
                let score = score res in
                if score < !best_score then (best_score := score; best_idx := i; best_split := res);
                loop rest
            | [] -> ()
          in
          loop idxs;
          let pos = !best_idx in
          let (first, last, buckets) = !best_split in
          let idxs = List.filter ((!=) pos) idxs in (* optim *)
          Node
            {
              pos;
              first;
              sub = Array.init (last - first + 1) (fun i -> split idxs buckets.(i + first));
            }


    let prepare strings : t =
      let rec max_len acc = function
        | [] -> acc
        | hd :: tl -> max_len (max acc (String.length hd)) tl
      in
      let max_len = max_len 0 strings in
      let buckets = Array.make (max_len + 1) [] in
      let rec dispatch i = function
        | [] -> ()
        | hd :: tl ->
            let len = String.length hd in
            buckets.(len) <- (i, hd) :: buckets.(len);
            dispatch (i + 1) tl
      in
      dispatch 0 strings;
      Array.mapi
        (fun len strings ->
           let idxs = List_.range 0 len in
           split idxs strings
        )
        buckets
  end
end

module List = List_
module Array = Array_
