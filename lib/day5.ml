open Core

type 'a t = | Node of 'a t * 'a t
            | Leaf of 'a

let make_tree ~init ~next depth =
  let rec make_tree' depth i =
    if depth = 0
    then Leaf i, next i
    else
      let lt,i = make_tree' (depth-1) i in
      let rt,i = make_tree' (depth-1) i in
      Node (lt,rt), i
  in
  fst (make_tree' depth init)

let rec map ~f = function
  | Leaf i -> Leaf (f i)
  | Node (l,r) -> Node (map ~f l, map ~f r)

let rec traverse path t =
  match path,t with
  | `L::tl, Node (l,_) -> traverse tl l
  | `R::tl, Node (_,r) -> traverse tl r
  | _,Leaf i -> i
  | [],Node _ -> failwith "path not to a leaf"

let rows = make_tree ~next:succ ~init:0 7
let cols = make_tree ~next:succ ~init:0 3

let char_to_trav = function | 'F' | 'L' -> `L
                            | 'B' | 'R' -> `R
                            | _ -> failwith "invalid"

let trav (rowtrav,coltrav) =
  traverse rowtrav rows, traverse coltrav cols

let get_id (row,col) = row * 8 + col

let (>>) f g x = g (f x)

let parse_one line =
  String.to_list line
  |> List.map ~f:char_to_trav
  |> fun l -> List.split_n l 7

let parse_file file =
  In_channel.create file
  |> In_channel.input_lines
  |> List.map ~f:(parse_one >> trav)

let find_missing l =
  let rec aux l =
    match l with
    | hd1::(hd2::_ as tl ) ->
      if hd2 - hd1 = 1
      then aux tl
      else Some (hd1+1)
    | [] | [_] -> None
  in
  aux (List.sort ~compare:Int.compare l)
