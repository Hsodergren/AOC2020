open Core

let closest_plus num i =
  let div = num / i in
  div * i + i

let find_bus n buses =
  let buses = List.map ~f:snd buses in
  let min a b = if snd a < snd b then a else b in
  List.reduce_exn ~f:min (List.map ~f:(fun i -> i,closest_plus n i) buses)

let p2 l =
  let rec aux step v l =
    match l with
    | [] -> v
    | (ix,bus)::tl as l ->
      if (v+ix) mod bus = 0
      then aux (step*bus) v tl
      else aux step (v+step) l
  in
  aux 1 1 l

let parse_one i = function
  | "x" -> None
  | num -> Some (i, int_of_string num)

let parse_str str =
  let input_l = String.split ~on:'\n' str in
  let buses = List.filter_mapi ~f:parse_one @@ String.split ~on:',' (List.nth_exn input_l 1) in
  int_of_string @@ List.nth_exn input_l 0, buses

let parse_file f = parse_str @@ In_channel.input_all (In_channel.create f)

let parse_p2 f = snd @@ parse_file f
