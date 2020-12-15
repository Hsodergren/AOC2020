open Core

let next_num last_num map turn =
  let next = match Int.Map.find map last_num with
    | Some v -> turn-v
    | None -> 0
  in
  next, Int.Map.set map ~key:last_num ~data:turn

let play start num_turns =
  let init_map, last_vs, turn = List.foldi start
      ~f:(fun i (map,_,_) v ->
          (Int.Map.set map ~key:v ~data:(i+1), v,i+1)
        )
      ~init:(Int.Map.empty,0, 1)
  in
  let rec aux map last_vs turn =
    if turn = num_turns then  last_vs else
    let next_vs = match Int.Map.find map last_vs with
      | Some v -> (turn-v)
      | None -> 0
    in
    aux
      (Int.Map.set map ~key:last_vs ~data:turn)
      next_vs
      (turn+1)
  in
  aux init_map last_vs turn


let parse str =
  List.map ~f:int_of_string @@ String.split str ~on:','
