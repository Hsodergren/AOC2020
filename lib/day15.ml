open Core

let play start num_turns =
  let map = Hashtbl.create (module Int) in
  let last_vs, turn = ref 0, ref 0 in
  List.iteri start ~f:(fun i v -> Hashtbl.set map ~key:v ~data:(i+1); last_vs := v; turn := i+1);
  let rec aux prev_vs turn =
    if turn = num_turns then prev_vs else
    let next_vs = match Hashtbl.find map prev_vs with
      | Some v -> turn-v
      | None -> 0
    in
    Hashtbl.set map ~key:prev_vs ~data:turn;
    aux next_vs (turn+1)
  in
  aux !last_vs !turn

let parse str =
  List.map ~f:int_of_string @@ String.split str ~on:','
