open Lib
open Core

let filename = Array.get (Sys.get_argv ()) 1

let () =
  let data = Day9.parse_file filename in
  let v = Option.value_exn in
  match Util.timeit @@ fun () -> Day9.find_weakness data 25 with
  | None -> failwith "error"
  | Some sum ->
    Printf.printf "sum: %d\n" sum;
    let p2 = Util.timeit @@ fun () -> v (Day9.find_cont_sum ~sum data) in
    let max = v (List.max_elt ~compare:Int.compare p2) in
    let min = v (List.max_elt ~compare:(Comparable.reverse Int.compare) p2) in
    printf "min: %d; max: %d\n" min max

