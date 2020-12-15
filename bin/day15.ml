open Lib

let start = Sys.argv.(1)

let num_turns = Sys.argv.(2)

let () =
  let start = Day15.parse start in
  let num_turns = int_of_string num_turns in
  let last_num = Util.timeit @@ fun () -> Day15.play start num_turns in
  Printf.printf "%d\n" last_num
