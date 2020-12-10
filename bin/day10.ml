open Lib
let file = Sys.argv.(1)
let () =
  Util.timeit @@ fun () ->
  Day10.parse_file file
  |> fun v -> Util.timeit (fun () -> (v|>Day10.find_differences|>Day10.diffs, Day10.number_of_confs v))
  |> fun ((p1a,p1b),p2) -> Printf.printf "%d %d %d\n" p1a p1b p2
