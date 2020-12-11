open Lib

let filename = Sys.argv.(1)

let () =
  let data = Day11.parse_file filename in
  Util.timeit (fun () ->
      let p1 = Util.timeit (fun () -> Day11.part1 data |> Day11.count_occ_mat) in
      let p2 = Util.timeit (fun () -> Day11.part2 data |> Day11.count_occ_mat) in
      Printf.printf "%d\n%d\n" p1 p2
    )
