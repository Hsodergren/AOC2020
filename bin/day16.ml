open Core
open Lib

let filename = Caml.Sys.argv.(1)

let starts_with s ~start = String.is_substring_at s ~pos:0 ~substring:start
let run () =
  let input = Day16.parse_file filename in
  printf "total tickets: %d\n%!" (List.length input.other_tickets);
  let valid_tickets = Day16.get_valid_tickets input.other_tickets (List.map ~f:Day16.Rule.range input.rules) in
  printf "valid tickets: %d\n%!" (List.length valid_tickets);
  let valid_tickets_t = List.transpose_exn valid_tickets in
  let names = Day16.find_order valid_tickets_t input.rules
              |> Option.map ~f:(fun l -> List.map ~f:Day16.Rule.name l) in
  (match names with
   | Some v ->
     let v = List.filter_mapi v ~f:(fun i name ->
         if starts_with name ~start:"departure"
         then Some (List.nth_exn input.my_ticket i)
         else None
       ) in
     List.reduce_exn v ~f:( * )
     |> sprintf "%d"
  | None -> "no result")

let () =
  run ()
  |> print_endline
