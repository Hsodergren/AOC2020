open Core

module Parser = struct
  open Angstrom

  let letter = function | 'a'..'z' -> true | _ -> false

  let record = sep_by1 (char '\n') (take_while1 letter)

  let file = sep_by (skip_many1 end_of_line) record <* skip_many end_of_line

  let parse s = match parse_string ~consume:Consume.All file s with
    | Ok v -> v
    | Error s -> failwith s
end

let string_to_set str =
  String.to_list str
  |> Char.Set.of_list

let get_uniq_answers strs =
  List.map ~f:string_to_set strs
  |> Char.Set.union_list

let get_common_answers strs =
  List.fold ~f:(fun acc str -> Char.Set.inter acc (string_to_set str))
    ~init:(string_to_set @@ List.hd_exn strs)
    (List.tl_exn strs)

let sum2 l = List.sum (module Int) l ~f:Fun.id
let lengths = List.map ~f:Char.Set.length
let sum = List.fold ~f:(+) ~init:0

let parse f =
  In_channel.create f
  |> In_channel.input_all
  |> Parser.parse
