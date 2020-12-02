open Core

type t = { f: int
         ; t: int
         ; c: char
         ; password: string }

module Parser = struct
  open Angstrom
  let digit = take_while (function | '0'..'9' -> true | _ -> false) >>| int_of_string

  let stringp = take_while (function | 'a'..'z' -> true | _ -> false)

  let char1 = take 1 >>| fun s -> String.get s 0

  let password = (fun f t c password -> {f;t;c;password}) <$>
                 (digit <* char '-') <*>
                 (digit <* char ' ') <*>
                 (char1 <* string ": ") <*>
                 stringp

  let parse str =
    match Angstrom.parse_string ~consume:Consume.All password str with
    | Ok s -> s | Error s -> failwith s
end

let num_filter pred =
  List.fold
    ~f:(fun acc v -> (if pred v then succ else Fun.id) acc)
    ~init:0

let times_in_string c s = num_filter (Char.equal c) @@ String.to_list s

let valid {f;t;c;password} =
  Int.between ~low:f ~high:t @@ times_in_string c password

let xor b1 b2 = match b1,b2 with
  | true,false | false,true -> true
  | _ -> false

let valid2 {f;t;c;password} =
  let validate c i = Char.(String.get password (i-1) = c) in
  xor (validate c t) (validate c f)

let p1 = num_filter valid
let p2 = num_filter valid2

let get_input f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:Parser.parse
