open Core

let fix_list l =
  match List.max_elt l ~compare:Int.compare with
  | None -> [0;3]
  | Some max -> 0::List.sort ~compare:Int.compare l @ [max+3]

let find_differences l =
  let rec aux l diffs =
    match l with
    | [] | [_] -> diffs
    | hd::(hd2::_ as tl) -> aux tl (hd2-hd::diffs)
  in
  aux (fix_list l) []

let count_diffs l = List.count l ~f:((=) 1), List.count l ~f:((=) 3)

let number_of_confs l =
  let memo = ref [] in
  let rec aux l =
    match List.Assoc.find !memo l ~equal:(List.equal Int.equal) with
    | Some v -> v
    | None ->
      match l with
      | [] | [_] -> 0
      | [hd;hd2] -> if hd2-hd > 3 then 0 else 1
      | hd::(hd2::tl2 as tl) -> begin
          let v =
            if (hd2-hd) > 3
            then 0
            else aux tl + aux (hd::tl2)
          in
          memo := (l,v)::!memo;
          v
        end
  in
  aux (fix_list l)

let parse_str str =
  String.split_lines str
  |> List.map ~f:int_of_string

let parse_file f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:int_of_string
