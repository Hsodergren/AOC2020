open Core

let contains : (string, (int * string) list) Base.List.Assoc.t -> string -> string -> bool =
  let mem = Hashtbl.create (module String) in
  let rec contains' bags bag_color search_color =
    match Hashtbl.find mem bag_color with
    | None ->
      let cont = List.Assoc.find_exn bags ~equal:String.equal bag_color in
      if String.equal bag_color search_color
      then true
      else
        let v = List.exists
            ~f:(fun (_,c) -> contains' bags c search_color)
            cont
        in
        Hashtbl.set mem ~key:bag_color ~data:v;
        v
    | Some v -> v
  in
  contains'

(* part 1 *)
let count bags color =
  List.length @@ List.filter ~f:(fun (c,_) -> contains bags c color) bags

(* part 2 *)
let rec num_contains bags bag_color =
  let b = List.Assoc.find_exn ~equal:String.equal bags bag_color in
  1 +
  List.fold
    ~f:(fun acc (num,color) -> num * num_contains bags color + acc)
    ~init:0
    b

let parse str =
  let ws = List.concat_map
      ~f:(String.split ~on:' ')
      (String.split ~on:'\n' str)
  in
  let get_color c1 c2 = c1 ^ " " ^ c2 in
  let rec contains ws acc = 
    match ws with
    | num::c1::c2::"bags,"::rest
    | ("1" as num)::c1::c2::"bag,"::rest ->
      contains rest ((int_of_string num, get_color c1 c2)::acc)
    | num::c1::c2::"bags."::rest
    | ("1" as num)::c1::c2::"bag."::rest ->
      (int_of_string num, get_color c1 c2)::acc, rest
    | "no"::"other"::"bags."::rest -> [], rest
    | _ -> failwith "error"
  in
  let rec bags ws acc =
    match ws with
    | c1::c2::"bags"::"contain"::rest ->
      let color = get_color c1 c2 in
      let con, rest = contains rest [] in
      bags rest ((color,con)::acc)
    | [] -> acc
    | _ -> failwith "error"
  in
  bags (List.filter ~f:(fun s -> not @@ String.equal "" s) ws) []

let parse_file f =
  In_channel.create f
  |> In_channel.input_all
  |> parse
