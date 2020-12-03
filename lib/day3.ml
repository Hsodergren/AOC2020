open Core

type node = | Tree | Empty

let get x arr =
  let len = Array.length arr in
  let idx = x mod len in
  arr.(idx)
  
let rec next dy t =
  if dy = 0 then t
  else
    match t with
    | [] -> []
    | _::tl -> next (dy-1) tl

let rec walk dx dy x t =
  match t with
  | [] -> 0
  | hd::_ ->
    (match get x hd with Tree -> 1 | Empty -> 0) + walk dx dy (x+dx) (next dy t)

let parse_row str =
  let len = String.length str in
  let res = Array.create ~len Empty in
  for i = 0 to len - 1 do
    if Char.(String.get str i = '#')
    then res.(i) <- Tree
  done;
  res

let parse_file f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:parse_row
