open Core

module Seat = struct
  type t = | Floor
           | Free
           | Occupied [@@deriving eq]

  let of_char = function
    | '.' -> Floor
    | 'L' -> Free
    | '#' -> Occupied
    | c -> failwithf "char %c not valid seat" c ()
end

let valid_cell (sizex,sizey) (x,y) =
  not (x < 0 || y < 0 || x >= sizex || y >= sizey)

let get_adjecent_cells mat (x,y) =
  let size = Array.length mat.(1), Array.length mat in
  let idxs = [x-1,y-1;x,y-1;x+1,y-1;x-1,y;x+1,y;x-1,y+1;x,y+1;x+1,y+1] in
  List.map ~f:(fun (x,y) -> mat.(y).(x)) (List.filter ~f:(valid_cell size) idxs)

let get_adjecent_visible mat (x,y) =
  let size = Array.length mat.(1), Array.length mat in
  let directions = [-1,0 ; 1,0 ; 0,-1 ; 0,1 ;
                    -1,-1 ; -1,1 ; 1,-1 ; 1,1] in
  let rec loop (x,y) ((dx,dy) as d) =
    let (x,y) as pos = (dx+x,dy+y) in
    if not @@ valid_cell size pos
    then None
    else match mat.(y).(x) with
      | Seat.Occupied | Seat.Free as v -> Some v
      | Floor -> loop pos d
  in
  List.filter_map ~f:(loop (x,y)) directions

let map_adj mat ~f ~find_adj =
  let copy = Array.map mat ~f:(fun x -> Array.copy x) in
  for y = 0 to Array.length mat - 1 do
    for x = 0 to Array.length mat.(1) - 1 do
      copy.(y).(x) <- f (find_adj mat (x,y)) mat.(y).(x)
    done;
  done;
  copy

let count_occ countf seats = countf seats ~f:(function | Seat.Occupied -> true | _ -> false)
let count_occ_mat mat =
  Array.fold ~f:(+) ~init:0 (Array.map ~f:(count_occ Array.count) mat)

let f rec_occ seats v =
  let num_occ = count_occ List.count seats in
  match v with
  | Seat.Free when num_occ = 0 -> Seat.Occupied
  | Seat.Occupied when num_occ >= rec_occ-> Seat.Free
  | v -> v

let do_untill_eq start ~f ~eq =
  let rec loop v =
    let new_v = f v in
    if eq v new_v then v else loop new_v
  in
  loop start

let mat_eq a b = Array.equal (Array.equal Seat.equal) a b

let part1 mat =
  do_untill_eq
    ~eq:mat_eq
    ~f:(fun mat -> map_adj ~f:(f 4) mat ~find_adj:get_adjecent_cells)
    mat

let part2 mat =
  do_untill_eq
    ~eq:mat_eq
    ~f:(fun mat -> map_adj ~f:(f 5) mat ~find_adj:get_adjecent_visible)
    mat

let parse_lines lines =
    lines
    |> List.map ~f:(fun l -> Array.map ~f:Seat.of_char (String.to_array l))
    |> List.to_array

let parse_file f =
  In_channel.create f
  |> In_channel.input_lines
  |> parse_lines

let parse_str str =
  String.split_lines str
  |> parse_lines
