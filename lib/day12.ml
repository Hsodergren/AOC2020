open Core

type pos = (int * int)
module Direction = struct
  type t = [ `E | `S | `W | `N ]

  let go_dist dis dir (x,y) =
    match dir with
    | `E -> (x+dis, y)
    | `W -> (x-dis, y)
    | `N -> (x, y+dis)
    | `S -> (x, y-dis)

  let go_relative mult (dx,dy) (x,y) = (x+dx*mult,y+dy*mult)

  let rec do_n n f x = if n = 0 then x else do_n (n-1) f (f x)
  let rotate_right = function
    | `E -> `S
    | `S -> `W
    | `W -> `N
    | `N -> `E
  let rotate_180 = do_n 2 rotate_right
  let rotate_left = do_n 3 rotate_right

  let rotate_cw (x,y) = (y,-x)
  let rotate_180_pos = do_n 2 rotate_cw
  let rotate_ccw = do_n 3 rotate_cw
end

module P1 = struct
  type t = | Move of (pos -> pos)
           | Move_direction of (Direction.t -> pos -> pos)
           | Rotate of (Direction.t -> Direction.t)
end

module P2 = struct
  type t = | MoveWP of (pos -> pos)
           | Move_ship of (wp:pos -> ship:pos -> pos)
end

type ship_status = { position: pos
                   ; waypoint: pos
                   ; heading: Direction.t }

let empty_status = { position = (0,0)
                   ; waypoint = (10,1)
                   ; heading = `E }

let rec sail2 ({position;waypoint;_} as status) insts =
  match insts with
  | [] -> status
  | P2.MoveWP f::tl -> sail2 {status with waypoint=f waypoint} tl
  | Move_ship f::tl ->
    sail2 {status with position=f ~wp:waypoint ~ship:position} tl

let rec sail1 ({position;heading; _} as status) insts =
  match insts with
  | [] -> status
  | P1.Move f::tl -> sail1 {status with position=f position} tl
  | Move_direction f::tl ->
    sail1 {status with position=f heading position} tl
  | Rotate f::tl -> sail1 {status with heading=f heading} tl

let inst1 ty num =
  let open Direction in
  match ty,num with
  | (`R | `L), 180 -> P1.Rotate rotate_180
  | `L, 90 | `R, 270 -> P1.Rotate rotate_left
  | `R, 90 | `L, 270 -> P1.Rotate rotate_right
  | `F, n -> P1.Move_direction (go_dist n)
  | (`N | `W | `S | `E as dir), n -> P1.Move (go_dist n dir)
  | _ -> failwith "invalid inst"

let inst2 ty num =
  let open Direction in
  match ty,num with
  | (`R | `L), 180 -> P2.MoveWP rotate_180_pos
  | `L, 90 | `R, 270 -> P2.MoveWP rotate_ccw
  | `R, 90 | `L, 270 -> P2.MoveWP rotate_cw
  | `F, n -> P2.Move_ship (fun ~wp ~ship -> go_relative n wp ship)
  | (`N | `W | `S | `E as dir), n -> P2.MoveWP (go_dist n dir)
  | _ -> failwith "invalid inst"

let split_str str ~at =
  let l = String.length str in
  String.sub str ~pos:0 ~len:at, String.sub str ~pos:at ~len:(l-at)

let parse_instruction f str =
  let ty,num = split_str str ~at:1 in
  let ty = match ty with
    | "N" -> `N
    | "S" -> `S
    | "E" -> `E
    | "W" -> `W
    | "L" -> `L
    | "R" -> `R
    | "F" -> `F
    | _ -> failwithf "Invalid type %s" ty ()
  in
  f ty (int_of_string num)

let parse_file1 f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:(parse_instruction inst1)

let parse_file2 f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:(parse_instruction inst2)
