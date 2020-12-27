open Core

type t = { ingredients : string list
         ; allergens : string list }

let rem_last str = String.sub str ~pos:0 ~len:(String.length str - 1)

let rec add_many set = function
  | hd::tl -> add_many (Set.add set hd)tl
  | [] -> set

let get_all_allergens foods =
  List.fold foods
    ~init:(Set.empty (module String))
    ~f:(fun acc {allergens;_} -> add_many acc allergens)

let inter_list m l =
  match List.reduce ~f:Set.inter l with
  | Some v -> v
  | None -> Set.empty m

let common_ingredients foods =
  inter_list (module String)
    (List.map ~f:(fun {ingredients;_} -> Set.of_list (module String) ingredients) foods)

let simplify ~eq l =
  let rec aux l used =
    match l with
    | [] -> Some (List.rev used)
    | hd::tl1 ->
      let valids = List.filter hd ~f:(fun v -> not @@ List.mem used v ~equal:eq) in
      let rec try_valids valids =
        match valids with
        | [] -> None
        | hd::tl2 ->
          match aux tl1 (hd::used) with
          | Some _ as v -> v
          | None -> try_valids tl2
      in
      try_valids valids
  in
  aux l []

let p1 foods =
  let allergens = get_all_allergens foods in
  Set.fold
    allergens
    ~init:(Map.empty (module String))
    ~f:(fun acc a ->
        Map.set
          acc
          ~key:a
          ~data:(List.filter foods ~f:(fun {allergens;_} -> List.exists ~f:(String.equal a) allergens)))
  |> Map.map ~f:common_ingredients
  |> Map.to_alist
  |> List.map ~f:(fun v -> Set.to_list @@ snd v)
  |> List.join

let p2 foods =
  let allergens = get_all_allergens foods in
  Set.fold
    allergens
    ~init:(Map.empty (module String))
    ~f:(fun acc a ->
        Map.set
          acc
          ~key:a
          ~data:(List.filter foods ~f:(fun {allergens;_} -> List.exists ~f:(String.equal a) allergens)))
  |> Map.map ~f:common_ingredients
  |> Map.to_alist
  |> List.map ~f:(fun v -> Set.to_list @@ snd v)
  |> simplify ~eq:String.equal

let comma_separate strs =
  List.intersperse strs ~sep:","
  |> List.reduce_exn ~f:(^)

let rec count ingrs foods =
  match foods with
  | [] -> 0
  | {ingredients;_}::tl ->
    List.count ingredients ~f:(List.mem ingrs ~equal:String.equal) + count ingrs tl

let parse_food food =
  let rec parse_ingredients = function
    | [] -> [],[]
    | hd::tl as l ->
      if Char.equal (String.get hd 0) '('
      then [], l
      else let tl, l = parse_ingredients tl in hd::tl, l
  in
  let rec parse_allergens = function
    | [] -> []
    | [hd] -> [rem_last hd]
    | hd::tl ->
      if String.equal hd "(contains"
      then parse_allergens tl
      else rem_last hd::parse_allergens tl
  in
  let l = String.split food ~on:' ' in
  let ingredients,l = parse_ingredients l in
  let allergens = parse_allergens l in
  {ingredients; allergens}

let parse_file f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:parse_food
