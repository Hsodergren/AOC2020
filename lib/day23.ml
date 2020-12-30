open Core

let rev_cmp compare a b = compare a b * -1

let max_elt l ~compare =
  match List.max_elt l ~compare with
  | Some v -> v
  | None -> failwith "error"

let rec find_dst ~min ~max cur take =
  let cur = if (cur-1) < min then max else cur-1 in
  if List.mem take cur ~equal:Int.equal
  then find_dst ~min ~max cur take
  else cur

let move_to take dst l =
  let rec aux l acc last =
    match l with
    | hd::tl ->
      if hd = dst
      then aux tl ((List.rev (hd::take)) @ acc) last
      else aux tl (hd::acc) last
    | [] -> last::acc
  in
  match l with
  | hd::tl -> List.rev (aux tl [] hd)
  | _ -> failwith "asd"

let play l =
  let min,max =
    max_elt l ~compare:(rev_cmp Int.compare),
    max_elt l ~compare:(Int.compare)
  in
  let find_dst = find_dst ~min ~max in
  let rec aux l n =
    if n = 0 then l else
    match l with
    | cur::c1::c2::c3::tl ->
      let take = [c1;c2;c3] in
      let dst = find_dst cur take in
      let l = move_to take dst (cur::tl) in
      aux l (n-1)
    | _ -> assert false
  in
  aux l

let prepare_p2_list l nums =
  let len = List.length l in
  l @ List.init (nums-len) ~f:(fun i -> i+len+1)

let parse_str str =
  String.to_list str
  |> List.map ~f:(fun x -> int_of_char x - int_of_char '0')
