open Core

let opt_or o f =
  match o with
  | Some _ -> o
  | None -> f ()

let rec aux sum acc l n =
  let (>>?) = opt_or in
  if sum < 0 then None else
  if n = 1
  then
    match List.find l ~f:(fun a -> a = sum) with
    | Some v -> Some (v::acc)
    | None -> None
  else
    match l with
    | [] -> None
    | hd::tl ->
      aux (sum-hd) (hd::acc) tl (n-1) >>? fun () -> aux sum acc tl n

let n_sum ~n ~sum l =
  aux sum [] l n


exception Result of int * int
exception Result3 of int * int * int
let sum_tree sum l =
  try
    List.iteri ~f:(fun i x ->
        List.iteri ~f:(fun j y ->
            List.iteri ~f:(fun k z ->
                if i <> j && j <> k && x + y + z = sum
                then raise (Result3 (x,y,z))
                else ()
              ) l
          ) l
      ) l;
  None
  with Result3 (x,y,z) -> Some (x,y,z)

let sum_two sum l =
  try
    List.iteri ~f:(fun i x ->
        List.iteri ~f:(fun j y ->
            if i <> j && x + y  = sum
            then raise (Result (x,y))
            else ()
          ) l
      ) l;
  None
  with Result (x,y) -> Some (x,y)

let read_input f =
  Core.In_channel.fold_lines
    ~init:[]
    ~f:(fun acc l -> int_of_string l::acc)
    (Core.In_channel.create f)
