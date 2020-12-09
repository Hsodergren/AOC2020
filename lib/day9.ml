open Core

module Queue = struct
  type 'a t = {front: 'a list; back: 'a list}

  let enqueue {front;back} v = {front;back=v::back}

  let empty = {front=[];back=[]}

  let of_list l = List.fold l ~f:(fun q v -> enqueue q v) ~init:empty

  let to_list {front;back} = front @ back

  let rec dequeue {front;back} =
    match front, back with
    | [], [] -> None
    | [], back -> dequeue {front=List.rev back;back=[]}
    | (hd::tl),back -> Some (hd, {front=tl;back})

  let dequeue_exn q =
    match dequeue q with
    | Some v -> v
    | None -> failwith "dequeue on empty queue"
end

let find_weakness l pre_len =
  let rec aux pre l =
    match l with
    | [] -> None
    | hd::tl ->
      match Day1.n_sum ~n:2 ~sum:hd (Queue.to_list pre) with
      | None -> Some hd
      | Some _ ->
        let _,q = Queue.dequeue_exn pre in
        aux (Queue.enqueue q hd) tl
  in
  let preamble,l = List.split_n l pre_len in
  aux (Queue.of_list preamble) l

let rec find_cont_sum ~sum l =
  let rec cont_sum sum l acc =
    if sum < 0 then None
    else if sum = 0 then Some acc else
      match l with
      | [] -> None
      | hd::tl -> cont_sum (sum-hd) tl (hd::acc)
  in
  match l with
  | [] -> None
  | _::tl ->
    match cont_sum sum l [] with
    | Some _ as v -> v
    | None -> find_cont_sum ~sum tl

let parse_str str =
  String.split ~on:'\n' str
  |> List.map ~f:int_of_string

let parse_file f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:int_of_string
