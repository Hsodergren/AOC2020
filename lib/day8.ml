open Core
type instruction = | Acc of int
                   | Jmp of int
                   | Nop of int [@@deriving show]

module Parser = struct
  open Angstrom

  let digit = function '0'..'9' -> true | _ -> false

  let number =
    (char '+' >>| fun _ i -> i) <|> (char '-' >>| fun _ i -> -i) >>= fun sign ->
    take_while digit >>| fun i ->
    sign (int_of_string i)

  let inst =
    take_till (function ' ' -> true | _ -> false) <* char ' ' >>= fun kind ->
    number >>= fun i ->
    match kind with
    | "acc" -> return @@ Acc i
    | "jmp" -> return @@ Jmp i
    | "nop" -> return @@ Nop i
    | _ -> fail ("invalid kind: " ^ kind)

  let file = sep_by (char '\n') inst <* skip_many (char '\n' <|> char ' ')
  let parse s = match parse_string ~consume:Consume.All file s with Ok v -> v | _ -> failwith "error"
end

type status = { pos : int
              ; acc : int
              ; visited : int list [@opaque]} [@@deriving show]

let empty_status = {pos=0; visited=[]; acc=0}

let execute_instruction ins status =
  let visited = status.pos::status.visited in
  match ins with
  | Acc acc -> { pos=status.pos + 1
               ; acc=status.acc + acc
               ; visited}
  | Nop _ -> {status with pos=status.pos+1 ; visited}
  | Jmp jmp -> {status with pos=status.pos+jmp ; visited}

let exec status memory =
  if List.mem status.visited status.pos ~equal:Int.equal
  then `Cycle_found status.pos
  else
    let instruction = memory.(status.pos) in
    let result = execute_instruction instruction status in
    if result.pos = Array.length memory
    then `Done result.acc
    else `Success result

(* part 1 *)
let execute insts =
  let rec aux status =
    match exec status insts with
    | `Success status -> aux status
    | `Done _ as d -> d
    | `Cycle_found pos -> `Cycle (status.acc, pos)
  in
  aux empty_status

let flip_inst = function
  | Acc _ as a -> a
  | Nop i -> Jmp i
  | Jmp i -> Nop i

(* part 2 *)
exception Done of int
let modify insts =
  try
    Array.iteri insts
      ~f:(fun i inst ->
          let copy = Array.copy insts in
          copy.(i) <- flip_inst (inst);
          match execute copy with
          | `Cycle _ -> ()
          | `Done acc -> raise (Done acc)
        );
    None
  with Done acc ->
    Some acc

let parse_file f =
  In_channel.create f
  |> In_channel.input_all
  |> Parser.parse
  |> Array.of_list
