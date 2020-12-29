open Core

module Bit = struct
  type t = [ `One | `Zero | `X ]

  let to_char = function
    | `One -> '1'
    | `Zero -> '0'
    | `X -> 'X'

  let of_char = function
    | '1' -> `One
    | '0' -> `Zero
    | 'X' -> `X
    | _ -> failwith "invalid char"
end

module BitStr : sig
  type t
  module Mask : sig type t val zero : t val of_str : string -> t end
  val zero : t
  val of_int : int -> t
  val to_int : t -> int
  val mask_value : t -> Mask.t -> t
  val mask_addr : t -> Mask.t -> t list
end = struct
  type t = Bit.t list

  let num_bits = 36
  let zero = List.init num_bits ~f:(fun _ -> `Zero)

  module Mask = struct
    type t = Bit.t list

    let zero = List.init num_bits ~f:(fun _ -> `X)
    let of_str str =
      if String.length str = num_bits
      then List.map ~f:Bit.of_char @@ String.to_list str
      else failwith "wrong length string"
  end

  let of_int i =
    let rec aux i acc =
      if i = 1 then `One::acc
      else if i mod 2 = 0
      then aux (i/2) (`Zero::acc)
      else aux (i/2) (`One::acc)
    in
    let l = aux i [] in
    let bit_diff = num_bits - List.length l in
    if bit_diff < 0
    then failwith "int overflow, only 36 bit integers"
    else (List.init bit_diff ~f:(fun _ -> `Zero)) @ l

  let to_int l =
    let rec aux l n =
      match l with
      | [] -> 0
      | `Zero::tl | `X::tl -> aux tl (n+1)
      | `One::tl -> Int.(2 ** n) + aux tl (n+1)
    in
    aux (List.rev l) 0

  let combine (b1,b2) =
    match b1 with
    | `X -> b2
    | _ -> b1

  let combine2 (b1,b2) =
    match b1 with
    | `X | `One -> b1
    | `Zero -> b2

  let mask_value i m =
    List.map ~f:combine (List.zip_exn m i)

  let mask_addr i m =
    let rec get_addrs acc = function
      | [] -> [acc]
      | `X::tl -> get_addrs (`One::acc) tl @ get_addrs (`Zero::acc) tl
      | (`One | `Zero as hd)::tl -> get_addrs (hd::acc) tl
    in
    let mask = List.map ~f:combine2 (List.zip_exn m i) in
    List.map ~f:List.rev @@ get_addrs [] mask
end

module Memory : sig
  type t

  val empty: t

  val to_list:  t -> (int * BitStr.t) list

  val store: t -> int -> BitStr.t -> t
end = struct
  type t = BitStr.t Int.Map.t

  let empty = Int.Map.empty

  let to_list t = Int.Map.to_alist t

  let store t key data = Int.Map.set t ~key ~data
end

type instr =
  | Mask of string
  | Store of {addr: int; value: int}

let mask str = Mask str

type status = { mask: BitStr.Mask.t ; memory: Memory.t }

let empty_status = { mask=BitStr.Mask.zero; memory=Memory.empty }

module Parser = struct
  open Angstrom

  let bitstr_char = function 'X' | '1' | '0' -> true | _ -> false
  let digit = function '0'..'9' -> true | _ -> false
  let spaces = skip_many (char ' ')
  let eq = spaces *> char '=' *> spaces
  let int = take_while1 digit >>| int_of_string
  let bitstr = take_while1 bitstr_char

  let mask = string "mask" *> eq *> bitstr >>| mask
  let mem =
    let* addr = string "mem" *> char '[' *> int <* char ']' in
    let* value = eq *> int in
    return @@ Store {addr;value}

  let instr = mask <|> mem
  let many_instr = sep_by end_of_line instr <* skip_many end_of_line

  let parse s =
    match parse_string ~consume:Consume.All many_instr s with
    | Ok v -> v
    | Error e -> failwith e
end

let exec_inst ({mask;_} as status) = function
  | Mask s -> {status with mask=BitStr.Mask.of_str s}
  | Store {addr;value} ->
    let value = BitStr.(mask_value (of_int value) mask) in
    {status with memory=Memory.store status.memory addr value}

let exec_inst2 ({mask;_} as status) = function
  | Mask s -> {status with mask=BitStr.Mask.of_str s}
  | Store {addr;value} ->
    let open BitStr in
    let addrs = mask_addr (of_int addr) mask in
    let value = of_int value in
    List.fold
      (List.map ~f:to_int addrs)
      ~init:status
      ~f:(fun s addr -> {s with memory=Memory.store s.memory addr value})

let exec_program f = List.fold ~init:empty_status ~f

let parse_file file =
  In_channel.create file
  |> In_channel.input_all
  |> Parser.parse

module Utils = struct
  let sum_memory mem =
    List.map ~f:(fun s -> BitStr.to_int (snd s)) (Memory.to_list mem)
    |> List.fold ~f:(+) ~init:0
end
