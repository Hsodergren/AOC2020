open Core

module Range : sig
  type t [@@deriving show]
  val make: int -> int -> t
  val empty: t
  val (<+>): t -> t -> t
  val valid: t -> int -> bool
end = struct
  type t = Range of int * int
         | Concat of t * t
         | Empty [@@deriving show]

  let empty = Empty

  let make lo hi =
    if lo < hi
    then Range (lo, hi)
    else Range (hi, lo)

  let (<+>) l r = Concat (l,r)

  let rec valid r v =
    match r with
    | Range (low,high) -> Int.between v ~low ~high
    | Concat (l,r) -> valid l v || valid r v
    | Empty -> false
end

module Rule : sig
  type t [@@deriving show]
  val make: string -> Range.t -> t
  val all_valid: t -> int list -> bool
  val equal: t -> t -> bool
  val name: t -> string
  val range: t -> Range.t
end = struct
  type t = (string * Range.t) [@@deriving show]

  let make name range = (name, range)
  let rec all_valid ((_,range) as rule) nums =
    match nums with
    | [] -> true
    | num::tl -> Range.valid range num && all_valid rule tl

  let equal (n1,_) (n2,_) = String.equal n1 n2

  let name (name,_) = name

  let range (_,range) = range
end

let all_non_valid xs range =
  List.filter xs ~f:(fun x -> not (Range.valid range x))

let get_valid_tickets tickets ranges =
  List.filter
    tickets
    ~f:(fun ticket -> List.is_empty (all_non_valid ticket ranges))

let rec find_pop ~f l =
  match l with
  | [] -> None
  | hd::tl ->
    if f hd
    then Some (hd,tl)
    else Option.map ~f:(fun (a,b) -> (a,hd::b)) (find_pop ~f tl)

let simplify_order ls ~eq =
  let rec aux ls =
    match find_pop ~f:(fun (_,l) -> Int.equal (List.length l) 1) ls with
    | Some ((_,l) as v, others) ->
      let hd = List.hd_exn l in
      let others = List.map ~f:(fun (i,os) -> (i,List.filter os ~f:(fun o -> not @@ eq hd o))) others in
      v::aux others
    | None -> ls
  in
  List.mapi ~f:(fun i v -> i,v) ls
  |> aux
  |> List.sort ~compare:(fun (i1,_) (i2,_) -> Int.compare i1 i2)
  |> List.map ~f:snd

let find_order fields rules =
  let find_valid col = List.filter ~f:(fun r -> Rule.all_valid r col) rules in
  let valid_rules = List.map ~f:find_valid fields in
  let valid_rules = simplify_order valid_rules ~eq:Rule.equal in
  let rec aux ruless acc n =
    match ruless with
    | [] -> Some (List.rev acc)
    | rules::tl ->
      let rec loop rules =
        match rules with
        | [] -> None
        | rule::loop_tl ->
          match aux tl (rule::acc) (n+1) with
          | Some v -> Some v
          | None -> loop loop_tl
      in
      List.filter ~f:(fun r -> not @@ List.exists ~f:(Rule.equal r) acc) rules
      |> loop
  in
  aux valid_rules [] 0

type input = {
  rules : Rule.t list
; my_ticket: int list
; other_tickets: int list list
}

module Parser = struct
  open Angstrom
  let digit = function '0'..'9' -> true | _ -> false

  let spaces = skip_while (Char.equal ' ')
  let int = take_while1 digit >>| int_of_string
  let new_lines = skip_many1 end_of_line

  let tag =
    take_till (Char.equal ':') >>= fun tag ->
    advance 1 >>= fun _ -> return tag
  let range =
    int <* char '-' >>= fun lo ->
    int >>= fun hi ->
    return @@ Range.make lo hi

  let ints = sep_by1 (char ',') int
  let rule =
    tag <* spaces >>= fun name ->
    sep_by1 (string " or ") range >>= fun ranges ->
    return @@ Rule.make name (List.reduce_exn ranges ~f:Range.(<+>))
  let header heading =
    new_lines <* string heading <* spaces <* end_of_line

  let input =
    sep_by1 end_of_line rule >>= fun rules ->
    header "your ticket:" >>= fun _ ->
    ints >>= fun my_ticket ->
    header "nearby tickets:" >>= fun _ ->
    sep_by1 end_of_line ints <* new_lines >>= fun other_tickets ->
    return @@ {rules;my_ticket;other_tickets}

  let parse str =
    match parse_string ~consume:Consume.All input str with
    | Ok v -> v
    | Error s -> failwith s
end

let p1 input =
  let ranges = List.map ~f:Rule.range input.rules
  and nums = List.join input.other_tickets in
  all_non_valid nums (List.reduce_exn ~f:(Range.(<+>)) ranges)
  |> List.fold ~f:(+) ~init:0

let parse_file f =
  In_channel.create f
  |> In_channel.input_all
  |> Parser.parse
