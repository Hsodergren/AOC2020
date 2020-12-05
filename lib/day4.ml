open Core

type height = | In of int | Cm of int
let cm i = Cm i
let inch i = In i
type t = { byr:int    ; iyr:int    ; eyr:int    ; hgt: height
         ; hcl:string ; ecl:string ; pid:string ; cid:string option}

module Parser = struct
  open Angstrom

  let letter = function | 'a'..'z' -> true | _ -> false
  let hex = function | 'a'..'f' | '0'..'9' -> true | _ -> false
  let alphanum = function | 'a'..'z' | '0'..'9' | '#' -> true | _ -> false
  let digit = function | '0'..'9' -> true | _ -> false
  let whitespace = function | ' ' | '\n' -> true | _ -> false
  let skip_char c = skip (Char.equal c)

  let field =
    (take_while letter <* char ':' >>= fun key ->
    take_while alphanum >>= fun value ->
    return (key,value)) <?> "field"

  let record = sep_by1 (skip whitespace) field <?> "record"

  let file = (sep_by (skip_many1 (skip_char '\n')) record <* skip_many (skip_char '\n')) <?> "file"

  let hgt =
    take_while digit >>= fun v ->
    let v = int_of_string v in
    ((string "cm" >>| fun _ -> Cm v)
     <|>
     (string "in" >>| fun _ -> In v))

  let hcl =
    peek_char_fail >>= function
    | '#' -> begin
        advance 1 >>= fun () ->
        take_while hex >>= fun s ->
        if String.length s = 6
        then return ("#" ^ s)
        else fail "wrong length"
      end
    | _ -> fail "#"

  let ecl = take_while letter >>= function
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" as s -> return s
    | s -> fail ("unknown color: " ^ s)

  let pid = take_while digit >>= fun s ->
    if String.length s = 9
    then return s
    else fail "wrong number of digits"

  let parse p s = parse_string ~consume:Consume.All p s
  let parse_file str = match parse file str with | Ok v -> v
                                                 | Error s -> failwith s
  let parse_hgt = parse hgt
  let parse_hcl = parse hcl
  let parse_ecl = parse ecl
  let parse_pid = parse pid
end

let make_t byr iyr eyr hgt hcl ecl pid cid =
  {byr;iyr;eyr;hgt;hcl;ecl;pid;cid}

let (<*>) f arg =
  match f, arg with
  | Some f, Some arg -> Some (f arg)
  | _ ->  None

let to_opt = function
  | Ok v -> Some v
  | Error _ -> None

let int = int_of_string_opt
let p pars str = str |> pars |> to_opt
let height = p Parser.parse_hgt
let hcl = p Parser.parse_hcl
let ecl = p Parser.parse_ecl
let pid = p Parser.parse_pid

let int_between_opt i ~high ~low =
  if Int.between i ~high ~low then Some i else None 

let t_of_assoc assoc =
  let open Option in
  let f str = List.Assoc.find assoc ~equal:String.equal str in
  Some make_t
  <*> (f "byr" >>= int >>= int_between_opt ~low:1920 ~high:2002)
  <*> (f "iyr" >>= int >>= int_between_opt ~low:2010 ~high:2020)
  <*> (f "eyr" >>= int >>= int_between_opt ~low:2020 ~high:2030)
  <*> (f "hgt" >>= height >>= function
    | Cm i -> int_between_opt i ~low:150 ~high:193 >>| cm
    | In i -> int_between_opt i ~low:59  ~high:76  >>| inch)
  <*> (f "hcl" >>= hcl)
  <*> (f "ecl" >>= ecl)
  <*> (f "pid" >>= pid)
  <*> Some (f "cid")

let file f =
  In_channel.read_all f
  |> Parser.parse_file
  |> List.filter_map ~f:t_of_assoc
