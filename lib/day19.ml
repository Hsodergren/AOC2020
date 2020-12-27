open Core

type rule = String of string
          | Ref of int
          | And of rule list
          | Or of rule * rule

type input = { rules: (int * rule) list
             ; tests: string list }

let rec match_seq match_l base_l =
  match match_l, base_l with
  | mhd::mtl,bhd::btl ->
    if Char.equal mhd bhd
    then match_seq mtl btl
    else `No
  | [], _ -> `Yes base_l
  | _ -> `No

let test rules_map rule seq =
  let rec aux rule seq =
    match rule with
    | String str -> match_seq (String.to_list str) seq
    | Ref i -> aux (List.Assoc.find_exn ~equal:Int.equal rules_map i) seq
    | And (hd::tl) -> begin
        match aux hd seq with
        | `Yes seq -> aux (And tl) seq
        | `No -> `No
      end
    | And [] -> `Yes seq
    | Or (e1, e2) -> begin
        match aux e1 seq with
        | `Yes seq -> `Yes seq
        | `No -> aux e2 seq
      end
  in
  match aux rule (String.to_list seq) with
  | `Yes [] -> `Match
  | `Yes l -> `Not l
  | `No -> `Not []

let modify_rules_p2 rules =
  let add l d v = List.Assoc.add l d v ~equal:Int.equal in
  let rules = add rules 8 (Or (Ref 42, And [Ref 42; Ref 8])) in
  add rules 11 (Or (And [Ref 42; Ref 31], And [Ref 42; Ref 11; Ref 31]))

let num_matching rules tests =
  List.fold
    tests
    ~init:0
    ~f:(fun acc v ->
        match test rules (Ref 0) v with
        | `Match -> (acc+1)
        | `Not _ -> acc)

module Parser = struct
  open Angstrom

  let digit = function '0'..'9' -> true | _ -> false
  let space = Char.equal ' '
  let spaces = skip_while space

  let int = take_while1 digit >>| int_of_string

  let string_rule = peek_char_fail >>= (function
      | '"' ->
        advance 1 *> (take_while (fun c -> not @@ Char.equal '"' c)) >>= fun str ->
        advance 1 >>= fun _ -> return @@ String str
      | _ -> fail "String")

  let int_rule = int >>= fun i -> return @@ Ref i

  let ruleAnd = sep_by1 spaces (int_rule <|> string_rule) >>= fun l -> return @@ And l
  let ruleOr =
    ruleAnd <* spaces <* char '|' <* spaces >>= fun e1 ->
    ruleAnd >>= fun e2 -> return @@ Or (e1,e2)

  let rule = ruleOr <|> ruleAnd <|> string_rule <|> int_rule

  let line =
    int <* char ':' <* skip_while space >>= fun id ->
    rule >>= fun r ->
    return (id,r)

  let rules = sep_by1 end_of_line line

  let tests = sep_by end_of_line (take_while (function 'a'..'z' -> true | _ -> false))

  let input =
    rules >>= fun rules ->
    end_of_line >>= fun () ->
    tests >>= fun tests ->
    return {rules;tests}

  let parse_p str p =  match parse_string ~consume:Prefix p str with | Ok v -> v | Error msg -> failwith msg
  let parse str = parse_p str input
end

let parse_file f =
  In_channel.create f
  |> In_channel.input_all
  |> Parser.parse
