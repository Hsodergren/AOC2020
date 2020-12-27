open Core

type exp = Int of int
         | Sum of exp * exp
         | Prod of exp * exp

let rec eval = function
  | Int i -> i
  | Sum (e1,e2) -> eval e1 + eval e2
  | Prod (e1,e2) -> eval e1 * eval e2

let int_exp i = Int i
let sum_exp e1 e2  = Sum  (e1, e2)
let prod_exp e1 e2 = Prod (e1, e2)

module Parser = struct
  open Angstrom
  let digit = function | '0'..'9' -> true | _ -> false
  let int = take_while1 digit >>| int_of_string
  let spaces = skip_while (Char.equal ' ')
  let parens p = char '(' *> p <* char ')'
  let add = char '+' *> return sum_exp
  let prod = char '*' *> return prod_exp

  let chain1 e op =
    let rec go acc =
      (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc
    in
    e >>= fun init -> go init

  let exp = fix (fun p ->
      let e = parens p <|> (int >>| int_exp) in
      let fac = chain1 e (spaces *> add <* spaces) in
      chain1 fac (spaces *> prod <* spaces)
    )

  let parse str = match parse_string ~consume:All exp str with Ok v -> v | Error msg -> failwith msg
end

let parse_file f =
  In_channel.create f
  |> In_channel.input_lines
  |> List.map ~f:Parser.parse
