open Core

module Queue : sig
  type 'a t [@@deriving hash, sexp, compare]

  val empty: 'a t
  val enqueue: 'a -> 'a t -> 'a t
  val dequeue: 'a t -> ('a * 'a t) option
  val of_list: 'a list -> 'a t
  val to_list: 'a t -> 'a list
end = struct
  type 'a t = {front: 'a list; back: 'a list} [@@deriving hash, sexp, compare]

  let empty = {front=[];back=[]}

  let enqueue v {front;back} = {front;back=v::back}

  let rec dequeue {front;back} =
    match front,back with
    | [], [] -> None
    | [],_ -> dequeue {front=List.rev back;back=[]}
    | hd::tl,_ -> Some (hd, {front=tl;back})

  let of_list l = {front=l;back=[]}

  let to_list {front;back} = front @ (List.rev back)
end

module Card = struct
  type t = int [@@deriving hash, sexp, compare]
end

module Deck : sig
  type t [@@deriving hash, sexp, compare]

  val of_cards: Card.t list -> t
  val take_first: t -> (Card.t * t) option
  val put_last: Card.t -> t -> t
  val take_top_n: t -> n:int -> t
  val size: t -> int
  val equal: t -> t -> bool
  val score: t -> int option
end = struct
  type t = Card.t Queue.t [@@deriving hash, sexp, compare]

  let of_cards = Queue.of_list
  let take_first = Queue.dequeue
  let put_last = Queue.enqueue
  let take_top_n d ~n =
    Queue.to_list d
    |> (fun l -> List.take l n)
    |> Queue.of_list

  let size d = List.length @@ Queue.to_list d

  let equal d1 d2 =
    List.equal
      Int.equal
      (Queue.to_list d1)
      (Queue.to_list d2)

  let score deck =
    Queue.to_list deck
    |> List.rev
    |> List.mapi ~f:(fun i c -> c * (i+1))
    |> List.reduce ~f:(+)
end

let play_combat p1 p2 =
  let rec round p1 p2 =
      match Deck.take_first p1, Deck.take_first p2 with
        | Some (c1,p1), Some (c2,p2) ->
          let open Deck in
          let cmp = Card.compare c1 c2 in
          if cmp = 1
          then round
              (put_last c2 (put_last c1 p1))
              p2
          else if cmp = -1
          then round
              p1
              (put_last c1 (put_last c2 p2))
          else assert false
        | Some _, None -> p1
        | None, Some _ -> p2
        | None,None -> assert false
  in
  round
    (Deck.of_cards p1)
    (Deck.of_cards p2)

module Deck2 = struct
  type t = Deck.t * Deck.t [@@deriving hash, sexp, compare]
end

let play_rec_combat p1 p2 =
  let rec aux p1 p2 =
    let memo = Hash_set.create (module Deck2) in
    let rec round p1 p2 =
      if Hash_set.mem memo (p1,p2) then `P1 p1 else begin
        Hash_set.add memo (p1,p2);
        match Deck.take_first p1,Deck.take_first p2 with
        | Some (c1,p1d), Some (c2,p2d) ->
          let open Deck in
          let cmp = if size p1d >= c1 && size p2d >= c2
            then begin
              match aux (take_top_n p1d ~n:c1) (take_top_n p2d ~n:c2) with
              | `P1 _ -> 1
              | `P2 _ -> -1
            end else Card.compare c1 c2
          in
          if cmp = 1
          then round
              (put_last c2 (put_last c1 p1d))
              p2d
          else if cmp = -1
          then round
              p1d
              (put_last c1 (put_last c2 p2d))
          else assert false   (* No two equal cards in the deck *)
        | Some _, None -> `P1 p1
        | None, Some _ -> `P2 p2
        | None,None -> assert false
      end
    in
    round p1 p2
  in
  match aux
    (Deck.of_cards p1)
    (Deck.of_cards p2) with
  | `P1 d | `P2 d -> d

let parse_cards l =
  let rec aux l cards acc =
    match l with
    | [] -> cards::acc
    | hd::tl ->
      match int_of_string_opt hd with
      | Some v -> aux tl (v::cards) acc
      | None ->
        if List.is_empty cards
        then aux tl cards acc
        else aux tl [] (cards::acc)
  in
  aux l [] []
  |> List.rev
  |> List.map ~f:List.rev

let to_2tup l = List.nth_exn l 0, List.nth_exn l 1

let parse_str str =
  str
  |> String.split ~on:'\n'
  |> List.filter ~f:(fun v -> not @@ String.equal v "")
  |> parse_cards
  |> to_2tup

let parse_file f =
  In_channel.create f
  |> In_channel.input_all
  |> parse_str
