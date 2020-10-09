module List = struct
  include List
  let zip xs ys =
    let rec go acc = function
      | [], _ | _, [] -> List.rev acc
      | x::xs, y::ys -> go ((x,y)::acc) (xs,ys)
    in
    go [] (xs,ys)
  let rec take n xs =
    match n, xs with
    | 0, _ | _, [] -> []
    | n, x::xs -> x :: take (n-1) xs
  let rec drop n xs =
    match n, xs with
    | 0, _ | _, [] -> xs
    | n, _::xs -> drop (n-1) xs
  let rec last = function
    | [] -> failwith "last: empty list"
    | [x] -> x
    | x::xs -> last xs
  let split_at k xs =
    let rec go l k = function
      | xs when k = 0 ->
          List.rev l, xs
      | [] ->
          List.rev l, []
      | x::xs ->
          go (x::l) (k-1) xs
    in
    go [] k xs
  let iota n =
    let rec go acc n =
      if n = 0 then
        acc
      else
        go ((n-1)::acc) (n-1)
    in
    go [] n
  let map_accuml_rev f acc xs =
    let acc, ys = List.fold_left (fun (acc,ys) x ->
      let acc, y = f acc x in
      acc, y::ys
    ) (acc,[]) xs
    in
    acc, ys
  let map_accuml f acc xs =
    let acc, ys = map_accuml_rev f acc xs in
    acc, List.rev ys
end

(* deque *)

type 'a deque = 'a list * 'a list

let emptyq = [], []
let pushl x (xs,ys) = x::xs, ys
let pushr x (xs,ys) = xs, x::ys
let viewl = function
  | [], [] ->
      None
  | [], ys ->
      let n = List.length ys in
      let ys, xs = List.split_at (n/2) ys in
      (match List.rev xs with
      | x::xs ->
          Some (x, (xs,ys))
      | _ -> assert false
      )
  | x::xs, ys ->
      Some (x, (xs,ys))
let viewr = function
  | [], [] ->
      None
  | xs, [] ->
      let n = List.length xs in
      let xs, ys = List.split_at (n/2) xs in
      (match Lis