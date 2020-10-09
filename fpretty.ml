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
    let acc, ys = List.fold_left (