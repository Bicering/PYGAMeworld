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
  