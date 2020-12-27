let (%) f g x = f (g x)

type ('a,'b) either = Left of 'a | Right of 'b

let partition_eithers xs =
  let rec go ls rs = function
    | [] ->
        List.rev ls, List.rev rs
    | x::xs ->
        match x with
        | Left l ->
            go (l::ls) rs xs
        | Right r ->
            go ls (r::rs) xs
  in
  go [] [] xs

module LazyList = struct
  type 'a node = Nil | Cons of 'a * 'a t
  and 'a t = 'a node Lazy.t
  let empty = lazy Nil
  let singleton x = lazy (Cons (x, empty))
  let force = Lazy.force
  let rec map f l = lazy (
    match force l with
    | Nil -> Nil
    | Cons (h, t) -> Cons (f h, map f t)
  )
  let rec append l1 l2 = l