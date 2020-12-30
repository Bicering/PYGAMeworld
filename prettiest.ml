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
  let rec append l1 l2 = lazy (
    match force l1 with
    | Nil -> force l2
    | Cons (h, t) -> Cons (h, append t l2)
  )
  let rec concat ll = lazy (
    match force ll with
    | Nil -> Nil
    | Cons (h, t) -> append h (concat t) |> force
  )
  let is_empty l = force l = Nil
end

let force = Lazy.force

type doc =
  | Empty
  | Line of bool
  | Nest of int * doc
  | Char of char
  | Text of string
  | Cat of doc * doc
  | Union of doc * doc
  | Column of (int -> doc)
  | Nesting of (int -> doc)
  | MaxColumn of int

type sdoc =
  | SEmpty
  | SChar of char * sdoc Lazy.t
  | SText of string * sdoc Lazy.t
  | SLine of int * sdoc Lazy.t

type idoc =
  | IEmpty
  | IChar of char * idoc
  | IText of string * idoc
  | ILine of int * idoc

let rec flatten x =
  match x with
  | Empty
  | Char _
  | Text _
  | MaxColumn _ -> x
  | Line false -> Empty