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
  | Line true -> Char ' '
  | Nest (_, x) -> flatten x
  | Cat (x, y) -> Cat (flatten x, flatten y)
  | Union (x, y) -> flatten x
  | Column f -> Column (flatten % f)
  | Nesting f -> Nesting (flatten % f)

let group x = Union (flatten x, x)
let (<|>) x y = Union (x, y)
let (<.>) x y = Cat (x, y)
let space = Text " "
let (<+>) x y = Cat (x, Cat (space, y))
let (</>) x y = Cat (x, Cat (group (Line true), y))
let (<//>) x y = Cat (x, Cat (group (Line false), y))
let (<+/>) x y = Cat (x, Cat (Union (space, Line true), y))
let (<$>) x y = Cat (x, Cat (Line true, y))
let align x = Column (fun k -> Nesting (fun i -> Nest (k-i, x)))
let nest i x = Nest(i, x)
let int i = Text (string_of_int i)
let intw w i =
  let s = string_of_int i in
  let l = String.length s in
  if w > l then
    Text (String.make (w-l) ' ' ^ s)
  else
    Text s
let width x f = Column (fun k1 -> x <.> Column (fun k2 -> f (k2-k1)))
let vsep = List.fold_left (<$>)

let empty = Empty
let char c = Char c
let text t = Text t
let space = Char ' '
let line = Line true
let linebreak = Line false
let softline = group line
let softbreak = group linebreak
let lbrace = Char '{'
let rbrace = Char '}'
let lbracket = Char '['
let rbracket = Char ']'
let langle = Char '<'
let rangle = Char '>'
let lparen = Char '('
let rparen = Char ')'
let semicolon = Char ';'
let comma = Char ','

let fill_break f x =
  width x (fun w ->
    if w > f then Nest (f, Line false)
    else Text (String.make (f-w) ' '))

let fill f x =
  width x (fun w ->
    if w >= f then Empty
    else Text (String.make (f-w) ' '))

let rec sep_by sep = function
  | [] -> Empty
  | x::xs ->
    let rec go acc = function
      | [] -> acc
      | x::xs -> go (acc <.> sep <.> x) xs
    in
    go x xs

let rec sep_by' sep xs =
  align @@ sep_by sep xs

let enclose l r x = l <.> x <.> r
let fill_sep xs =
  match xs with
  | [] -> Empty
  | x::xs -> List.fold_left (</>) x xs

let rec enclose_sep l r sep = function
  | [] -> l <.> r
  | x::xs ->
    let rec go acc = function
      | [] -> acc
      | x::xs -> go (acc <.> sep <.> x) xs
    in
    align (go (l <.> x) xs <.> r)

let rec enclose_sep_a l r sep a =
  let n = Array.length a in
  if n = 0 then
    l <.> r
  else
    let rec go acc i =
      if i = n then
        acc
      else
        go (acc <.> sep <.> a.(i)) (i+1)
    in
    align (go (l <.> a.(0)) 1 <.> r)

let render_greedy rfrac w x =
  let r = rfrac *. float_of_int w |> int_of_float |> min w |> max 0 in
  let better n k x y =
    let rec fits w = function
      | _ when w < 0 -> false
      | SEmpty -> true
      | SChar (c, x) -> fits (w-1) (force x)
      | SText (s, x) -> fits (w-String.length s) (force x)
      | SLine (i, x) -> true
    in
    if fits (min (w-k) (r-k+n)) x then x else y
  in
  let rec best n k = function
    | [] -> SEmpty
    | (i,d)::ds ->
        match d with
        | Empty -> best n k ds
        | Line _ -> SLine (i, lazy (best i i ds))
        | Nest (j,x) -> best n k ((i+j,x)::ds)
        | Char c -> SChar (c, lazy (best n (k+1) ds))
        | Text s -> SText (s, lazy (best n (k+String.length s) ds))
        | Cat (x,y) -> best n k ((i,x)::(i,y)::ds)
        | Union (x,y) -> better n k (best n k ((i,x)::ds)) (best n k ((i,y)::ds))
        | Column f -> best n k ((i,f k)::ds)
        | Nesting f -> best n k ((i,f i)::ds)
        | MaxColumn w -> best n k ds (* ignore *)
  in
  let buf = Buffer.create 0 in
  let rec out = function
    | SEmpty ->
        Buffer.contents buf
    | SChar (c, x) ->
        Buffer.add_char buf c;
        out (force x)
    | SText (s, x) ->
 