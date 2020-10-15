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
      (match List.rev ys with
      | y::ys ->
          Some (y, (xs,ys))
      | _ -> assert false
      )
  | xs, y::ys ->
      Some (y, (xs,ys))

type doc =
  | Nil
  | Line of int
  | Char of char
  | Text of string
  | Cat of doc * doc
  | Group of doc
  | Nest of int * doc
  | Align of int * doc

let empty = Nil
let line = Line 1
let linebreak = Line 0
let char c = Char c
let text t = Text t
let (<.>) x y = Cat (x, y)
let group x = Group x
let nest i x = Nest (i, x)
let align x = Align (0, x)

let space = Char ' '
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
let int i = Text (string_of_int i)

let (<+>) x y = x <.> text " " <.> y
let (<$>) x y = x <.> line <.> y
let (<$$>) x y = x <.> linebreak <.> y
let (</>) x y = x <.> softline <.> y
let (<//>) x y = x <.> softbreak <.> y
let enclose l r x = l <.> x <.> r
let fill_sep = List.fold_left (</>) empty

let rec sep_by sep = function
  | [] -> empty
  | x::xs ->
    let rec go acc = function
      | [] -> acc
      | x::xs -> go (acc <.> sep <.> x) xs
    in
    align (go x xs)

let rec enclose_sep l r sep = function
  | [] -> l <.> r
  | x::xs ->
    let rec go acc = function
      | [] -> acc
      | x::xs -> go (acc <.> sep <.> x) xs
    in
    align (go (l <.> x) xs <.> r) <.> Align (-10, empty)

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

let rec len = function
  | Nil -> 0
  | Char _ -> 1
  | Text t -> String.length t
  | Cat (x, y) -> len x + len y
  | _ -> assert false

let normalize x =
  (* ensure y and (fst result) only built from text,nil and Cat *)
  let rec go x y =
    match x with
    | Nil -> y, Nil
    | Line _ -> Nil, x <.> y
    | Char _ | Text _ -> x <.> y, Nil
    | Cat (u,v) ->
        let l2,r2 = go v y in
        let l1,r1 = go u l2 in
        l1, r1 <.> r2
    | Group u ->
        let l,r = go u y