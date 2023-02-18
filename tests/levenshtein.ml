let min a b =
  if a < b then a else b

let levenshtein a b =
  let m = String.length a
  and n = String.length b in
  let s = Array.make 2 [||] in
  s.(0) <- Array.make (n+1) 0;
  s.(1) <- Array.make (n+1) 0;
  for j = 1 to n do
    s.