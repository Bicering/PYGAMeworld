module List = struct
  include List
  let zip xs ys =
    let rec go acc = function
      | [], _ | _, [