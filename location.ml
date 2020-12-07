
let input_chan = ref stdin

let output_loc oc input seek (pos1,pos2) =
  let rec move_to pos topos line line_pos =
    if pos >= topos then
      pos, line, line_pos
    else
      try
        let c = input() in
        if c = '\n' then
          move_to (pos+1) topos (line+1) (pos+1)
        else
          move_to (pos+1) topos line line_pos
      with End_of_file ->
        pos+1, line, line_pos
  in
  let copy_line () =
    let c = ref ' ' in
    begin try
      while c := input(); !c != '\n' do output_char oc !c done
    with End_of_file ->
      ()
    end;
    output_char oc '\n'
  in
  let skip_line () =
    try
      while input() != '\n' do () done
    with End_of_file ->
      ()
  in
  let pr_line f l ch =
    let c = ref ' ' and f = ref f and l = ref l in