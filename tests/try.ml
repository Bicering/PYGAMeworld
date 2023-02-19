exception A of int;;

try
  try 1/0; ()
  with A i -> output_int i
with Division_by_zero ->
  output_char 't'
;;
