let check b =
  output_char (if b then '.' else 'x')

let (@@) f x = f x;;
let (+:) x y = 2*x+y;;

check (3 +: 4 = 10);
check (1+2*3/4 = 2);
(*check (3