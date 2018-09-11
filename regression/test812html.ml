type t = { a : GT.int; b : GT.string }
[@@deriving gt ~options:{html}]

type 'a t2 = A of GT.int | C of 'a * GT.int
[@@deriving gt ~options:{html}]

type t3 = D of t | E of GT.int t2
[@@deriving gt ~options:{html}]

type t4 = GT.int t2
[@@deriving gt ~options:{html}]

let () =
  let ch = open_out "/tmp/out.html" in
  let fmt = Format.formatter_of_out_channel ch in
  let t1 = {a=5; b="beeeee"} in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  html_t t1;

  let t2 = A 5655 in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  html_t2 GT.float.GT.plugins#html t2;
  
  let t3 = C (3.1415, 888) in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  html_t2 GT.float.GT.plugins#html t3;

  let t4 = D t1 in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  html_t3 t4;

  let t5 = E (A 18) in
  Format.fprintf fmt "%s\n\n%!" @@ HTML.toHTML @@
  html_t3 t5;

  Format.pp_force_newline fmt ();
  close_out ch
