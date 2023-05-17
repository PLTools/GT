@type ('a, 'b) t = A of 'a GT.list * 'b GT.list with show, gmap

let _ =
  let x = A ([1; 2; 3], ["4"; "5"; "6"]) in
  let y = GT.gmap t string_of_int int_of_string x in
  Printf.printf "%s\n" @@ GT.show t string_of_int Fun.id x;
  Printf.printf "%s\n" @@ GT.show t Fun.id string_of_int y
