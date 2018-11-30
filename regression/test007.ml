@type ('a, 'b) t = A of 'a GT.list * 'b GT.list with show, gmap

let _ =
  let x = A ([1; 2; 3], ["4"; "5"; "6"]) in
  let y = gmap_t string_of_int int_of_string x in
  Printf.printf "%s\n" @@ show_t string_of_int (fun x -> x) x;
  Printf.printf "%s\n" @@ show_t (fun x -> x) string_of_int y
