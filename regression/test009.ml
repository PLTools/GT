@type ('a, 'b) a = A of 'a | B of 'b with show, gmap
@type ('a, 'b) t = X of ('a, 'b) a * ('a, 'b) t GT.list | 
                   Y of ('b, 'a) a * ('a, 'b) t GT.list with show, gmap

let _ =
  let x = X (A 1, [Y (A "2", []); X (A 2, []); Y (A "3", [])]) in
  let y = gmap_t string_of_int int_of_string x in
  Printf.printf "%s\n" @@ show_t string_of_int (fun x -> x) x;
  Printf.printf "%s\n" @@ show_t (fun x -> x) string_of_int y
