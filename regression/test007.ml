@type ('a, 'b) t = A of 'a GT.list * 'b GT.list with show, gmap

let _ =
  let x = A ([1; 2; 3], ["4"; "5"; "6"]) in
  let y = GT.gmap t (GT.lift string_of_int) (GT.lift int_of_string) x in
  Printf.printf "%s\n" @@ GT.show t (GT.lift string_of_int) GT.(lift id) x;
  Printf.printf "%s\n" @@ GT.show t GT.(lift id) (GT.lift string_of_int) y
