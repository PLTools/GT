open GT

(* TODO: remove this test *)
@type test = string with show, gmap, foldl, foldr, eq, compare

let _ =
  Printf.printf "%s\n" (transform(test) (new @test[show]) () "abc")
