(* module M = My_metaquot
 *
 * let () = M.notify "hello from pp_mymetaquot" *)
let () =
  print_endline "Hello";
  failwith "shitty shit"

let () = Ppxlib.Driver.standalone ()
