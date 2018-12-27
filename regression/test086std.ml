(* The same as test 805 but in camlp5 syntax *)
module GT = Show_typed_api

module T : sig
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;

  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;

  @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;

  @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;
  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;

  @type 'a u3 = {aa: GT.int; bb:GT.string} with show,gmap,foldl,eq,compare,eval;;
end = struct
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful,html,show_typed;;

  let () = ();;

  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare,eval,stateful,html,show_typed;;

  let () = ();;
  @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful,show_typed,html;;

  let () = ();;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,html,foldl,eq,compare,eval,stateful,show_typed;;

  let () = ();;

  @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful,html,show_typed;;

  let () = ();;

  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;

  let () = ();;
  (* TODO: implement stateful for records *)
  @type 'a u3 = {aa: GT.int; bb:GT.string} with show,gmap,foldl,eq,compare,eval;;
  let () = ()
end
