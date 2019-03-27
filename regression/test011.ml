(* @type 'a option = Some of 'a | None
 *   with show,html,gmap,fmt,eval,stateful,foldl,foldr,compare,eq *)


(* @type ('a,'b,'c) triple = Triple of 'a*'b*'c
 *   with foldr,foldl,eq,compare,stateful,eval,gmap,html,fmt,show *)
let () = () ;;

@type ('a) t = Lazy of 'a
  with foldr,foldl,eq,compare,stateful,eval,gmap,html,fmt,show
