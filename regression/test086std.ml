module GT = struct
  include GT
  include Show_typed_api
end

module T : sig
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;
  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;
end = struct
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type 'a t1 = 'a with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;

  @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare,eval,stateful,show_typed;;
  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare,eval,stateful;;
end


