module T : sig
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare;;
  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare;;

  @type 'a t1 = 'a with show,gmap,foldl,eq,compare;;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare;;

  @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare;;
  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare;;
end = struct
  @type t2 = GT.int * GT.string with show,gmap,foldl,eq,compare;;
  @type 'a t3 = GT.int * 'a * GT.string with show,gmap,foldl,eq,compare;;

  @type 'a t1 = 'a with show,gmap,foldl,eq,compare;;

  @type bindings = (GT.string * GT.int) GT.list with show,gmap,foldl,eq,compare;;

  (* Let also test some standart types from GT library *)
  @type 'a u1 = 'a GT.option with show,gmap,foldl,eq,compare;;
  @type 'a u2 = 'a GT.Lazy.t with show,gmap,foldl,eq,compare;;
end
