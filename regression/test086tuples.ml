module T : sig
  @type t2 = GT.int * GT.string with show;;
  @type 'a t3 = GT.int * 'a * GT.string with show;;
  @type 'a t1 = 'a with show;;
end = struct
  @type t2 = GT.int * GT.string with show;;
  @type 'a t3 = GT.int * 'a * GT.string with show;;
  @type 'a t1 = 'a with show;;

  @type bindings = (GT.string * GT.int) GT.list with show
end
