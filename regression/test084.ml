let id x = x

module PV : sig
  @type ('a,'b)  pv = [ `A of 'a | `B of 'b * GT.int ] GT.list with show
end = struct
  @type ('a,'b)  pv = [ `A of 'a | `B of 'b * GT.int ] GT.list with show
end

let () =
  print_endline @@ (GT.show PV.pv (GT.show GT.int) (GT.show GT.int)) [`A 5]