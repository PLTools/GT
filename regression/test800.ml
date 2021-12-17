
module PV: sig
  type a = A of b | C of GT.int    | E of a
  and  b = B of a | D of GT.string | F of b
  [@@deriving gt ~options:{show;gmap}]
end = struct
  type a = A of b | C of GT.int    | E of a
  and  b = B of a | D of GT.string | F of b
  [@@deriving gt ~options:{show;gmap}]
end

(*
TODO:
type 'a t = [> `Abs of GT.string * 'a ] as 'a
  [@@deriving gt ~options:{show;gmap}]
   *)


let _ = [%show: GT.int]

let _ = [%gmap: GT.int]

let _ = [%fmt: GT.int GT.list]

let () =
  Printf.printf "string %s and int %s\n" ([%show: GT.string] () "asdf") ([%show: GT.int] () 42);
  Format.printf "int list %a\n%!" [%fmt: GT.int GT.list] [0;1;2];
  (* Format.printf "string list %a\n%!" [%fmt: GT.int GT.list] ([%gmap: 'a GT.list] ((+)1) () [0;1;2]); *)
  ()
