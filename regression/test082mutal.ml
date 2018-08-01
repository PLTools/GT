
(* module T1 = struct
 *   @type ('a, 'b) a = B of ('a, 'b) b | D of 'a
 *   and ('a, 'b) c = ('a, 'b) b
 *   and ('a, 'b) b = A of ('a, 'b) a | C of 'a * 'b | E
 *   with show,gmap
 * end
 *
 * module T2 = struct
 * (\* leaving mutally recursive classes here strikes regularity restriction *\)
 *   @type mmm = GT.int
 *   and uuu = GT.bool
 *   and www = GT.string
 *   and 'a class_infos = 'a
 *   and class_description = GT.int class_infos
 *   and zzz = GT.char
 *   and class_declaration = GT.string class_infos
 *   with fmt
 * end *)

(*
type ('a, 'b) a = B of ('a, 'b) b | D of 'a (* | F of ('a, 'b) a *)
and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
   doesn't work because of non-regualrity
*)
open GT
module T3 = struct

  @type core_type = CT
  and class_type_declaration = class_type class_infos
  and class_type = XXX
  and class_signature = YYY
  and class_declaration = class_expr class_infos
  and 'a class_infos =
    {
      pci_params: (core_type) list ;
      pci_expr: 'a }
  and class_expr = ZZZ
    with fmt
end
