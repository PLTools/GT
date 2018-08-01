open GT

let show_tuple2 f g (a,b) = Printf.sprintf "(%s,%s)" (f a) (g b)

type ('a, 'b) a = B of ('a, 'b) b | D of 'a
and  ('a, 'b) b = A of ('a, 'b) a | C of 'a * 'b | E
[@@deriving gt ~gmap ~show]

type 'a class_infos = {
  pci_virt: bool;
  (* pci_params: (core_type * Asttypes.variance) list ;
   * pci_name: string Asttypes.loc ; *)
  pci_expr: 'a;
  (* pci_loc: Location.t ;
   * pci_attributes: attributes *)
}

and class_type = int
and class_expr = string
and class_description = class_type class_infos
and class_declaration = class_expr class_infos
[@@deriving gt ~gmap ~show]

(*
type ('a, 'b) a = B of ('a, 'b) b | D of 'a (* | F of ('a, 'b) a *)
and  ('a, 'b) b = A of ('a * 'a, 'b) a | C of 'a * 'b | E
   doesn't work because of non-regualrity
*)

