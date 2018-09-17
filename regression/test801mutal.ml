open Printf

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of GT.int a | J | K of b
[@@deriving gt ~options:{show;gmap;fmt}]

class ['self_b] show_b_hack for_show_a  for_show_b  fself = object
  inherit ['self_b] show_b_t_stub for_show_a  for_show_b  fself
  method c_I inh___037_ _x__038_ =
    Printf.sprintf "I{%s}"
      (for_show_a.show_a_trf (GT.int.GT.plugins)#show _x__038_)
  method c_K () x = Printf.sprintf "K {%s}" (fself x)
end

let show_b2 subj =
  let { show_b } = show_fix_a ~b0:({ show_b_func = new show_b_hack }) () in
  show_b.show_b_trf subj

let _ =
  printf "Testing Show\n";
  printf "%s\n" @@ show_a (GT.show GT.int) (E C);
  printf "%s\n" @@ show_a (GT.show GT.int) (A (I C));
  printf "%s\n" @@ show_b                  (I (A J));
  printf "%s\n" @@ show_b                  (K J);
  printf "Testing Show with fixed b\n";
  printf "%s\n" @@ show_b2                 (I (A J));
  printf "%s\n" @@ show_b2                 (K J);

  printf "Testing Gmap\n";
  printf "%s\n" @@ show_a (GT.show GT.int) @@ gmap_a ((+)1) (D 6);

  ()

(* let b =
 *   {
 *     GT.gcata = gcata_b;
 *     GT.plugins = (object method gmap = gmap_b method show = show_b end)
 *   }
 * let a =
 *   {
 *     GT.gcata = gcata_a;
 *     GT.plugins = (object method gmap = gmap_a method show = show_a end)
 *   } *)

(* ************************************ *)
(* type c = b GT.list [@@deriving gt ~options:{show;gmap}]
 * type 'a d = 'a GT.list a GT.list  [@@deriving gt ~options:{show;gmap}] *)


(* type ctd = class_type ci
 * and class_type = C
 * and 'a ci = 'a
 * [@@deriving gt ~options:{fmt}] *)

type class_declaration = class_expr class_infos
and 'a class_infos =
  {
  pci_virt: GT.string ;
  pci_expr: 'a ;
}
and class_expr =
  {
  pcl_desc: class_expr_desc;
 }
and class_expr_desc =
  | CE
(* [@@deriving gt ~options:{fmt}] *)
