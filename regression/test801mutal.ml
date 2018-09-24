open Printf

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of GT.int a | J | K of b
and   all = (GT.int a) GT.list
[@@deriving gt ~options:{show;gmap}]

class ['self_b] show_b_hack prereq  fself = object
  inherit ['self_b] show_b_t_stub prereq fself
  method c_I inh___037_ _x__038_ =
    Printf.sprintf "I {%s}"
      (prereq.show_a.show_a_trf (GT.int.GT.plugins)#show _x__038_)
  method c_K () x = Printf.sprintf "K {%s}" (fself x)
end

let show_all2, show_b2 =
  let { show_b; show_all } = show_fix_a ~b0:({ show_b_func = new show_b_hack }) () in
  (show_all.show_all_trf, show_b.show_b_trf )

let show_b2 subj =
  let { show_b } = show_fix_a ~b0:({ show_b_func = new show_b_hack }) () in
  show_b.show_b_trf subj

let _ =
  printf "Testing show_a\n";
  printf "%s\n" @@ show_a (GT.show GT.int) (E C);
  printf "%s\n" @@ show_a (GT.show GT.int) (A (I C));
  printf "Testing show_b\n";
  printf "%s\n" @@ show_b                  (I (A J));
  printf "%s\n" @@ show_b                  (K J);
  printf "Testing show_b with fixed b\n";
  printf "%s\n" @@ show_b2                 (I (A J));
  printf "%s\n" @@ show_b2                 (K J);

  printf "Testing gmap_a\n";
  printf "%s\n" @@ show_a (GT.show GT.int) @@ gmap_a ((+)1) (D 6);
  printf "Testing show_all with fixed b\n";
  printf "%s\n" @@ show_all                  [A(K J)];

  ()

