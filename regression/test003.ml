@type a = A of b | C of GT.int GT.list with show
and   b = B of a | D of GT.string with show

(* let assf _ = assert false
 * class show_a' =
 *   object(this)
 *     inherit [_] @a[show] assf as super
 *     method c_C () a ys = "new " ^ super#c_C () a ys
 *   end *)

(* let _ =
 *   let x = A (B (C [1; 2; 3; 4])) in
 *   let y = B (A (D "3")) in
 *   Printf.printf "%s\n" (GT.transform(a) (new @a[show] assf) () x);
 *   Printf.printf "%s\n" (GT.transform(b) (new @b[show] assf) () y);
 *   Printf.printf "%s\n" (GT.transform(a) (new show_a'      ) () x); *)
