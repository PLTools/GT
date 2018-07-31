type t = {a : GT.int; b: GT.string} [@@deriving gt ~options:{fmt}]
type t2 = (* RRR of {asdf:int} *)
        | QQQ of GT.string
 [@@deriving gt ~options:{fmt}]
let () =
  let open Format in
  fprintf std_formatter "%a\n"  t.GT.plugins#fmt {a=1; b="x"};
  (* fprintf std_formatter "%a\n" t2.GT.plugins#fmt (RRR {asdf=20}); *)
  fprintf std_formatter "%a\n" t2.GT.plugins#fmt (QQQ "azerty");
  ()
