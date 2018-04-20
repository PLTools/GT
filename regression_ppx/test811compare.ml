type ('a,'b) t = A of 'a | B of 'b * int
[@@deriving gt ~show ~compare]

let () =
  let cmp1 x y = compare_t (GT.compare GT.int) (GT.compare GT.string) x y in
  assert (GT.EQ =  cmp1 (A 5) (A 5));
  assert (GT.EQ <> cmp1 (A 5) (B ("asdf",5)) );
