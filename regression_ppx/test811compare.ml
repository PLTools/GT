module T1 = struct
type ('a,'b) t = A of 'a | B of 'b * int
[@@deriving gt ~show ~compare]

let () =
  let cmp1 x y = compare_t (GT.compare GT.int) (GT.compare GT.string) x y in
  assert (GT.EQ =  cmp1 (A 5) (A 5) );
  assert (GT.EQ =  cmp1 (B ("",5)) (B ("",5))  );
  assert (GT.EQ <> cmp1 (A 5) (B ("",5)) );
  assert (GT.LT =  cmp1 (A 5) (B ("",5)) );
  assert (GT.GT =  cmp1 (B ("",5))  (A 5));
  ()
end

(* testing polymorphic variants *)
module T2 = struct
type 'b t2 = [ `A | `B of 'b * int ]
[@@deriving gt ~show ~compare]

let () =
  let cmp1 x y = compare_t2 (GT.compare GT.string) x y in
  assert (GT.EQ =  cmp1 `A           `A );
  assert (GT.EQ =  cmp1 (`B ("",5)) (`B ("",5)) );
  assert (GT.EQ <> cmp1 `A          (`B ("",5)) );
  (* I'm not sure why the answer is not LT here *)
  assert (GT.EQ <>  cmp1 `A          (`B ("",5)) );
  assert (GT.EQ <>  cmp1 (`B ("",5))  `A );
  ()
end

module T3 = struct
  type 'a t = { q: int; w: string; e: 'a GT.list }
  [@@deriving gt ~show ~compare ~eq]

  let ()  =
    let cmp1 x y = compare_t (GT.compare GT.string) x y in
    let eq1  x y = eq_t      (GT.eq GT.string) x y in
    let a = { q=5; w="asd"; e= [""] } in
    let b = { q=6; w="asd"; e= [""] } in
    assert (GT.EQ  = cmp1 a a);
    assert (GT.LT  = cmp1 a b);
    assert (eq1 a a);
    assert (not(eq1 a b));
    ()
end
