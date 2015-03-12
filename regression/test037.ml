@type 'a t1 = [`A | `B of 'a] with show, map
@type 'a t2 = [`C | `D of 'a] with show, map
@type 'a t  = ['a t1 | 'a t2] with show, map

let _ = 
  let a = `B (`B `A) in
  let rec mapt1 x = GT.transform(t1) (fun _ -> mapt1) new @t1[map] () x in
  let rec show1 x = GT.transform(t1) (fun _ -> show1) new @t1[show] () x in
  Printf.printf "a=%s, map a=%s\n" (show1 a) (show1 (mapt1 a));
  let b = `D (`D `C) in
  let rec mapt2 x = GT.transform(t2) (fun _ -> mapt2) new @t2[map] () x in
  let rec show2 x = GT.transform(t2) (fun _ -> show2) new @t2[show] () x in
  Printf.printf "b=%s, map b=%s\n" (show2 b) (show2 (mapt2 b));
  let c = `D (`B (`D `A)) in
  let rec mapt x = GT.transform(t) (fun _ -> mapt) new @t[map] () x in
  let rec show x = GT.transform(t) (fun _ -> show) new @t[show] () x in
  Printf.printf "c=%s, map c=%s\n" (show c) (show (mapt c))
