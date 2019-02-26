type 'a s = SS of 'a
and t = int s
and u = float s
[@@deriving gt ~options:{show}]
