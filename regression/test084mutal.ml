type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of int a | J | K of b
[@@deriving gt ~options:{show}]
