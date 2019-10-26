type z = Z
type 'a s = S : 'prev -> 'prev s

type term = Term : 'a s -> term [@@deriving gt]
