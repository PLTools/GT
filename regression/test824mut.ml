type t = HEmpty
[@@deriving gt (* ~options:{ show } *)]

and heap = t [@@deriving gt (* ~options:{ show } *)]
