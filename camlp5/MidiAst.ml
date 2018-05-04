type 'ctyp typ  =
  Arbitrary of 'ctyp
| Variable  of 'ctyp * string
| Self      of 'ctyp * string list * string list
| Tuple     of 'ctyp * 'ctyp typ list
| Instance  of 'ctyp * 'ctyp typ list * string list
