type lam =
  Var of GT.string
| Abs of GT.string * lam
| App of lam * lam

  [@@deriving gt ~plugins:{show}]


type value = Closure of env * lam
and          env   = (GT.string * value) GT.list
  [@@deriving gt ~plugins:{show}]