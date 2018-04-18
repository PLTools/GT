type 'info expr_node =
  | EConst of GT.int
  | EAdd of 'info expr * 'info expr
and 'info expr =
  { info : 'info ; node : 'info expr_node }
  [@@deriving gt ~show ]
