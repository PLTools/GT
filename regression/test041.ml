@type ('self) t =
   (* | App of 'self * 'self *)
   | Abs of 'self
 with show
