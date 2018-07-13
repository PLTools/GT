module E = struct
  type ('a,'b,'c) expr = .. [@@deriving gt]
  (* TODO: support whildcards here *)

  (* let gcata _ _ _ = failwith "Not defined yet"
   * class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] expr_t = object end *)
end

module Lam = struct
  type ('name_abs, 'name, 'lam) E.expr += App of 'lam * 'lam | Var of 'name
  [@@deriving gt]

end
