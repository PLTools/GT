open Base
open HelpersBase
open Ppxlib
open Printf

let trait_name = "foldr"

module Make(AstHelpers : GTHELPERS_sig.S) = struct
open AstHelpers
module P = Foldl.Make(AstHelpers)

let trait_name =  trait_name

class g initial_args tdecls = object(self: 'self)
  inherit P.g initial_args tdecls as super

  method trait_name = trait_name

  method join_args ~loc do_typ ~init (xs: (string * core_type) list) =
    List.fold_left ~f:(fun acc (name,typ) ->
        Exp.app_list ~loc
          (do_typ typ)
          [ acc; Exp.sprintf ~loc "%s" name]
        )
        ~init
        (List.rev xs)

end

let g =
  (new g :>
     (Plugin_intf.plugin_args -> Ppxlib.type_declaration list ->
      (loc, Exp.t, Typ.t, type_arg, Ctf.t, Cf.t, Str.t, Sig.t) Plugin_intf.typ_g))

end

let register () =
  Expander.register_plugin trait_name (module Make: Plugin_intf.PluginRes)

let () = register ()
