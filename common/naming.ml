open Base
open Printf

let self_arg_name = "fself"
let gcata_name_for_typ name = Printf.sprintf "gcata_%s" name
let class_name_for_typ name = Printf.sprintf "%s_t" name
let trait_class_name_for_typ ~trait name =
  class_name_for_typ (if String.equal trait ""
                      then name
                      else Printf.sprintf "%s_%s" trait name)
let meth_name_for_constructor = Printf.sprintf "c_%s"

let fix_name ~plugin_name = sprintf "%s_fix"
let typ1_for_class_arg ~plugin_name = sprintf "%s_t_%s_1" plugin_name
let typ2_for_class_arg ~plugin_name = sprintf "%s_t_%s_2" plugin_name
let typ3_for_class_arg ~plugin_name = sprintf "%s_t_%s_3" plugin_name
