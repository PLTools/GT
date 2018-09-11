module Pervasives = struct include Pervasives let ref = GT.ref end

open GT

(* let (_:int) = GT.ref *)
(* module Pervasives = struct
 *   include Pervasives
 *     (\* type 'a array = 'a GT.array *\)
 * 
 * 
 * end *)

  let array =
    { GT.gcata = (fun _ _ -> failwith "arrays not implemented")
    ; GT.plugins = object
        method fmt _fa fmt s = Format.fprintf fmt "<array>%!"
        method html _fa s = HTML.string "array HERE"
      end
    }

module Location = Camlast.Location
module Longident = struct
  type t = [%import: Longident.t] [@@deriving gt ~options:{ fmt; html }]
end

module Asttypes = Camlast.Asttypes

module Ident = struct
  type t = [%import: Ident.t]  [@@deriving gt ~options:{ fmt; html }]
end
module Path = struct
  type t = [%import: Path.t]  [@@deriving gt ~options:{ fmt; html }]
end

module Primitive = struct
  type boxed_integer = [%import: Primitive.boxed_integer] [@@deriving gt ~options:{ fmt; html }]
  type native_repr   = [%import: Primitive.native_repr]   [@@deriving gt ~options:{ fmt; html }]
  type description   = [%import: Primitive.description]   [@@deriving gt ~options:{ fmt; html }]
end

module Types = struct


  type type_expr        = [%import: Types.type_expr]  
  and  row_desc         = [%import: Types.row_desc]   
  and  type_desc        = [%import: Types.type_desc]  
  and  row_field        = [%import: Types.row_field]  
  and  abbrev_memo      = [%import: Types.abbrev_memo]
  and  field_kind       = [%import: Types.field_kind] 
  and  commutable       = [%import: Types.commutable]
  [@@deriving gt ~options:{ fmt; html }]

  module Parsetree = Camlast
  module Meths = struct
    type 'a t = [%import: 'a Types.Meths.t] 
    let t =
      { GT.gcata = (fun _ _ -> failwith "meths not implemented")
      ; GT.plugins = object
          method fmt _fa fmt s = Format.fprintf fmt "<meths>%!"
          method html _fa s = HTML.string "meths HERE"
        end
      }
  end
  module Vars = struct
    type 'a t = [%import: 'a Types.Vars.t] 
    let t =
      { GT.gcata = (fun _ _ -> failwith "vars not implemented")
      ; GT.plugins = object
          method fmt _fa fmt s = Format.fprintf fmt "<vars>%!"
          method html _fa s = HTML.string "vars HERE"
        end
      }
  end

  type value_description  = [%import: Types.value_description]
  and  value_kind         = [%import: Types.value_kind]
  [@@deriving gt ~options:{ fmt; html }]

  module Variance = struct
    type t = Types.Variance.t
    let t =
      { GT.gcata = (fun _ _ -> assert false)
      ; GT.plugins = object
          method html _ = HTML.string "Types.variance"
          method fmt fmt _ = Format.fprintf fmt "'some variance'"
        end
      }
  end
  type type_declaration = [%import: Types.type_declaration]
  and type_kind = [%import: Types.type_kind]
  and record_representation = [%import: Types.record_representation]
  and label_declaration = [%import: Types.label_declaration]
  and constructor_declaration = [%import: Types.constructor_declaration]
  and constructor_arguments = [%import: Types.constructor_arguments]
  and unboxed_status = [%import: Types.unboxed_status]
  [@@deriving gt ~options:{ fmt; html }]

  type extension_constructor = [%import: Types.extension_constructor]
  and type_transparence = [%import: Types.type_transparence]
  [@@deriving gt ~options:{ fmt; html }]

  module Concr = struct
    type t = [%import: Types.Concr.t] 
    let t =
      { GT.gcata = (fun _ _ -> failwith "concr not implemented")
      ; GT.plugins = object
          method fmt fmt s = Format.fprintf fmt "<concr>%!"
          method html s = HTML.string "concr HERE"
        end
      }
  end

  type class_type = [%import: Types.class_type]
  and class_signature = [%import: Types.class_signature]
  [@@deriving gt ~options:{ fmt; html }]

  type class_declaration = [%import: Types.class_declaration]
  [@@deriving gt ~options:{ fmt; html }]
  type class_type_declaration = [%import: Types.class_type_declaration]
  [@@deriving gt ~options:{ fmt; html }]

  type module_type = [%import: Types.module_type]
  and alias_presence = [%import: Types.alias_presence]
  and signature = [%import: Types.signature]
  and signature_item = [%import: Types.signature_item]
  and module_declaration = [%import: Types.module_declaration]
  and modtype_declaration = [%import: Types.modtype_declaration]
  and rec_status = [%import: Types.rec_status]
  and ext_status = [%import: Types.ext_status]
  [@@deriving gt ~options:{ fmt; html }]

  (* type value_description = [%import: Types.value_description] [@@deriving gt ~options:{ fmt; html }]
   * and value_kind   = [%import: Types.value_kind] [@@deriving gt ~options:{ fmt; html }] *)
  type constructor_description = [%import: Types.constructor_description]
  and constructor_tag = [%import: Types.constructor_tag]
  [@@deriving gt ~options:{ fmt; html }]

  (* let (_:int) = Pervasives.array *)
  type label_description = [%import: Types.label_description]
  [@@deriving gt ~options:{ fmt; html }]

end

module Env = struct
  type t = [%import: Env.t]
  let t  =
      { GT.gcata = (fun _ _ -> failwith "env not implemented")
      ; GT.plugins = object
          method fmt fmt s = Format.fprintf fmt "<env>%!"
          method html s = HTML.string "env HERE"
        end
      }
end

module Parsetree = Camlast
open Parsetree
open GT

type partial = [%import: Typedtree.partial]   [@@deriving gt ~options:{ fmt; html }]
type attribute = [%import: Camlast.attribute]   [@@deriving gt ~options:{ fmt; html }]
type attributes = attribute GT.list   [@@deriving gt ~options:{ fmt; html }]


type pattern = [%import: Typedtree.pattern]
and pat_extra = [%import: Typedtree.pat_extra]
and pattern_desc = [%import: Typedtree.pattern_desc]
and expression = [%import: Typedtree.expression]
and exp_extra = [%import: Typedtree.exp_extra]
and expression_desc = [%import: Typedtree.expression_desc]
and meth = [%import: Typedtree.meth]
and case = [%import: Typedtree.case]
and record_label_definition = [%import: Typedtree.record_label_definition]
and class_expr = [%import: Typedtree.class_expr]
and class_expr_desc = [%import: Typedtree.class_expr_desc]
and class_structure = [%import: Typedtree.class_structure]
and class_field = [%import: Typedtree.class_field]
and class_field_kind = [%import: Typedtree.class_field_kind]
and class_field_desc = [%import: Typedtree.class_field_desc]
and module_expr = [%import: Typedtree.module_expr]
and module_type_constraint = [%import: Typedtree.module_type_constraint]
and module_expr_desc = [%import: Typedtree.module_expr_desc]
and structure = [%import: Typedtree.structure]
and structure_item = [%import: Typedtree.structure_item]
and structure_item_desc = [%import: Typedtree.structure_item_desc]
and module_binding = [%import: Typedtree.module_binding]
and value_binding = [%import: Typedtree.value_binding]
and module_coercion = [%import: Typedtree.module_coercion]
and module_type = [%import: Typedtree.module_type]
and module_type_desc = [%import: Typedtree.module_type_desc]
and primitive_coercion = [%import: Typedtree.primitive_coercion]
and signature = [%import: Typedtree.signature]
and signature_item = [%import: Typedtree.signature_item]
and signature_item_desc = [%import: Typedtree.signature_item_desc]
and module_declaration = [%import: Typedtree.module_declaration]
and module_type_declaration = [%import: Typedtree.module_type_declaration]
and open_description = [%import: Typedtree.open_description]
and 'a include_infos = [%import: 'a Typedtree.include_infos]
and include_description = [%import: Typedtree.include_description]
and include_declaration = [%import: Typedtree.include_declaration]
and with_constraint = [%import: Typedtree.with_constraint]
and core_type = [%import: Typedtree.core_type]
and core_type_desc = [%import: Typedtree.core_type_desc]
and package_type = [%import: Typedtree.package_type]
and row_field = [%import: Typedtree.row_field]
(* and row_field_desc = [%import: Typedtree.row_field_desc] *)
and object_field = [%import: Typedtree.object_field]
(* and object_field_desc = [%import: Typedtree.object_field_desc] *)
and value_description = [%import: Typedtree.value_description]
and type_declaration = [%import: Typedtree.type_declaration]
and type_kind = [%import: Typedtree.type_kind]
and label_declaration = [%import: Typedtree.label_declaration]
and constructor_declaration = [%import: Typedtree.constructor_declaration]
and constructor_arguments = [%import: Typedtree.constructor_arguments]
and type_extension = [%import: Typedtree.type_extension]
(* and type_exception = [%import: Typedtree.type_exception] *)
and extension_constructor = [%import: Typedtree.extension_constructor]
and extension_constructor_kind = [%import: Typedtree.extension_constructor_kind]
and class_type = [%import: Typedtree.class_type]
and class_type_desc = [%import: Typedtree.class_type_desc]
and class_signature = [%import: Typedtree.class_signature]
and class_type_field = [%import: Typedtree.class_type_field]
and class_type_field_desc = [%import: Typedtree.class_type_field_desc]
and class_declaration = [%import: Typedtree.class_declaration]
and class_description = [%import: Typedtree.class_description]
and class_type_declaration = [%import: Typedtree.class_type_declaration]
and 'a class_infos = [%import: 'a Typedtree.class_infos]
[@@deriving gt ~options:{ fmt; html }]

