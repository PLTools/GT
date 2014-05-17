changequote([[,]])

define(GENERIFY, [[
type proto$1 = $1
type $1 = proto$1
                                                                          
class type show_$1_env_tt    = object  end                      
class type foldl_$1_env_tt   = object  end                      
class type foldr_$1_env_tt   = object  end                      
class type eq_$1_env_tt      = object  end                      
class type compare_$1_env_tt = object  end                      
class type map_$1_env_tt     = object  end                      
                                                                          
class type virtual ['inh, 'syn] $1_tt =                           
  object                                                                  
    method t_$1 : 'inh -> $1 -> 'syn                
  end                                                                     
                                                                          
class virtual ['inh, 'syn] $1_t =                                 
  object (this)                                                           
    method virtual t_$1 : 'inh -> $1 -> 'syn 
  end                                                                     
                                                                          
class show_proto_$1 env =                                         
  object (this)                                                           
    inherit [unit, string] @$1                                      
    method t_$1 inh x = string_of_$1 x                           
  end                                                                     
                                                                          
class show_$1_t =                                               
  let self = Obj.magic (ref ()) in                                        
  object (this)                                                           
    inherit [unit, string] @$1                                      
    inherit show_proto_$1 self                                    
    initializer (:=) self (this :> show_$1_t)                   
  end                                                                     
                                                                          
class map_proto_$1 env =                                          
  object (this)                                                           
    inherit [unit, $1] @$1                                    
    method t_$1 _ x = x                                                  
  end                                                                     
                                                                          
class map_$1_t =                                                
  let self = Obj.magic (ref ()) in                                        
  object (this)                                                           
    inherit [unit, $1] @$1                                    
    inherit map_proto_$1 self                                     
    initializer (:=) self (this :> map_$1_t)                    
  end                                                                     
                                                                          
class ['syn] foldl_proto_$1 env =                                 
  object (this)                                                           
    inherit ['syn, 'syn] @$1                                        
    method t_$1 s _ = s                                                  
  end                                                                     
                                                                          
class ['syn] foldl_$1_t =                                       
  let self = Obj.magic (ref ()) in                                        
  object (this)                                                           
    inherit ['syn, 'syn] @$1                                        
    inherit ['syn] foldl_proto_$1 self                            
    initializer (:=) self (this :> 'syn foldl_$1_t)             
  end                                                                     
                                                                          
class ['syn] foldr_proto_$1 env =                                 
  object (this)                                                           
    inherit ['syn, 'syn] @$1                                        
    method t_$1 s _ = s                                                  
  end                                                                     
                                                                          
class ['syn] foldr_$1_t =                                       
  let self = Obj.magic (ref ()) in                                        
  object (this)                                                           
    inherit ['syn, 'syn] @$1                                        
    inherit ['syn] foldr_proto_$1 self                            
    initializer (:=) self (this :> 'syn foldr_$1_t)             
  end                                                                     
                                                                          
type 't $1_tags = [`t of 't]                                      
type ('self, 't) $1_tags_open = 'self constraint 'self = [> 't $1_tags ]
                                                             
class ['t] eq_proto_$1 env =                                           
  object (this)                                                           
    inherit [('t, $1) $1_tags_open, bool] @$1                
    method t_$1 inh x = match inh with `t y -> x = y                     
  end                                                                     
                                                                          
class ['t] eq_$1_t =                                                 
  let self = Obj.magic (ref ()) in                                        
  object (this)                                                           
    inherit [('t, $1) $1_tags_open, bool] @$1                
    inherit ['t] eq_proto_$1 self                                      
    initializer (:=) self (this :> 't eq_$1_t)                     
  end                                                                     
                                                                          
class ['t] compare_proto_$1 env =                                      
  object (this)                                                           
    inherit [('t, $1) $1_tags_open, comparison] @$1              
    method t_$1 inh x = match inh with `t y -> compare_primitive y x     
  end                                                                     
                                                                          
class ['t] compare_$1_t =                                            
  let self = Obj.magic (ref ()) in                                        
  object (this)                                                           
    inherit [('t, $1) $1_tags_open, comparison] @$1              
    inherit ['t] compare_proto_$1 self                                
    initializer (:=) self (this :> 't compare_$1_t)                
  end                                                                             
                                                                                  
let $1 : (('inh, 'syn) # $1_tt -> 'inh -> $1 -> 'syn) t =  
  let $1_gcata t inh x = t#t_$1 inh x in                            
  {gcata = $1_gcata}
]])
