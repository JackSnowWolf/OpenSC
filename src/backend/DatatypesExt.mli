(*
this module defines extensions to the generated Datatypes module
 *)

module D = Datatypes

val eval_nat : D.nat -> int
val caml_list : 'a D.list -> 'a list
val caml_prod : ('a, 'b) D.prod -> 'a * 'b 
