open AST
open Cop
open Datatypes
open Globalenvs
open Integers
open Language5
open Language6
open List0
open Maps0
open Options
open Semantics1
open Values
open Zpower

val methodical_fundefs : coq_function PTree.t -> code

val methodical_opt_function : coq_function option -> code

val methodical_methods : coq_function option IntMap.t -> code

val label_method_starts_with : coq_function -> label option

val sg_val : Int.int -> coq_val

val methodical_multiplexer_body :
  Int.int list -> coq_function option IntMap.t -> code option

val methodical_main : program -> code option

val methodical_genv : program -> genv option
