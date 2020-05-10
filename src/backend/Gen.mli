open AST
open BinInt
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open Language
open Maps0
open Options
open Trees

val int_t : coq_type

val struct_lvalue : expr -> coq_type -> ident -> expr option

val array_lvalue : expr -> expr -> expr option

val hash_lvalue : expr -> expr -> expr option

val clike_rvalue : expr -> expr option

val clike_lvalue : expr -> expr option

val clike_rvalue_list : expr list -> expr list option

val clike_optvalue : expr option -> expr option option

val clike_stm : statement -> statement option

val clike_function : coq_function -> coq_function option

val clike_constructor : coq_function option -> coq_function option option

val clike_functions : coq_function PTree.t -> coq_function PTree.t option

val clike_methoddefs :
  coq_function option IntMap.t -> coq_function option IntMap.t option

val clike_genv : genv -> genv option
