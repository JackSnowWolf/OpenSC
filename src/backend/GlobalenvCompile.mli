open AST
open BinInt
open BinNums
open Compiled
open Ctypes
open Datatypes
open Language6
open Maps0
open Options
open StmCompile

val allocate_addrs :
  ident list -> coq_Z -> coq_type PTree.t -> coq_Z PTree.t option

val allocations : genv -> coq_Z PTree.t option

val genv_compiled : genv -> compiled

val get_main_entrypoint : genv -> label option
