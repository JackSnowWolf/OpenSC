open AST
open Datatypes
open Globalenvs
open Language5
open Language0
open Maps0
open MemoryModel
open Options
open Semantics0
open Trees
open Values

val expressionless_expr : expr -> Language5.statement

val pops : nat -> Language5.statement list

val cleanup : nat -> Language5.statement list

val expressionless_rt : ret_type -> Language5.ret_type option

val fetch_args : nat -> nat -> Language5.statement list

val extract_lbl : typed_label -> label

val expressionless_stm : statement -> Language5.statement list option

val expressionless_code : code -> Language5.code option

val expressionless_function : coq_function -> Language5.coq_function option

val expressionless_fundefs :
  coq_function PTree.t -> Language5.coq_function PTree.t option

val expressionless_methods :
  coq_function option IntMap.t -> Language5.coq_function option IntMap.t
  option

val expressionless_constructor :
  coq_function option -> Language5.coq_function option

val expressionless_genv : genv -> Language5.genv option

val expressionless_program : program -> Language5.program option
