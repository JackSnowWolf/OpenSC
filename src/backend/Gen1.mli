open Datatypes
open Globalenvs
open Language3
open Language1
open Language2
open Maps0
open Options
open Trees

val cbasic_stm : bool option -> node -> Language1.statement -> bblock

val cbasic_code : bool option -> Language1.code -> Language3.code

val cbasic_function :
  bool option -> Language1.coq_function -> Language3.coq_function

val cbasic_fundef :
  bool option -> Language1.coq_function -> Language3.coq_function option

val cbasic_fundefs :
  Language1.coq_function PTree.t -> Language3.coq_function PTree.t option

val cbasic_methoddefs :
  Language1.coq_function option IntMap.t -> Language3.coq_function option
  IntMap.t option

val cbasic_constructor :
  Language1.coq_function option -> Language3.coq_function option

val cbasic_genv : Language1.genv -> Language3.genv option
