open BinNums
open BinPos
open Coqlib
open Ctypes
open Datatypes
open Globalenvs
open Language1
open Language
open Maps0
open Options
open Specif
open Trees

type state = { st_nextnode : positive; st_code : code }

val st_nextnode : state -> positive

val st_code : state -> code

val init_state : state

type 'a res =
| Error
| OK of 'a * state

type 'a mon = state -> 'a res

val ret : 'a1 -> 'a1 mon

val error : 'a1 mon

val bind : 'a1 mon -> ('a1 -> 'a2 mon) -> 'a2 mon

val add_instr : Language1.statement -> node mon

val reserve_instr : node mon

val check_empty_node : state -> node -> sumbool

val update_instr : node -> Language1.statement -> coq_unit mon

val cgraph_statement :
  statement -> node -> node -> node -> node option -> node mon

val cgraph_function : coq_function -> Language1.coq_function option

val empty_constructor : coq_function

val cgraph_constructor :
  coq_function option -> Language1.coq_function option option

val cgraph_functions :
  coq_function PTree.t -> Language1.coq_function PTree.t option

val cgraph_methoddefs :
  coq_function option IntMap.t -> Language1.coq_function option IntMap.t
  option

val cgraph_genv : genv -> Language1.genv option
