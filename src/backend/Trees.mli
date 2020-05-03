open BinNums
open Datatypes
open Globalenvs
open Maps0
open Options

val map_error : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list option

val xtransl_tree :
  ('a1 -> 'a2 option) -> (positive, 'a1) prod list -> 'a2 PTree.t option

val transl_tree : ('a1 -> 'a2 option) -> 'a1 PTree.t -> 'a2 PTree.t option

val partial_f : ('a1 -> 'a2 option) -> 'a1 option -> 'a2 option option

val transl_map :
  ('a1 -> 'a2 option) -> 'a1 option IntMap.t -> 'a2 option IntMap.t option

val xtransl_tree_keys_move :
  (positive -> 'a1 -> (positive, 'a2) prod option) -> (positive, 'a1) prod
  list -> 'a2 PTree.t option

val transl_tree_keys_move :
  (positive -> 'a1 -> (positive, 'a2) prod option) -> 'a1 PTree.t -> 'a2
  PTree.t option
