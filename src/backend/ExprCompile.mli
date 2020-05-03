open AST
open BinNums
open Compiled
open Cop
open Datatypes
open EVM
open Integers
open MachineModel
open Maps0
open PeanoNat

val dup_ident : nat -> compiled

val global_address : coq_Z PTree.t -> ident -> compiled

val sha_base : Int256.int

val sha_arg2 : Int256.int

val sha_size2 : Int256.int

val sha_2_compiled : compiled

val binop_compiled : binary_operation -> bool -> compiled

val unop_compiled : unary_operation -> compiled

val builtin0_compiled : builtin0 -> evm

val builtin1_compiled : builtin1 -> evm
