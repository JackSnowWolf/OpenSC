open Datatypes
open EVM
open Options

type compiled = evm list option

val command_compiled : evm -> compiled

val error_compiled : compiled

val empty_compiled : compiled

val append_compiled : evm -> compiled -> compiled

val concatenate_compiled : compiled -> compiled -> compiled
