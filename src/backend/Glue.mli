open AST
open Datatypes
open EVM
open Gen1
open Gen0
open Gen3
open Gen
open Gen2
open Gen5
open Gen6
open Gen4
open GlobalenvCompile
open Language
open Options

val full_compile_genv : genv -> (evm list, label) prod option
