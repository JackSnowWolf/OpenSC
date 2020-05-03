open Datatypes
open EVM
open Options

type compiled = evm list option

(** val command_compiled : evm -> compiled **)

let command_compiled cmd =
  Some (Coq_cons (cmd, Coq_nil))

(** val error_compiled : compiled **)

let error_compiled =
  None

(** val empty_compiled : compiled **)

let empty_compiled =
  Some Coq_nil

(** val append_compiled : evm -> compiled -> compiled **)

let append_compiled cmd rest =
  bind rest (fun code -> Some (Coq_cons (cmd, code)))

(** val concatenate_compiled : compiled -> compiled -> compiled **)

let concatenate_compiled chunk1 chunk2 =
  bind chunk1 (fun code1 -> bind chunk2 (fun code2 -> Some (app code2 code1)))
