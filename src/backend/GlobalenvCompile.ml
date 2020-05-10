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

(** val allocate_addrs :
    ident list -> coq_Z -> coq_type PTree.t -> coq_Z PTree.t option **)

let rec allocate_addrs vars next_addr defs =
  match vars with
  | Coq_nil -> Some PTree.empty
  | Coq_cons (id, rest) ->
    bind (PTree.get id defs) (fun ty ->
      let size = sizeof_words ty in
      bind (allocate_addrs rest (Z.add next_addr size) defs)
        (fun allocated -> Some (PTree.set id next_addr allocated)))

(** val allocations : genv -> coq_Z PTree.t option **)

let allocations ge =
  allocate_addrs ge.genv_vars Z0 ge.genv_defs

(** val genv_compiled : genv -> compiled **)

let genv_compiled ge =
  bind (allocations ge) (fun allocated ->
    code_compiled ge.genv_main allocated ge.genv_main_entrypoint)

(** val get_main_entrypoint : genv -> label option **)

let get_main_entrypoint ge =
  Some ge.genv_main_entrypoint
