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

(** val full_compile_genv : genv -> (evm list, label) prod option **)

let full_compile_genv ge =
  bind (clike_genv ge) (fun clike ->
    bind (cgraph_genv clike) (fun cgraph ->
      bind (cbasic_genv cgraph) (fun cbasic ->
        bind (clinear_genv cbasic) (fun clinear ->
          bind (clabeled_program clinear) (fun clabeled ->
            bind (stacked_program clabeled) (fun stacked ->
              bind (expressionless_program stacked) (fun expressionless ->
                bind (methodical_genv expressionless) (fun methodical ->
                  bind (genv_compiled methodical) (fun program ->
                    bind (get_main_entrypoint methodical)
                      (fun main_entrypoint -> Some (Coq_pair (program,
                      main_entrypoint))))))))))))
