open AST
open BinNums
open Compiled
open Datatypes
open EVM
open ExprCompile
open Integers
open Language5
open Maps0
open MemoryModel
open Nat0
open PeanoNat
open Values

(** val assign_stack_compiled : nat -> compiled **)

let assign_stack_compiled required_index =
  match Nat.leb required_index (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S O)))))))))))))))) with
  | Coq_true -> command_compiled (Coq_evm_swap required_index)
  | Coq_false -> error_compiled

(** val push_public_args : nat -> nat -> nat -> Int.int -> compiled **)

let rec push_public_args args first_arg_index retval_count funsig =
  match args with
  | O ->
    append_compiled Coq_evm_mstore
      (append_compiled (Coq_evm_push
        (Int256.repr (public_funsig_pos retval_count)))
        (command_compiled (Coq_evm_push (funsig_aligned funsig))))
  | S n ->
    let rest_compiled =
      push_public_args n (add first_arg_index (S O)) retval_count funsig
    in
    let arg_pos = Coq_evm_push
      (Int256.repr (public_arg_pos first_arg_index retval_count))
    in
    append_compiled Coq_evm_mstore (append_compiled arg_pos rest_compiled)

(** val code_return : label -> compiled **)

let code_return code_label =
  append_compiled Coq_evm_return
    (append_compiled (Coq_evm_push Int256.zero)
      (append_compiled Coq_evm_codesize
        (append_compiled Coq_evm_codecopy
          (append_compiled (Coq_evm_push Int256.zero)
            (append_compiled (Coq_evm_push_label code_label)
              (command_compiled Coq_evm_codesize))))))

(** val cleanup : ret_type -> label -> compiled **)

let cleanup rv code_label =
  match rv with
  | Tvoid_method ->
    append_compiled Coq_evm_return
      (append_compiled (Coq_evm_push Int256.zero)
        (command_compiled (Coq_evm_push Int256.zero)))
  | Tconstructor -> code_return code_label
  | Tfun ->
    append_compiled Coq_evm_jump (command_compiled (Coq_evm_swap (S O)))
  | Tsome_method ->
    append_compiled Coq_evm_return
      (append_compiled (Coq_evm_push Int256.zero)
        (append_compiled (Coq_evm_push
          (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
            Coq_xH))))))))
          (append_compiled Coq_evm_mstore
            (command_compiled (Coq_evm_push Int256.zero)))))

(** val stm_compiled : statement -> coq_Z PTree.t -> label -> compiled **)

let stm_compiled stm ge code_label =
  match stm with
  | Spush s ->
    (match s with
     | Coq_inl v ->
       (match v with
        | Vunit -> command_compiled (Coq_evm_push Int256.zero)
        | Vint i -> command_compiled (Coq_evm_push i)
        | Vptr i ->
          (match i with
           | Iident id -> global_address ge id
           | Ihash (_, _) -> error_compiled)
        | _ -> error_compiled)
     | Coq_inr l -> command_compiled (Coq_evm_push_label l))
  | Sdup id -> dup_ident id
  | Ssload -> command_compiled Coq_evm_sload
  | Sunop op -> unop_compiled op
  | Sbinop (op, sgn) -> binop_compiled op sgn
  | Scall0 b -> command_compiled (builtin0_compiled b)
  | Scall1 b -> command_compiled (builtin1_compiled b)
  | Sskip -> empty_compiled
  | Spop -> command_compiled Coq_evm_pop
  | Ssstore -> command_compiled Coq_evm_sstore
  | Sswap lv -> assign_stack_compiled (S lv)
  | Sdone rv -> cleanup rv code_label
  | Slabel l -> command_compiled (Coq_evm_label l)
  | Sjump -> command_compiled Coq_evm_jump
  | Sjumpi -> command_compiled Coq_evm_jumpi
  | Stransfer -> command_compiled Coq_evm_call
  | Scallmethod (sg, args, rvcount) ->
    append_compiled Coq_evm_call (push_public_args args O rvcount sg)
  | Slog ->
    append_compiled Coq_evm_log
      (append_compiled (Coq_evm_push (Int256.repr Z0))
        (append_compiled (Coq_evm_push
          (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
            Coq_xH))))))))
          (append_compiled Coq_evm_mstore
            (command_compiled (Coq_evm_push (Int256.repr Z0))))))
  | Srevert ->
    append_compiled Coq_evm_revert
      (append_compiled (Coq_evm_push Int256.zero)
        (command_compiled (Coq_evm_push Int256.zero)))
  | Scalldataload -> command_compiled Coq_evm_calldataload

(** val code_compiled : code -> coq_Z PTree.t -> label -> compiled **)

let rec code_compiled c ge code_label =
  match c with
  | Coq_nil -> empty_compiled
  | Coq_cons (stm, rest) ->
    let sc = stm_compiled stm ge code_label in
    let rest0 = code_compiled rest ge code_label in
    concatenate_compiled sc rest0
