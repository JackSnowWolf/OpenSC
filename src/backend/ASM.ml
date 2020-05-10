module Links = Map.Make(struct type t = AST.label let compare = compare end)

type asm =
| EVM_STOP
| EVM_ADD
| EVM_MUL
| EVM_SUB
| EVM_DIV
| EVM_SDIV
| EVM_MOD
| EVM_SMOD
| EVM_ADDMOD
| EVM_MULMOD
| EVM_EXP
| EVM_SIGNEXTEND
| EVM_LT
| EVM_GT
| EVM_SLT
| EVM_SGT
| EVM_EQ
| EVM_ISZERO
| EVM_AND
| EVM_OR
| EVM_XOR
| EVM_NOT
| EVM_BYTE
| EVM_SHA3
| EVM_ADDRESS
| EVM_BALANCE
| EVM_ORIGIN
| EVM_CALLER
| EVM_CALLVALUE
| EVM_CALLDATALOAD
| EVM_CALLDATASIZE
| EVM_CODESIZE
| EVM_GASPRICE
| EVM_EXTCODESIZE
| EVM_BLOCKHASH
| EVM_COINBASE
| EVM_TIMESTAMP
| EVM_NUMBER
| EVM_DIFFICULTY
| EVM_GASLIMIT
| EVM_GAS
| EVM_CODECOPY
| EVM_POP
| EVM_MLOAD
| EVM_MSTORE
| EVM_MSTORE8
| EVM_SLOAD
| EVM_SSTORE
| EVM_JUMP
| EVM_JUMPI
| EVM_JUMPDEST of string
| EVM_PUSH of int * string
| EVM_DUP of int
| EVM_SWAP of int
| EVM_LOG
| EVM_CALL
| EVM_REVERT
| EVM_RETURN

type asm_program  = asm list
type evm_program  = EVM.evm list

type intermediate = {
    constructor : evm_program ;
    body        : evm_program
  }

(* we assume jump destinations can fit in 4 bytes *)
let address_bytes = 4


let pad len s =
  String.make (len - (String.length s)) '0' ^ s

(* compute number of bytes needed to represent a value *)
let allocate =
  let allocate' p =
    let rec count_digits = function
      | BinNums.Coq_xI v -> 1 + count_digits v
      | BinNums.Coq_xO v -> 1 + count_digits v
      | BinNums.Coq_xH   -> 1
    in (count_digits p + 7) / 8
  in function
  | BinNums.Z0 -> 1
  | BinNums.Zpos x -> allocate' x
  | BinNums.Zneg x -> raise (Failure "allocate is undefined on negative numbers")

(* construct a mapping from label -> PC *)
let map_labels program = 
  let rec map_labels' program counter =
    match program with
    | [] -> Links.empty
    | (x :: xs) ->
       match x with
       | EVM.Coq_evm_label l -> Links.add l counter (map_labels' xs (counter + 1))
       | EVM.Coq_evm_push_label l -> map_labels' xs (counter + 5)
       | EVM.Coq_evm_push v -> map_labels' xs (counter + 1 + allocate v)
       | _ -> map_labels' xs (counter + 1)
  in map_labels' program 0

let hex x = Printf.sprintf "%x" x

(* compute the bytecode for some push data *) 
let assemble_pushdata n data =
  let hex_of_coq_int =
    let rec hex_of_coq_pos partial count =
      let rec pow2 = function
        | 0 -> 1
        | n -> 2 * (pow2 (n-1)) in
      let lastchar = if count == 4 then Printf.sprintf "%x" partial else ""
      and partial = if count == 4 then 0 else partial
      and count = count mod 4
      in function
      | BinNums.Coq_xI rest -> (hex_of_coq_pos (partial + (pow2 count)) (count + 1) rest) ^ lastchar
      | BinNums.Coq_xO rest -> (hex_of_coq_pos partial (count + 1) rest) ^ lastchar
      | BinNums.Coq_xH -> (hex (partial + (pow2 count))) ^ lastchar
    in function
    | BinNums.Z0 -> "0"
    | BinNums.Zpos v -> hex_of_coq_pos 0 0 v
    | BinNums.Zneg v -> raise (Failure "undefined")
  in
  pad (n * 2) (hex_of_coq_int data)

(* compute the bytecode for an opcode *)
let assemble_op x = pad 2 (hex x)

(* evm -> asm *)
let transform_inst links = function
  | EVM.Coq_evm_stop               -> EVM_STOP
  | EVM.Coq_evm_add                -> EVM_ADD
  | EVM.Coq_evm_mul                -> EVM_MUL
  | EVM.Coq_evm_sub                -> EVM_SUB
  | EVM.Coq_evm_div                -> EVM_DIV
  | EVM.Coq_evm_sdiv               -> EVM_SDIV
  | EVM.Coq_evm_mod                -> EVM_MOD
  | EVM.Coq_evm_smod               -> EVM_SMOD
  | EVM.Coq_evm_addmod             -> EVM_ADDMOD
  | EVM.Coq_evm_mulmod             -> EVM_MULMOD
  | EVM.Coq_evm_exp                -> EVM_EXP
  | EVM.Coq_evm_signextend         -> EVM_SIGNEXTEND
  | EVM.Coq_evm_lt                 -> EVM_LT
  | EVM.Coq_evm_gt                 -> EVM_GT
  | EVM.Coq_evm_slt                -> EVM_SLT
  | EVM.Coq_evm_sgt                -> EVM_SGT
  | EVM.Coq_evm_eq                 -> EVM_EQ
  | EVM.Coq_evm_iszero             -> EVM_ISZERO
  | EVM.Coq_evm_and                -> EVM_AND
  | EVM.Coq_evm_or                 -> EVM_OR
  | EVM.Coq_evm_xor                -> EVM_XOR
  | EVM.Coq_evm_not                -> EVM_NOT
  | EVM.Coq_evm_byte               -> EVM_BYTE
  | EVM.Coq_evm_sha3               -> EVM_SHA3
  | EVM.Coq_evm_address            -> EVM_ADDRESS
  | EVM.Coq_evm_balance            -> EVM_BALANCE
  | EVM.Coq_evm_origin             -> EVM_ORIGIN
  | EVM.Coq_evm_caller             -> EVM_CALLER
  | EVM.Coq_evm_callvalue          -> EVM_CALLVALUE
  | EVM.Coq_evm_calldataload       -> EVM_CALLDATALOAD
  | EVM.Coq_evm_calldatasize       -> EVM_CALLDATASIZE
  | EVM.Coq_evm_codesize           -> EVM_CODESIZE
  | EVM.Coq_evm_gasprice           -> EVM_GASPRICE
  | EVM.Coq_evm_extcodesize        -> EVM_EXTCODESIZE
  | EVM.Coq_evm_blockhash          -> EVM_BLOCKHASH
  | EVM.Coq_evm_coinbase           -> EVM_COINBASE
  | EVM.Coq_evm_timestamp          -> EVM_TIMESTAMP
  | EVM.Coq_evm_number             -> EVM_NUMBER
  | EVM.Coq_evm_difficulty         -> EVM_DIFFICULTY
  | EVM.Coq_evm_gaslimit           -> EVM_GASLIMIT
  | EVM.Coq_evm_gas                -> EVM_GAS
  | EVM.Coq_evm_codecopy           -> EVM_CODECOPY
  | EVM.Coq_evm_pop                -> EVM_POP
  | EVM.Coq_evm_mload              -> EVM_MLOAD
  | EVM.Coq_evm_mstore             -> EVM_MSTORE
  | EVM.Coq_evm_mstore8            -> EVM_MSTORE8
  | EVM.Coq_evm_sload              -> EVM_SLOAD
  | EVM.Coq_evm_sstore             -> EVM_SSTORE
  | EVM.Coq_evm_jump               -> EVM_JUMP
  | EVM.Coq_evm_jumpi              -> EVM_JUMPI
  | EVM.Coq_evm_label l            -> EVM_JUMPDEST (Printf.sprintf "%08x" (Links.find l links))
  | EVM.Coq_evm_push x ->
     EVM_PUSH (allocate x, assemble_pushdata (allocate x) x)
  | EVM.Coq_evm_push_label l ->
     EVM_PUSH (address_bytes, Printf.sprintf "%08x" (Links.find l links))
  | EVM.Coq_evm_dup n ->
     EVM_DUP (DatatypesExt.eval_nat n)
  | EVM.Coq_evm_swap n ->
     EVM_SWAP (DatatypesExt.eval_nat n)
  | EVM.Coq_evm_log                -> EVM_LOG
  | EVM.Coq_evm_call               -> EVM_CALL
  | EVM.Coq_evm_revert             -> EVM_REVERT
  | EVM.Coq_evm_return             -> EVM_RETURN

(* asm -> bytecode *)
let assemble_inst = function
  | EVM_STOP -> "00"
  | EVM_ADD -> "01"
  | EVM_MUL -> "02"
  | EVM_SUB -> "03"
  | EVM_DIV -> "04"
  | EVM_SDIV -> "05"
  | EVM_MOD -> "06"
  | EVM_SMOD -> "07"
  | EVM_ADDMOD -> "08"
  | EVM_MULMOD -> "09"
  | EVM_EXP -> "0a"
  | EVM_SIGNEXTEND -> "0b"
  | EVM_LT -> "10"
  | EVM_GT -> "11"
  | EVM_SLT -> "12"
  | EVM_SGT -> "13"
  | EVM_EQ -> "14"
  | EVM_ISZERO -> "15"
  | EVM_AND -> "16"
  | EVM_OR -> "17"
  | EVM_XOR -> "18"
  | EVM_NOT -> "19"
  | EVM_BYTE -> "1a"
  | EVM_SHA3 -> "20"
  | EVM_ADDRESS -> "30"
  | EVM_BALANCE -> "31"
  | EVM_ORIGIN -> "32"
  | EVM_CALLER -> "33"
  | EVM_CALLVALUE -> "34"
  | EVM_CALLDATALOAD -> "35"
  | EVM_CALLDATASIZE -> "36"
  | EVM_CODESIZE -> "38"
  | EVM_GASPRICE -> "3a"
  | EVM_EXTCODESIZE -> "3b"
  | EVM_BLOCKHASH -> "40"
  | EVM_COINBASE -> "41"
  | EVM_TIMESTAMP -> "42"
  | EVM_NUMBER -> "43"
  | EVM_DIFFICULTY -> "44"
  | EVM_GASLIMIT -> "45"
  | EVM_GAS -> "5a"
  | EVM_CODECOPY -> "39"
  | EVM_POP -> "50"
  | EVM_MLOAD -> "51"
  | EVM_MSTORE -> "52"
  | EVM_MSTORE8 -> "53"
  | EVM_SLOAD -> "54"
  | EVM_SSTORE -> "55"
  | EVM_JUMP -> "56"
  | EVM_JUMPI -> "57"
  | EVM_JUMPDEST l -> "5b"
  | EVM_PUSH (n, data) ->
     (assemble_op (95 + n)) ^ data
  | EVM_DUP n -> assemble_op (127 + n)
  | EVM_SWAP n -> assemble_op (143 + n)
  | EVM_LOG -> "a0"
  | EVM_CALL -> "f1"
  | EVM_REVERT -> "fd"
  | EVM_RETURN -> "f3"


let show_asm_inst = function
  | EVM_STOP -> "STOP"
  | EVM_ADD -> "ADD"
  | EVM_MUL -> "MUL"
  | EVM_SUB -> "SUB"
  | EVM_DIV -> "DIV"
  | EVM_SDIV -> "SDIV"
  | EVM_MOD -> "MOD"
  | EVM_SMOD -> "SMOD"
  | EVM_ADDMOD -> "ADDMOD"
  | EVM_MULMOD -> "MULMOD"
  | EVM_EXP -> "EXP"
  | EVM_SIGNEXTEND -> "SIGNEXTEND"
  | EVM_LT -> "LT"
  | EVM_GT -> "GT"
  | EVM_SLT -> "SLT"
  | EVM_SGT -> "SGT"
  | EVM_EQ -> "EQ"
  | EVM_ISZERO -> "ISZERO"
  | EVM_AND -> "AND"
  | EVM_OR -> "OR"
  | EVM_XOR -> "XOR"
  | EVM_NOT -> "NOT"
  | EVM_BYTE -> "BYTE"
  | EVM_SHA3 -> "SHA3"
  | EVM_ADDRESS -> "ADDRESS"
  | EVM_BALANCE -> "BALANCE"
  | EVM_ORIGIN -> "ORIGIN"
  | EVM_CALLER -> "CALLER"
  | EVM_CALLVALUE -> "CALLVALUE"
  | EVM_CALLDATALOAD -> "CALLDATALOAD"
  | EVM_CALLDATASIZE -> "CALLDATASIZE"
  | EVM_CODESIZE -> "CODESIZE"
  | EVM_GASPRICE -> "GASPRICE"
  | EVM_EXTCODESIZE -> "EXTCODESIZE"
  | EVM_BLOCKHASH -> "BLOCKHASH"
  | EVM_COINBASE -> "COINBASE"
  | EVM_TIMESTAMP -> "TIMESTAMP"
  | EVM_NUMBER -> "NUMBER"
  | EVM_DIFFICULTY -> "DIFFICULTY"
  | EVM_GASLIMIT -> "GASLIMIT"
  | EVM_GAS -> "GAS"
  | EVM_CODECOPY -> "CODECOPY"
  | EVM_POP -> "POP"
  | EVM_MLOAD -> "MLOAD"
  | EVM_MSTORE -> "MSTORE"
  | EVM_MSTORE8 -> "MSTORE8"
  | EVM_SLOAD -> "SLOAD"
  | EVM_SSTORE -> "SSTORE"
  | EVM_JUMP -> "JUMP"
  | EVM_JUMPI -> "JUMPI"
  | EVM_JUMPDEST l -> ("L"^ l ^ ": JUMPDEST")
  | EVM_PUSH (n, data) ->
     Printf.sprintf "%-16s %s" (Printf.sprintf "PUSH%d" n) data
  | EVM_DUP n -> Printf.sprintf "DUP%d" n
  | EVM_SWAP n -> Printf.sprintf "SWAP%d" n
  | EVM_CALL -> "CALL"
  | EVM_REVERT -> "REVERT"
  | EVM_RETURN -> "RETURN"
  | EVM_LOG -> "LOG"

let show_evm program =
  List.fold_left (fun acc x -> acc ^ "\n" ^ (EVMExt.show x)) "" program

let split program label =
  let rec constructor_helper target acc = function
    | x :: xs ->
       if (x = target) then
         acc
       else
         constructor_helper target (acc @ [x]) xs
    | [] -> []
  and body_helper target = function
    | x :: xs ->
       if (x = target) then
         x :: xs
       else
         body_helper target xs
    | [] -> []
  in
  {
    constructor = constructor_helper (EVM.Coq_evm_label label) [] program ;
    body = body_helper (EVM.Coq_evm_label label) program
  }

let assemble program =
  let bytecode_list = List.map assemble_inst program in
  List.fold_left (fun acc x -> acc ^ x) "" bytecode_list

let transform program entrypoint =
  let transform_intermediate program =
    let transform' links p =
      List.map (transform_inst links) p in
    let size_of_inst = function
      | EVM_PUSH (n, data) -> 1 + n
      | _ -> 1 in
    let size_of_program asm =
      List.fold_left (fun acc x -> acc + (size_of_inst x)) 0 asm in
    let chop_links n =
      Links.map (fun x -> x - n) in
    let links = map_labels (program.constructor @ program.body) in
    let constructor_asm = transform' links program.constructor in
    let constructor_size = size_of_program constructor_asm in
    let links' = chop_links constructor_size links in
    let body_asm = transform' links' program.body in
    constructor_asm @ body_asm
  in
  transform_intermediate (split program entrypoint)

let mnemonics program =
  List.fold_left (fun acc x -> acc ^ "\n" ^ (show_asm_inst x)) "" program
