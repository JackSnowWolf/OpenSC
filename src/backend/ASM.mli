
type asm (* representation corresponding directly to real EVM instructions *)
type asm_program  = asm list
type evm_program  = EVM.evm list
type intermediate = {
    constructor : evm_program ;
    body        : evm_program
  }

(* *)
val transform : evm_program -> AST.label -> asm_program

(* final bytecode output *)
val assemble  : asm_program  -> string

(* representation for humans *)
val mnemonics : asm_program  -> string
