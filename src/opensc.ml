open Sast
open TranslateMinic
open Ast 

type mode = AST | SAST | MINIC | BYTECODE


let usage () = 
  prerr_endline ( "usage: ./opensc.native program.sc (ast | sast | minic | bytecode) \n");
  exit 1

let main argv = 
  let open LanguageExt in
  let open Datatypes in 
  let open Glue in
  let open ASM in
  let open DatatypesExt in
  (if (Array.length argv <> 3) then usage());
  let filename = argv.(1) in
  let mode_flag = match Array.get argv 2 with
    | "ast" -> AST
    | "sast" -> SAST
    | "minic" -> MINIC
    | "bytecode" -> BYTECODE
    | _ -> usage() in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
(*   print_endline (string_of_sprogram sprogram) in *)
  let minicAST = TranslateMinic.minicgen sprogram in
    match mode_flag with
    | AST ->   print_endline (string_of_program program)
    | SAST -> print_endline (string_of_sprogram sprogram)
    | MINIC -> print_endline (show_genv minicAST)
    | BYTECODE -> 
     match full_compile_genv minicAST with
     | None -> print_endline "Compilation failed"; exit 1
     | Some (Coq_pair (program, entrypoint)) ->
        let asm =
          transform
            (List.rev (caml_list program))
            entrypoint in
            print_endline (assemble asm)
            
            
            
let _ = main Sys.argv

(* ocamlbuild -pkg cryptokit -I backend opensc.native *)
