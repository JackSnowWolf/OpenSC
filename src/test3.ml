open Sast
open TranslateMinic

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check (fst program) in
(*   print_endline (string_of_sprogram sprogram) in *)
  let minicAST = TranslateMinic.gen_expr (List.hd sprogram.sinterfacebody) in
  print_endline (string_of_expr minicAST)




  (* | MINIC -> let ge = minicgen filename ast_structure in print_endline (Backend.LanguageExt.show_genv ge)			      *)


(* ocamlbuild  -I backend test3.native *)