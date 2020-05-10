open Ast 
open Sast
open List

module StringMap = Map.Make(String)


(* 
let strore_ids ta = function *)


(* need to implement *)
let check (signature, implementation) =
  let rec check_funcs = function
    [] -> []
  | hd :: tl -> hd :: tl
  in

  let rec check_sexpr_list = function
  [] -> []
  | hd :: tl -> (check_sexpr hd) :: check_sexpr_list tl
  and check_sexpr = function
    | NumLit l -> (Int, SNumLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StrLit l -> (Void("void"), SStrLit l)
    (* check Id retrun with the correct type, keep Int for now *)
    | Id x -> (Int, SId x)
    | EnvLit(x, y) -> (Int, SEnvLit(x,y))
    | Mapexpr(e1, e2) -> (Int, SMapexpr(e1, e2))
    | Binop(e1, op, e2) -> (Int, SBinop(e1, op, e2))
    | Logexpr(e1, e2) -> (Int, SLogexpr(e1, e2))

    in

    let sinterface_def =
        {
          ssignaturename = check_sexpr signature.signaturename;
          sinterfacebody = signature.interfacebody;
        }
    in 
    let simplementation_def = 
      {
        sconsturctor = {
          sname = check_sexpr implementation.consturctor.name;
          sparams = implementation.consturctor.params;
          sconsturctor_body = check_sexpr_list implementation.consturctor.consturctor_body;
          sreturn_type = implementation.consturctor.return_type;
        };

        smethods = [] (* check_funcs implementation.methods; *)
      }
    in 
    let sprogram = (sinterface_def, simplementation_def)
    in 
    sprogram
