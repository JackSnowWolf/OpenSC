open Ast 
open Sast
open List

module StringMap = Map.Make(String)


(* 
let strore_ids ta = function *)


(* need to implement *)
let check (signature, implementation) =
  let check_expr = function
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

  (* let check_decl = function
    | Var(expr,typ) -> 
    | TypeAssigndecl(expr,typ) ->
    | MapAssigndecl(expr,typ) -> 
    | Eventdecl(expr,typli) -> 
    | Constructordecl(expr,typ,typ) -> 
    | Methodecls(expr,typli,typ) -> 
  in *)

  let check_func func = 
    { 
      smethodname = check_expr func.methodname;
      sparams = func.params;
      sguard_body = List.map check_expr func.guard_body;
      sstorage_body = List.map check_expr func.storage_body;
      seffects_body = List.map check_expr func.effects_body;
      sreturns = func.returns;
    }
  in

  let sinterface_def =
      {
        ssignaturename = check_expr signature.signaturename;
        sinterfacebody = signature.interfacebody;
      }
  in 

  let simplementation_def = 
    {
      sconsturctor = {
        sname = check_expr implementation.consturctor.name;
        sparams = implementation.consturctor.params;
        sconsturctor_body = List.map check_expr implementation.consturctor.consturctor_body;
        sreturn_type = implementation.consturctor.return_type;
      };

      smethods = List.map check_func implementation.methods;
    }
  in 

  let sprogram = (sinterface_def, simplementation_def)
  in
  sprogram
