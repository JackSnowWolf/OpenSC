open Ast 
open Sast
open List

module StringMap = Map.Make(String)


(* 
let strore_ids ta = function *)


(* need to implement *)
let rec check_sexpr e = match e with
   | NumLit l -> (Int, SNumLit l)
   | BoolLit l -> (Bool, BoolLit l)
   | StrLit l -> (Void, StrLit l)
   (* check Id retrun with the correct type, keep Int for now *)
   | Id x -> (Int, SId x)
   | EnvLit(x, y) -> (Int, SEnvLit x, y)
   | Mapexpr(e1, e2) -> (Int, SMapexpr e1, e2)
   | Binop(e1, op, e2) -> (Int, SBinop e1, op, e2)
   | Logexpr(e1, e2) -> (Int, SLogexpr e1, e2)

let rec check_sexpr_list = function
   [] -> []
  | hd :: tl -> (check_sexpr hd) :: check_sexpr_list tl

let rec check_consturctor_def cons = function
   [] -> []
  | cons.name n -> check_sexpr n
  | cons.params ps -> check_sexpr ps
  | cons.consturctor_body cb -> check_sexpr cb 
  | cons.retrun_type t -> t
  | _ -> .

let rec check_method_def md = function
     [] -> []
   | hd :: tl -> (match hd with 
   |md.methodname n -> check_sexpr n 
   |md.params ps -> check_sexpr ps 
   |md.guard_body gb -> check_sexpr gb 
   |md.storage_body sb -> check_sexpr sb 
   |md.effects_body eb -> check_sexpr eb
   |md.returns rt -> rt
   ) :: check_method_def tl 


let rec check_interfaces interface = function
  | [] -> []
  | hd :: tl -> (match hd with 
      | interface.signaturename(e) -> check_sexpr e
      | interface.interfacebody(e) -> check_sexpr e 
  ) :: check_interfaces tl 


let rec check_implementations implementation = function
  | [] -> []
  | hd :: tl -> (match hd with
      | implementation.consturctor(c) -> check_consturctor_def c
      | implementation.methods(ms) -> check_method_def ms
  ) :: check_implementations tl

let check program = function
  | [] -> []
  | program.interface -> check_interfaces fst program.interface
  | program.implementation -> check_implementations program.imeplementations 


