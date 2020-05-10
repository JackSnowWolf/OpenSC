open Ast 
open Sast
open List

module StringMap = Map.Make(String)




let check_ids id = function
  [] -> []
  | hd :: tl -> []
  

(* need to implement *)
let rec check_sexpr = function
  [] -> []
  | hd :: tl -> 
    (match hd with 
   | EnvLit l -> SenvLit l
   | NumLit l -> SnumLit l
   | BooLit l -> SbooLit l
   | StrLit l -> SstrLit l
   | Id x -> SId x
   | Var (s, t) -> Svar (s, t)
   | TypeAssign (e, t) -> StypeAssign((check_sexpr e), t)
   | MapAssign (e, t1, t2) -> SmapAssign ((check_sexpr e), t1, t2)
   | EnvironmentAssign (e1, e2, e3, e4) -> SenvironmentAssign(check_sexpr e1, check_sexpr e2, check_sexpr e3, check_sexpr e4)
   | EnvironmentBinop (e1, e2, op, e4) -> SenvironmentBinop(check_sexpr e1, check_sexpr e2, op, check_sexpr e4)
   | PointAssign(e1, e2) -> SpointAssign((check_sexpr e1), (check_sexpr e2))
   | Event (s, t) -> Sevent(s, t)
   | Binop (e1, op, e2) -> Sbinop((check_sexpr e1), op, (check_sexpr e2))
   | Constructorexpr (s, t1, t2) -> Sconstructorexpr(s, t1, t2)
   | Methodexpr (s, t1, t2) -> Smethodexpr(s, t1, t2)
   | Logexpr (e, e2) -> Slogexpr(e, check_sexpr e2)
   ) :: check_sexpr tl 
  

let rec check_consturctor_def c = function
   [] -> []
  | c.name n -> check_sexpr n
  | c.params ps -> check_sexpr ps
  | c.consturctor_body cb -> check_sexpr cb 
  | c.retrun_type t -> check_type t

let rec check_method_def md = function
     [] -> []
   | hd :: tl -> (match hd with 
   |md.methodname n -> check_sexpr n 
   |md.params ps -> check_sexpr ps 
   |md.guard_body gb -> check_sexpr gb 
   |md.storage_body sb -> check_sexpr sb 
   |md.effects_body eb -> check_sexpr eb
   |md.returns rt -> check_type rt
   ) :: check_method_def tl 


let rec check_interfaces interface = function
  | [] -> []
  | hd :: tl -> (match hd with 
      | interface.signaturename(e) -> check_sexpr e
      | interface.interfacebody(e) -> check_sexpr e 
  ) :: check_interfaces tl 

let rec check_implementations implementation = function
  | [] -> []
  | implementation.consturctor(c) -> check_consturctor_def c
  | implementation.methods(ms) -> check_method_def ms


let check program = function
  | [] -> []
  | program.interface -> check_interfaces program.interface
  | program.implementation -> check_implementations program.imeplementations 
