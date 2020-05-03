(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
  | SNumLit of int 
  | SBooLit of bool
  | SStrLit of string
  | SId of string
  | SVar of string * typ
  (* | AddressLit of string list *)
  | STypeAssign of string * typ
  | SMapAssign of string * typ * typ 
  (* | MapAssigns of string * typ list * typ  *)
  | SPointAssign of sexpr * sexpr
  | SEvent of string * typ list
  | SBinop of sexpr * op * sexpr
  | SConstructorexpr of string * typ * typ 
  | SMethodexpr of string * typ  * typ 
  | SLogexpr of sexpr * sexpr list

type sstmt =
    SBlock of sstmt list
	|	SExpr of sexpr
	| SReturn of sexpr

type sconsturctor_def ={
  sname: sexpr;
  sparams: sexpr list;
  sconsturctor_body: sexpr list;
  sreturn_type: typ;
}

type smethod_def = {
	smethodname: sexpr;
	sparams: sexpr list;
	sguard_body: sexpr list;
	sstorage_body: sexpr list;
	seffects_body: sexpr list;
	sreturns: typ;
}

type sinterface_def = {
  ssignaturename: sexpr;
  sinterfacebody: sexpr list;
}

type simplementation_def = {
	sconsturctor: sconsturctor_def;
	smethods: smethod_def list;
}

type sprogram = sinterface_def list * simplementation_def list

let rec string_of_sexpr = function
		SnumLit(x) -> string_of_int x ^ " "
	| SbooLit(x) -> string_of_bool x ^ " "
	| SId(x) -> "ID: " ^ x ^ " "
	| SstrLit(x) -> x
	| Svar(x, t) -> x ^ string_of_typ t
	| StypeAssign(x, y)-> "Type Assign: " ^ string_of_sexpr x  ^ " " ^ string_of_typ y ^ "\n"
	| SmapAssign(x, t1, t2) -> "Map assign: " ^ string_of_sexpr x ^ " " ^ (string_of_typ t1) ^ (string_of_typ t2) ^ "\n"
	| SpointAssign(x, e) -> "pointer assign: " ^string_of_sexpr x ^ " " ^ (string_of_sexpr e) ^ "\n"
	| Sevent(x, ty) -> x ^ "Event: " ^ String.concat " " (List.map string_of_typ ty) ^ "\n"
	| Sbinop(e1, op, e2) ->  "binary operation: " ^ (string_of_sexpr e1) ^ " " ^ " "  ^ (string_of_op op) ^ " " ^ (string_of_sexpr e2) ^ "\n"
	| Sconstructorexpr(x, ty1, ty2) -> "constructor expr: " ^ " " ^ x ^ " " ^ string_of_typ ty1 ^ " " ^  string_of_typ ty2 ^ "\n"
	| Smethodexpr(x, ty1, ty2) -> "Method expr: " ^ x ^ " "  ^ string_of_typ ty1 ^ " " ^ string_of_typ ty2 ^ " " ^ "\n"
	| Slogexpr(e, el) -> "Log for event: " ^ " " ^ string_of_sexpr e ^ " " ^ String.concat " " (List.map string_of_sexpr el) ^ "\n"



let string_of_sinterfacedef interfacedecl =
	string_of_sexpr interfacedecl.ssignaturename ^ " \n " ^
  String.concat " \n " (List.map string_of_sexpr interfacedecl.sinterfacebody)


let string_of_sprogram (interfacebody) =
  "\n\n Test for semantic analysis \n\n " ^
  string_of_sinterfacedef interfacebody ^ "\n"
