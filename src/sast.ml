open Ast 


(* type sparam =  *)

type sexpr = typ * sx
and sx = 
	| SNumLit of int  (* number literal *)
	| SBoolLit of bool
	| SStrLit of string
	| SId of string
	| SVar of expr * typ
	| SEnvLit of string * string
	| SLiteralexpr of expr
	| SMapexpr of expr * expr list 
	| SBinop of expr * op * expr
	| SLogexpr of expr * expr list


type sconsturctor_def ={
	sname: sexpr;
	sparams: sexpr list;
	sconsturctor_body: sexpr list;
	sreturn_type: typ;
}


type smethod_def = {
	smethodname: sexpr;
	sparams: literal list;
	sguard_body: sexpr list;
	sstorage_body: sexpr list;
	seffects_body: sexpr list;
	sreturns: typ;
}

type sinterface_def = {
	ssignaturename: sexpr;
	sinterfacebody: decls list;
}

type simplementation_def = {
	sconsturctor: sconsturctor_def;
	smethods: smethod_def list;
}

type sprogram = sinterface_def list * simplementation_def list


let rec string_of_sexpr (t, e) = 
	"(" ^ string_of_typ t ^ " : " ^ (match e with 
		SNumLit(x) -> string_of_int x ^ " "
	| SBoolLit(x) -> string_of_bool x ^ " "
	| SStrLit(x) -> x
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
	) ^ " ) "


let string_of_sconstructordef constructordecl = 
	string_of_expr constructordecl.smethodname ^ " \n " ^  
	String.concat " \n " (List.map string_of_expr constructordecl.sparams) ^ 
	String.concat " \n " (List.map string_of_expr constructordecl.sconsturctor_body) ^
	" \n " ^ string_of_typ constructordecl.sreturn_type

let string_of_smethoddef methoddecl = 
	string_of_sexpr methoddecl.smethodname ^ " \n " ^
	String.concat " \n " (List.map string_of_sexpr methoddecl.sparams) ^
	String.concat " \n " (List.map string_of_sexpr methoddecl.sguard_body) ^
	String.concat " \n " (List.map string_of_sexpr methoddecl.sstorage_body) ^
	String.concat " \n " (List.map string_of_sexpr methoddecl.seffects_body) ^
	string_of_typ methoddecl.returns

let string_of_sinterfacedef sinterfacedecl =
	string_of_sexpr sinterfacedecl.ssignaturename ^ " \n " ^
  String.concat " \n " (List.map string_of_decls sinterfacedecl.sinterfacebody)


let string_of_simplementation implementdecl =
	string_of_sconstructordef implementdecl.sconsturctor ^ " " ^
	String.concat "\n" (List.map string_of_smethoddef implementdecl.smethods)

let string_of_sprogram (interfaces, implementations) =
	"\n\nParsed program: \n\n" ^
	String.concat "" (List.map string_of_sinterfacedef interfaces) ^ "\n"  ^
	String.concat "\n" (List.map string_of_simplementation implementations) ^ "Yeah"
	
