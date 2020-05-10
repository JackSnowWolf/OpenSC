open Ast 


(* type sparam =  *)

  
type sexpr =
	| SenvLit of string
	| SnumLit of int  (* number literal *)
	| SbooLit of bool
	| SstrLit of string
  | SId of string
  | Svar of string * typ 
	(* | AddressLit of string list *)
	| StypeAssign of sexpr * typ
	| SmapAssign of sexpr * typ * typ 
	(* | MapAssigns of string * typ list * typ  *)
	| SpointAssign of sexpr * sexpr
	| Sevent of string * typ list
	| Sbinop of sexpr * op * sexpr
	| Sconstructorexpr of string * typ * typ 
	| Smethodexpr of string * typ  * typ 
	| Slogexpr of sexpr * sexpr list

(* control flow statement: if, while *)
type sstmt =
		Sblock of sstmt list
	|	Sexpr of sexpr
  | Sreturn of sexpr


type sconsturctor_def ={
	name: sexpr;
	params: sexpr list;
	consturctor_body: sexpr list;
	return_type: typ;
}


type smethod_def = {
	methodname: sexpr;
	params: sexpr list;
	guard_body: sexpr list;
	storage_body: sexpr list;
	effects_body: sexpr list;
	returns: typ;
}

type sinterface_def = {
	ssignaturename: sexpr;
	sinterfacebody: sexpr list;
}

type simplementation_def = {
	consturctor: sconsturctor_def;
	methods: smethod_def list;
}

type sprogram = sinterface_def list * simplementation_def list


(* let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
	| Or -> "||"
	| LGT -> ">"


let unit_to_string () = "void"	

let rec string_of_styp = function
		Bool-> "bool" ^ " "
	| Int -> "int" ^ " "
	| SUint(x) ->  x ^ " "
	| SAddress(x) ->  x ^ " "
	| SVoid(x) ->  x ^ " "
  | SMapstruct(x, y) -> x ^ "I am mapping struct " ^ y ^ " "
   *)

(* let rec string_of_sexpr = function
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
  string_of_sinterfacedef interfacebody ^ "\n" *)
