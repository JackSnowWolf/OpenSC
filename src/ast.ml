type op = Add | Sub | Equal | Neq | Less | And | Or | LGT

type typ = 
	| Bool 
	| Int
	| Uint of string 
	| Address of string
	| Void of string
	| Mapstruct of string * string


(* Need change *)
(* type ocamlbuiltin = Int | Bool | String *)

(* type param =  *)

type expr =
	| NumLit of int  (* number literal *)
	| BooLit of bool
	| StrLit of string
	| Id of string
	| Var of string * typ 
	(* | AddressLit of string list *)
	| TypeAssign of expr * typ
	| MapAssign of expr * typ * typ 
	(* | MapAssigns of string * typ list * typ  *)
	| PointAssign of expr * expr
	| Event of string * typ list
	| Binop of expr * op * expr
	| Constructorexpr of string * typ * typ 
	| Methodexpr of string * typ  * typ 
	| Logexpr of expr * expr list

(* control flow statement: if, while ?? *)
type stmt =
		Block of stmt list
	|	Expr of expr
	| Return of expr

type consturctor_def ={
	name: expr;
	params: expr list;
	consturctor_body: expr list;
	return_type: typ;
}


type method_def = {
	methodname: expr;
	params: expr list;
	guard_body: expr list;
	storage_body: expr list;
	effects_body: expr list;
	returns: typ;
}

type interface_def = {
	signaturename: expr;
	interfacebody: expr list;
}

type implementation_def = {
	consturctor: consturctor_def;
	methods: method_def list;
}

(* type program = interface_def list * consturctor_def list *)
(* consturctor list is bad ! *)
type program = interface_def list * implementation_def list
(* type program = interface_def list * consturctor_def list * method_def list  *)

(* pretty printing *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
	| Or -> "||"
	| LGT -> ">"

(* let string_of_builtin = function
		Int -> "int"
	|	Bool -> "boolean"
	|	String -> "string" *)
let unit_to_string () = "void"	

let rec string_of_typ = function
		Bool-> "bool" ^ " "
	| Int -> "int" ^ " "
	| Uint(x) ->  x ^ " "
	| Address(x) ->  x ^ " "
	| Void(x) ->  x ^ " "
	| Mapstruct(x, y) -> x ^ "I am mapping struct " ^ y ^ " "

(* let string_of_param = function *)


let rec string_of_expr = function
		NumLit(x) -> string_of_int x ^ " "
	| BooLit(x) -> string_of_bool x ^ " "
	| Id(x) -> "ID: " ^ x ^ " "
	| StrLit(x) -> x
	| Var(x, t) -> x ^ string_of_typ t
	| TypeAssign(x, y)-> "Type Assign: " ^ string_of_expr x  ^ " " ^ string_of_typ y ^ "\n"
	| MapAssign(x, t1, t2) -> "Map assign: " ^ string_of_expr x ^ " " ^ (string_of_typ t1) ^ (string_of_typ t2) ^ "\n"
	| PointAssign(x, e) -> "pointer assign: " ^string_of_expr x ^ " " ^ (string_of_expr e) ^ "\n"
	| Event(x, ty) -> x ^ "Event: " ^ String.concat " " (List.map string_of_typ ty) ^ "\n"
	| Binop(e1, op, e2) ->  "binary operation: " ^ (string_of_expr e1) ^ " " ^ " "  ^ (string_of_op op) ^ " " ^ (string_of_expr e2) ^ "\n"
	| Constructorexpr(x, ty1, ty2) -> "constructor expr: " ^ " " ^ x ^ " " ^ string_of_typ ty1 ^ " " ^  string_of_typ ty2 ^ "\n"
	| Methodexpr(x, ty1, ty2) -> "Method expr: " ^ x ^ " "  ^ string_of_typ ty1 ^ " " ^ string_of_typ ty2 ^ " " ^ "\n"
	| Logexpr(e, el) -> "Log for event: " ^ " " ^ string_of_expr e ^ " " ^ String.concat " " (List.map string_of_expr el) ^ "\n"

(* let string_of_expr = function
		NumLit(l) -> string_of_int l
	| BooLit(true) -> "true"
	| BooLit(false) -> "false"
	| Id(s) -> s *)

let string_of_interfacedef interfacedecl =
	string_of_expr interfacedecl.signaturename ^ " \n " ^
  String.concat " \n " (List.map string_of_expr interfacedecl.interfacebody)

let string_of_constructordef constructordecl = 
	string_of_expr constructordecl.name ^ " \n " ^  
	String.concat " \n " (List.map string_of_expr constructordecl.params) ^ 
	String.concat " \n " (List.map string_of_expr constructordecl.consturctor_body) ^
	" \n " ^ string_of_typ constructordecl.return_type

let string_of_methoddef methoddecl = 
	string_of_expr methoddecl.methodname ^ " \n " ^
	String.concat " \n " (List.map string_of_expr methoddecl.params) ^
	String.concat " \n " (List.map string_of_expr methoddecl.guard_body) ^
	String.concat " \n " (List.map string_of_expr methoddecl.storage_body) ^
	String.concat " \n " (List.map string_of_expr methoddecl.effects_body) ^
	string_of_typ methoddecl.returns


let string_of_implementation implementdecl =
	string_of_constructordef implementdecl.consturctor ^ " " ^
	String.concat "\n" (List.map string_of_methoddef implementdecl.methods)

let string_of_program (interfaces, implementations) =
	"\n\nParsed program: \n\n" ^
	String.concat "" (List.map string_of_interfacedef interfaces) ^ "\n"  ^
  String.concat "\n" (List.map string_of_implementation implementations) ^ "Yeah"
