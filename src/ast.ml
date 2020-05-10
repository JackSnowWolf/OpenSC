type op = Add | Sub | Times | Divide | Equal | Neq | Less | And | Or | LGT | RGT | LGTEQ | RGTEQ

type typ = 
	| Bool 
	| Int
	| Uint of string 
	| Address of string
	| Void of string
	| Mapstruct of typ list * typ list


(* Need change *)
(* type ocamlbuiltin = Int | Bool | String *)

(* type param =  *)

type expr =
	| EnvLit of string
	| NumLit of int  (* number literal *)
	| BooLit of bool
	| StrLit of string
	| Id of string
	| Var of string * typ 
	(* | AddressLit of string list *)
	| TypeAssign of expr * typ
	| MapAssign of expr * typ
	(* | MapAssigns of string * typ list * typ  *)
	| EnvironmentAssign of expr * expr * expr * expr
	| EnvironmentBinop of expr * expr * op * expr
	| PointAssign of expr * expr
	| Event of string * typ list
	| Binop of expr * op * expr
	| Constructorexpr of string * typ * typ 
	| Methodexpr of expr * typ list * typ 
	| Logexpr of expr * expr list

(* control flow statement: if, while ?? *)
type stmt =
	  Block of stmt list
	| Expr of expr
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
  | Times -> "*"
  | Divide -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
	| LGT -> ">"
	| RGT -> "<"
	| LGTEQ -> ">="
	| RGTEQ -> "<="

(* let string_of_builtin = function
		Int -> "int"
	|	Bool -> "boolean"
	|	String -> "string" *)
let unit_to_string () = "void"	

let rec string_of_typ = function
		Bool-> "bool"
	| Int -> "int"
	| Uint(x) ->  "uint(" ^ x ^ ")"
	| Address(x) ->  "address(" ^ x ^ ")"
	| Void(x) ->  "void(" ^ x ^ ")"
	| Mapstruct(x, y) ->  "Mapstruct(" ^ String.concat " " (List.map string_of_typ x) ^ String.concat " " (List.map string_of_typ y) ^ ")"

(* let string_of_param = function *)


let rec string_of_expr = function
		NumLit(x) -> string_of_int x
	| BooLit(x) -> string_of_bool x
	| Id(x) -> x
	| EnvLit(x) -> "Envrionment(" ^ x ^ ")"
	| StrLit(x) -> x
	| Var(x, t) -> x ^ ": " ^ string_of_typ t
	| TypeAssign(x, y)-> "TypeAssign(" ^ string_of_expr x  ^ ", " ^ string_of_typ y ^ ")\n"
	| MapAssign(x, t) -> "MapAssign(" ^ string_of_expr x ^ ", " ^ (string_of_typ t) ^ ")\n"
	| EnvironmentBinop (e1, e2, op, e3) -> "EnvBinop " ^ string_of_expr e1 ^ string_of_expr e2 ^ string_of_op op ^ string_of_expr e3
	| EnvironmentAssign (e1, e2, e3, e4) -> "EnvAssign " ^ string_of_expr e1 ^string_of_expr e2 ^ string_of_expr e3 ^ string_of_expr e4 ^ "\n"
	| PointAssign(x, e) -> "PtrAssign(" ^ string_of_expr x ^ ", " ^ (string_of_expr e) ^ ")\n"
	| Event(x, ty) -> x ^ "Event: " ^ String.concat " " (List.map string_of_typ ty) ^ "\n"
	| Binop(e1, op, e2) ->  "Binop(" ^ (string_of_expr e1) ^ " "  ^ (string_of_op op) ^ " " ^ (string_of_expr e2) ^ ")\n"
	| Constructorexpr(x, ty1, ty2) -> "constructor expr: " ^ " " ^ x ^ " " ^ string_of_typ ty1 ^ " " ^  string_of_typ ty2 ^ "\n"
	| Methodexpr(x, ty1, ty2) -> "Method expr: " ^ string_of_expr x ^ " "  ^ String.concat " " (List.map string_of_typ ty1)  ^ (string_of_typ ty2) ^ " " ^ "\n"
	| Logexpr(e, el) -> "Log for event: " ^ " " ^ string_of_expr e ^ " " ^ String.concat " " (List.map string_of_expr el) ^ "\n"

(* let string_of_expr = function
		NumLit(l) -> string_of_int l
	| BooLit(true) -> "true"
	| BooLit(false) -> "false"
	| Id(s) -> s *)

let string_of_interfacedef interfacedecl =
	"--interface\n\n" ^
	"signature " ^
	string_of_expr interfacedecl.signaturename ^ "\n " ^
  String.concat "\n " (List.map string_of_expr interfacedecl.interfacebody)

let string_of_constructordef constructordecl = 
	"constructor " ^
	string_of_expr constructordecl.name ^ 
	"(" ^ String.concat ", " (List.map string_of_expr constructordecl.params) ^ ")\n " ^
	String.concat "\n " (List.map string_of_expr constructordecl.consturctor_body) ^
	"\n returns " ^ string_of_typ constructordecl.return_type ^ "\n\n"

let string_of_methoddef methoddecl = 
	"method " ^
	string_of_expr methoddecl.methodname ^ 
	"(" ^ String.concat ", " (List.map string_of_expr methoddecl.params) ^ ")\n" ^
	" guard\n  " ^
	String.concat "\n  " (List.map string_of_expr methoddecl.guard_body) ^ 
	" storage\n  " ^
	String.concat "\n  " (List.map string_of_expr methoddecl.storage_body) ^
	" effects\n  " ^
	String.concat "\n  " (List.map string_of_expr methoddecl.effects_body) ^
	"\n returns " ^ string_of_typ methoddecl.returns ^ "\n\n"


let string_of_implementation implementdecl =
	"--implementation\n\n" ^
	string_of_constructordef implementdecl.consturctor ^ 
	String.concat "\n" (List.map string_of_methoddef implementdecl.methods)

let string_of_program (interfaces, implementations) =
	"\n\n-------------------\n  Parsed program \n-------------------\n\n" ^
	String.concat "" (List.map string_of_interfacedef interfaces) ^ "\n"  ^
  String.concat "\n" (List.map string_of_implementation implementations) ^ "\n\n***Yeah!***"
