type op = Add | Sub | Times | Divide | Equal | Neq | Less | And | Or | LGT | RGT | LGTEQ | RGTEQ | PASSIGN

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
type literal =
	| NumLit of int  (* number literal *)
	| BoolLit of bool
	| StrLit of string
	| Id of string
	| Var of literal * typ
	| EnvLit of string * string

type decls = 
	| TypeAssigndecl of literal * typ
	| MapAssigndecl of literal * typ
	| Vardecl of literal * typ 
	| Eventdecl of literal * typ list
	| Constructordecl of literal * typ * typ 
	| Methodecls of literal * typ list * typ
	(* | PointAssign of literal * literal *)

type expr =
	| Literalexpr of literal
	| Mapexpr of literal * literal list 
	| Binop of expr * op * expr
	| Logexpr of literal * literal list



(* control flow statement: if, while ?? *)
(* type stmt =
	  Block of stmt list
	| Expr of expr
	| Return of expr *)

type consturctor_def ={
	name: literal;
	params: literal list;
	consturctor_body: expr list;
	return_type: typ;
}


type method_def = {
	methodname: literal;
	params: literal list;
	guard_body: expr list;
	storage_body: expr list;
	effects_body: expr list;
	returns: typ;
}

type interface_def = {
	signaturename: literal;
	interfacebody: decls list;
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
	| PASSIGN -> "|->"

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
	| Mapstruct(x, y) ->  "I am mapping struct " ^ String.concat " " (List.map string_of_typ x) ^ String.concat " " (List.map string_of_typ y) ^ " "

(* let string_of_param = function *)




let rec string_of_literal = function
		| NumLit(l) -> "Number Literal " ^ string_of_int l ^ " "
		| BoolLit(l) -> "Bool " ^ string_of_bool l ^ " "
		| StrLit(l) -> "String literal " ^ l ^ " "
		| Id(x) -> "ID: " ^ x ^ " "
		| Var(x , t) -> "Var: " ^ string_of_literal x ^ string_of_typ t ^ " "
		| EnvLit(l, l2) -> "Envrionment: " ^ l ^ (l2) ^ " "


(* 
type decls = 
	| TypeAssigndecl of literal * typ
	| MapAssigndecl of literal * typ
	| Var of literal * typ 
	| Event of literal * typ list
	| Constructordecl of literal * typ * typ 
	| Methodecls of literal * typ list * typ 
*)

let rec string_of_decl = function
		| TypeAssigndecl(l, t) -> "Type Assign: " ^ string_of_literal l  ^ " " ^ string_of_typ t ^ "\n"
		| MapAssigndecl (l, t) -> "Map assign: " ^ string_of_literal l ^ " " ^ (string_of_typ t) ^ "\n"
		| Vardecl(l, t) ->  "Var: " ^ string_of_literal l ^ string_of_typ t
		| Eventdecl(l ,t) ->  "Event: " ^ (string_of_literal l) ^  String.concat " " (List.map string_of_typ t) ^ "\n"
		| Constructordecl(l, t1, t2) ->"constructor expr: " ^ " " ^ string_of_literal l^ " " ^ string_of_typ t1 ^ " " ^  string_of_typ t2 ^ "\n"
		| Methodecls (l, t1, t2) -> "Method expr: " ^ string_of_literal l ^ " "  ^ String.concat " " (List.map string_of_typ t1)  ^ (string_of_typ t2) ^ " " ^ "\n"
		(* | PointAssign(x, e) -> "pointer assign: " ^ string_of_literal x ^ " " ^ (string_of_literal e) ^ "\n" *)

(*


type expr =
	| Literalexpr of literal
	| Mapexpr of decls * literal 
	| Binop of expr * op * expr
	| Logexpr of literal * literal list
	
*)
let rec string_of_expr = function
	| Literalexpr(l1) -> "Literal expr: " ^ string_of_literal l1 
	| Mapexpr (l1, l2) -> "Map expr: " ^ string_of_literal l1 ^ String.concat " " (List.map string_of_literal l2)
	| Binop(e1, op, e2) ->  "binary operation: " ^ (string_of_expr e1) ^ " " ^ " "  ^ (string_of_op op) ^ " " ^ (string_of_expr e2) ^ "\n"
	| Logexpr(e, el) -> "Log for event: " ^ " " ^ string_of_literal e ^ " " ^ String.concat " " (List.map string_of_literal el) ^ "\n"

(* let string_of_expr = function
		NumLit(l) -> string_of_int l
	| BooLit(true) -> "true"
	| BooLit(false) -> "false"
	| Id(s) -> s *)

let string_of_interfacedef interfacedecl =
	string_of_literal interfacedecl.signaturename ^ " \n " ^
  String.concat " \n " (List.map string_of_decl interfacedecl.interfacebody)

let string_of_constructordef constructordecl = 
	string_of_literal constructordecl.name ^ " \n " ^  
	String.concat " \n " (List.map string_of_literal constructordecl.params) ^ 
	String.concat " \n " (List.map string_of_expr constructordecl.consturctor_body) ^
	" \n " ^ string_of_typ constructordecl.return_type

let string_of_methoddef methoddecl = 
	string_of_literal methoddecl.methodname ^ " \n " ^
	String.concat " \n " (List.map string_of_literal methoddecl.params) ^
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

