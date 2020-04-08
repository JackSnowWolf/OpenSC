
type op = Add | Sub | Equal | Neq | Less | And | Or | Mul | Div

type typ = 
	Bool
	| Int 
	| UInt 
	| Address
	| UNIT
	| Mapstruct
	| typ list
	
type bind = string * typ

type interface_expr = 
	StorageInterface of string * typ
	| MapInterface of string * typ * typ
	| EventInterface of string * string * typ
	| ConstructorInterface of string * typ * typ
	| MethodInterface of string * typ * typ

type implementation_expr = 
	ConstructorImplement
	| MethodImplement

type ConstructorImplement = 
	{
		fname: typ;
		formals: field list;
		body: block list;
	}	

type MethodImplement = 
	{
		fname: typ;
		formals: field list;
		body: block list;
	} 

type field = 
	Guard of stmt list
	| Storage of stmt list
	| Effects of stmt list
	| Returns of expr

type stmt = 
	Block of stmt list
  (* | Expr of expr *)
	| If of expr * stmt * stmt
	| Logs of string * expr
	| Expr of Expr
  (* | While of expr * stmt *)

type expr =
	Var of string
	| Env of string
	| NumLit of int 
	| BooLit of bool
	| AddressLit of string * expr
	| UnitLit of unit
	| Call of string * expr list
	| Var of string
	| Assign of expr * expr
	| PointAssign of expr * expr

type Interface = 
	signature of string * interface_expr list

type Implementation = 
	Implementationbody of implementation_expr list

type program = Interface * Implementation

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
  | Mult -> "*"
  | Div -> "/"
  
let string_of_typ = function
    Bool -> "bool"
  | Int -> "int"
  | UInt -> "uint"
  | Address -> "address"
  | UNIT -> "unit"
  | Mapstruct -> "mapstruct"
  | typ list -> 
  
 let rec string_of_expr = function
    Var(v) -> v
  | Env() -> ""
  | NumLit(
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | AddressLit(v, e) -> v ^ " = " ^ string_of_expr e
  | UnitLit of unit
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Var(v) -> v
  | Assign(e1, e2) -> "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | PointAssign(e1, e2) -> "(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
 
  let rec string_of_stmt = function
    Block(stmts) ->
     "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  (*| Expr(expr) -> string_of_expr expr ^ ";\n";*)
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Logs(v, e) -> v ^ " " ^ string_of_expr e
  (*| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s*)

