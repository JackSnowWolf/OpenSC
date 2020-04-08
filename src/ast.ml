
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
		rtyp: typ;
	}	

type MethodImplement = 
	{
		fname: typ;
		formals: field list;
		body: block list;
		rtyp: typ;
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

type var = 
	Var of string
	| Env of string
	| NumLit of int 
	| BooLit of bool
	| AddressLit of string * expr
	| UnitLit of unit

type expr =
	Call of string * expr list
	| Var of string
	| Assign of expr * expr
	| PointAssign of expr * expr

type Interface = 
	signature of string * interface_expr list

type Implementation = 
	Implementationbody of implementation_expr list

type program = Interface * Implementation

