
type op = Add | Sub | Equal | Neq | Less | And | Or

type typ = 
	| Bool of bool
	| Int of int 
	| Uint of int 
	| Address of string
	| UNIT of unit

type lit =
	| NumLit of int 
	| BooLit of bool
	| StrLit of string
	| AddressLit of string 

(*  need change *)
type assignmentexpr = 
	  MapAssign of string * typ list * typ 
	| VarAssign of string * typ
	| TypeAssign of typ * typ
	| PointAssign of string * string
	| EventAssign of string * string * typ list

type param = 
		Var of string * typ

type params =
		Paras of param list

(*  need change *)
type expr =
	| Assignment of assignmentexpr
	| Log of string * params 
	| Mapliteral of string * expr 
	| Binop of expr * op * expr
	| Literal of lit

type body = 
	| Guard of string * expr list
	| Storage of string * expr list
	| Effects of string * expr list
	| Returns of string * expr list

type implementationDef = 
	| Consturctor of string * params * body (* body list? *)
	| Method of string * params * expr * body list



type interfacesDef = 
	| Stroagedef_interface of string * typ
	|	Mapdef_interface of string * typ list * typ list
	| Eventdef_interface of string * string * typ list
	| Constructordef_interface of string * typ * typ
	| Methoddef_interface of string * typ list * typ
	| End of string

type program = interfacesDef list * implementationDef

