
type op = Add | Sub | Equal | Neq | Less | And | Or

type typ = 
	| Bool of bool
	| Int of int 
	| UInt of int 
	| Address of string
	| UNIT of unit

type lit =
	| NumLit of int 
	| BooLit of bool
	| StrLit of string
	| AddressLit of string 
	
type assignmentexpr = 
	  MapAssign of string * string * lit 
	| VarAssign of string * typ
	| RetAssign of typ * typ
	| PointASsign of string * string

type param = 
		Var of string

type params =
		Paras of param list

type expr =
	| Assignment of assignmentexpr
	| Log of string * params 
	| Binop of expr * op * expr
	| Literal of lit
	| Mapliteral of string * expr 


type typedidentifiers = 
	| TypedIdentifierList of expr list


type decl = 
	| Bind of expr * string

type body = 
	| Guard of expr 
	| Storage of expr 
	| Effects of expr 
	| Returns of expr 

type implementationDef = 
	| Consturctor of string * string * expr * body
	| Method of string * string * expr * body


type interfacesDefs = 
	| Interfacedef of decl list
		

tyoe Interfacedef =
	| 
	| End of string

type program = interfacesDefs * implementationDef

