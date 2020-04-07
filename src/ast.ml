
type op = Add | Sub | Equal | Neq | Less | And | Or

type typ = 
	| Bool of bool
	| Int of int 
	| Uint of int 
	| Address of string
	| UNIT of unit
	| Mapstruct of string * string

type param = 
		Var of string * typ

type params =
		Paras of param list 

type expr =
	| NumLit of int 
	| BooLit of bool
	| StrLit of string
	| AddressLit of string
	| Id of string
	| TypeAssign of string * string
	| MapAssign of string * typ * typ 
	| MapAssigns of string * typ list * typ 
	| PointAssign of string * expr
	| Event of string * params
  | Binop of expr * op * expr
	| MethodDecl of string * params * expr list 
	| ConstructorDef of string * param * expr list 
	| End_sep of string
(* 
type methodbody = 
	| Guard of expr list
	| Storage of expr list
	| Effects of expr list
	| Returns of expr list
 *)

type decls = 
	| Interfacebody of expr list
	| Implementation of expr list 


type program = decls list


(*  need change *)
(* type assignmentexpr = 
	  MapAssign of string * typ list * typ 
	| VarAssign of string * typ
	| TypeAssign of typ * typ
	| PointAssign of string * string
	| EventAssign of string * string * typ list

type param = 
		Var of string * typ

type params =
		Paras of param list *)

(*  need change *)
(* type expr =
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
	| Consturctor of string * params * body
	| Method of string * params * expr * body list



type interfacesDef = 
	| Stroagedef_interface of lit * typ
	|	Mapdef_interface of lit * typ list * typ list
	| Eventdef_interface of lit * lit * typ list
	| Constructordef_interface of string * typ * typ
	| Methoddef_interface of string * typ list * typ

type program = interfacesDef list * implementationDef *)

