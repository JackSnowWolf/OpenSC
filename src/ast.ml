

type program = Interfaces * Implementation

type typ = 
	  Bool
	| Int of int 
	| Uint of int 
	| ID of string

type lit =
	| NumLit of int 
	| BooLit of bool 

type expr =
	| ID of string
	| MethodCall of string * expr 

type id = expr 

