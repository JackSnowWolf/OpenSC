%{ open Ast
%}


(* add some simple CFG for general ideas *)

program:
	 Interfaces
	|Implementation
	|eof

Interfaces:
	|Interfaces
	|Interfaces { (* add something like $1 ... *)}

Interface:
	 InterfaceStroages
	|Maps
	|Events
	|InterfaceConstructor
	|InterfaceMethods

InterfaceStroages:
	 InterfaceStorage 
	|InterfaceStroages { (* add something like $1 ... *)}


Maps: 
	 Map 
	|Maps  { (* add something like $1 ... *)}

Events:
	 Event
	|Events

InterfaceConsturtcor:
	ID Declaration TYPE RETYPES TYPE

InterfaceMethods:
	 InterfaceMethod
	|InterfaceMethods


Implementation:
	  ImplementationContrustor
	 |ImplementationMethods

ImplementationMethods:
	 ImplementationMethod 
	|ImplementationMethods

ImplementationMethod:
	|METHOD ID Declaration Body

Body:
	 GUARD
	|ImplementationSTORAGE 
	|EFFECTS
	|RETURNS
