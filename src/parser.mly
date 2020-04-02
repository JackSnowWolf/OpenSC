%{ open Ast
%}

%token SIGNATURE END STROAGE EVENT OF METHOD CONSTRUCTOR ENVIRONMENT GUARD EFFECTS LOGS RETURNS MAP
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
(*  this part need to be revised  *)
%token <string> NUMLITERAL UINTType
%token <string> ID ADDRESS
%token <unit> UNIT 
%token <bool> BooLit
(*  end of the part  *)
%token EOF

(*   left .......... left associativity    *)
%start program
%type <Ast.program> program

%%

program:
	 InterfacesDefs ImplementationDef eof { $1 }

InterfacesDefs:
	 { [] }   /* nothing */ 
	|InterfacesDef InterfacesDefs {$1 :: $2} (* I am not sure need to confirmed! *)

InterfacesDef:
	| SIGNATURE InterfaceBody END {InterfaceDef $2} (* need change.*)

InterfaceBody:
	(*  should line32 be STROAGE ID COLON UINTType however I think type is nonterimal. need talk *)
	| STROAGE ID COLON typ {TypeAssign $2, $4} (* should we have a type called typeIdentifier ?? *)
	| MAP ID COLON ADDRESS MAPASSIGN typ
	| EVENT ID ASSIGN ID OF typ (* need typeIdentifiers for multiple type. leave now *)
	| CONSTRUCTOR ID COLON typ ARROW typ
	| METHOD ID COLON typ ARROW typ(*  change to typeIdentifiers later *)

/* typ:
          INT { Int }
        | BOOL { Bool }
        | STRING { String }
	| UNIT { Unit }

ImplementationDef:
	  ImplementationConstructor
	 |ImplementationMethods

ImplementationConstructor:
	| CONSTRUCTOR ID LPAREN ID COLON typ RPAREN
	|
	
ImplementationMethods:
	 ImplementationMethod 
	|ImplementationMethods

ImplementationMethod:
	|METHOD ID Declaration Body

Body:
	 GUARD
	|ImplementationSTORAGE 
	|EFFECTS
	|RETURNS */
