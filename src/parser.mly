%{ open Ast
%}

%token SIGNATURE UINTTYPE STROAGE EVENT OF METHOD CONSTRUCTOR ENVIRONMENT GUARD EFFECTS LOGS RETURNS MAP UINTType 
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI PASSIGN COMMA
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
/*  this part need to be revised  */
%token <int> NUMLITERAL 
%token <string> ID ADDRESSLIT END
%token <unit> UNIT 
%token <bool> BooLit
/*  end of the part  */
%token EOF

/*   left .......... left associativity    */
%start program
%type <Ast.program> program

%%

program:
	 Defs EOF {$1}

Defs:
	|InterfacesDefs {$1}
	/* |ImplementationDef {$1} */

/* (owner : Address, spender : Address) */
vdecl_list:
  /*nothing*/ { [] }
  | vdecl COMMA vdecl_list  {  Paras($1 :: $3) }

vdecl:
  ID COLON builtintypename { Var($1, $3) }

InterfacesDefs:
	 { [] }   /* nothing */ 
	|InterfacesDef InterfacesDefs {$1 :: $2} 

InterfacesDef:
	| InterfaceBody {Interfacebody($1)}

builtintypename:
  | BooLit { BooLit{$1} }
	| ADDRESSLIT {AddressLit{$1}}
	| UINTTYPE { int }

InterfaceBody:
	| STROAGE ID COLON builtintypename {TypeAssign (Id($2), $4)}
	/* | MAP ID COLON ADDRESS MAPASSIGN builtintypename {MapAssign $2, $4, $6}
	| EVENT ID ASSIGN ID OF builtintypename {Eventdef_interface $2, $4, $6}
	| CONSTRUCTOR ID COLON builtintypename ARROW builtintypename {Constructordef_interface $2, $4, $6}
	| METHOD ID COLON builtintypename ARROW builtintypename {Methoddef_interface $2, $4, $6}
	| END {End_sep $1}  */




/* ImplementationDef:
	  ImplementationConstructor
	 |ImplementationMethods

ImplementationConstructor:
	| CONSTRUCTOR ID LPAREN inputs RPAREN Body {Consturctor $2, $4, $6}
	
ImplementationMethods:
	 ImplementationMethod 
	|ImplementationMethod ImplementationMethods

ImplementationMethod:
	|METHOD ID inputs Body


inputs:
	| input 
	| input inputs

input:
	| ID COLON type

Body:
	 GUARD exprs
	|ImplementationSTORAGE exprs  
	|EFFECTS exprs 
	|RETURNS exprs 

exprs:
	| expr 
	| expr exprs

expr:
	|ID PASSIGN ID {PointAssign $1, $3} */
