%{ open Ast
%}

%token SIGNATURE UINTTYPE STROAGE EVENT OF METHOD CONSTRUCTOR ENVIRONMENT GUARD EFFECTS LOGS RETURNS MAP UINTType 
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI PASSIGN COMMA POINT
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK 
%token EQ NEQ LGT ADD SUB MUL DIVIDE AND OR BOOL
%token <int> NUMLITERAL 
%token <string> ID ADDRESSLIT END
%token <string> UNIT 
%token <bool> BooLit
%token EOF

/*   left .......... left associativity maybe need in the future    */
%start program
%type <Ast.program> program

%%

program:
	 defs EOF {$1}

defs:
   /* nothing */ 
	 {( [] )}/* { ([], [], [] )} */
	| interfacedecl defs{$1 :: $2}
	/* | interfacedecl defs { (($1 :: fst $3), snd $3) } */
	/* | constructordecl RBRACE decls { (($1 :: fst $3), snd $3) }
	| methoddecl decls { (fst $2, ($1 :: snd $2)) } */


/* (owner : Address, spender : Address) */
param_list:
  /*nothing*/ { [] }
  | param COMMA param_list  {($1 :: $3) }

param:
  ID COLON builtintypename { Var($1, $3) }

id_ok:
	| ID {Id($1)}

interfacedecl:
	SIGNATURE id_ok LBRACE interfaceBody_list RBRACE
	{
		{
			signaturename = $2;
			interfacebody =  $4
		}
	}


builtintypenames:
	| builtintypename builtintypenames {($1 :: $2) }

builtintypename:
  | BooLit { Bool($1) }
	| ADDRESSLIT {Address($1)}
	/* | UINTTYPE { Int } */

interfaceBody_list:
		{ [] }
	|interfaceBody interfaceBody_list { $1::$2 }

/* TODO builtintypenames !!  */
interfaceBody:
	| STROAGE ID COLON builtintypename SEMI {TypeAssign ($2, $4)}
	| MAP ID COLON builtintypename MAPASSIGN builtintypename SEMI{MapAssign ($2, $4, $6)}
	| EVENT ID ASSIGN ID OF LPAREN builtintypenames RPAREN SEMI {Event ($2, $7)}
	| CONSTRUCTOR ID COLON builtintypename ARROW builtintypename SEMI{Constructorexpr ($2, $4, $6)}
	| METHOD ID COLON builtintypename ARROW builtintypename SEMI{Methodexpr ($2, $4, $6)} 




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