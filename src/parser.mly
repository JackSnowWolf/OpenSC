%{ open Ast
%}

%token SIGNATURE END STROAGE EVENT OF METHOD CONSTRUCTOR ENVIRONMENT GUARD EFFECTS LOGS RETURNS MAP
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI PASSIGN
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
/*  this part need to be revised  */
%token <string> NUMLITERAL UINTType
%token <string> ID ADDRESS
%token <unit> UNIT 
%token <bool> BooLit
/*  end of the part  */
%token EOF

/*   left .......... left associativity    */
%start program
%type <Ast.program> program

%%

program:
	 InterfacesDefs ImplementationDef eof { $1 }

InterfacesDefs:
	 { [] }   /* nothing */ 
	|InterfacesDef InterfacesDefs {$1 :: $2} /* I am not sure need to confirmed! */

InterfacesDef:
	| InterfaceBody
	| END {End $1}

InterfaceBody:
	| STROAGE ID COLON typ {Stroagedef_interface $2, $4}
	| MAP ID COLON ADDRESS MAPASSIGN typ {MapAssign $2, $4, $6}
	| EVENT ID ASSIGN ID OF typ {Eventdef_interface $2, $4, $6}
	| CONSTRUCTOR ID COLON typ ARROW typ {Constructordef_interface $2, $4, $6}
	| METHOD ID COLON typ ARROW typ {Methoddef_interface $2, $4, $6}

typ:
    INT { Int }
  | BOOL { Bool }
  | STRING { String }
	| UNIT { Unit }

ImplementationDef:
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
	|ID PASSIGN ID {PointAssign $1, $3}
