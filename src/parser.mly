%{ open Ast
%}

%token SIGNATURE UINTTYPE STROAGE EVENT OF METHOD CONSTRUCTOR GUARD EFFECTS LOGS RETURNS MAP UINTType STORAGE
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI PASSIGN COMMA POINT
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK 
%token EQ NEQ LGT ADD SUB MUL DIVIDE AND OR BOOL LGTEQ RGTEQ RGT
%token INT
%token <int> NUMLITERAL 
%token <string> ID ADDRESSTYPE END STRLIT UINTTYPE
%token <string> UNIT ENVIRONMENT
%token <bool> BooLit
%token EOF

%start program
%type <Ast.program> program

%%

program:
	 defs EOF {$1}

/* defs:
	interfacedecls defs {$1 :: $2, $2, $2}
	| constructordecl  { $1 }
	| methoddecls defs  {$1 :: $2} */


/* | interfacedecl defs{$1 :: $2} */
defs: 
   /* nothing */ 
	 { ([], [] )} 
	| interfacedecl defs { (($1 :: fst $2), snd $2) }
	| implementationdecl defs { (fst $2, ($1 :: snd $2)) }

implementationdecl:
	constructordecl methoddecls
	{
		{
			consturctor = $1;
			methods =  $2
		}
	}
/* (owner : Address, spender : Address) */
param_list:
  /*nothing*/ { [] }
	| param { [$1] }
  | param COMMA param_list  { $1 :: $3 }


arg_list:
  /*nothing*/ { [] }
	| argument { [$1] }
  | argument COMMA arg_list  { $1 :: $3 }

argument:
	| ID {Id($1)}

param:
  ID COLON type_ok { Var($1, $3) }

id_ok:
	| ID {Id($1)}
	| ENVIRONMENT {EnvLit($1)}

interfacedecl:
	SIGNATURE id_ok LBRACE interfaceBody_list RBRACE
	{
		{
			signaturename = $2;
			interfacebody =  $4
		}
	}

/* typ:
    INT   { Int  }
  | BOOL  { Bool } */

types_ok:
  /*nothing*/ { [] }
	|	type_ok {[$1]}
	| type_ok COMMA types_ok { $1 :: $3 }

/* do we need to map in type?  */
type_ok:
    INT   { Int  }
   | UINTTYPE { Uint($1) }
   | BOOL  { Bool }
   | ADDRESSTYPE {Address($1)}
   | UNIT { Void($1) }


literal:
  | BooLit { BooLit($1) }
	| NUMLITERAL { NumLit($1) }
	| STRLIT { StrLit($1) }

interfaceBody_list:
		{ [] }
	|interfaceBody interfaceBody_list { $1::$2 }

/* TODO types !!  */
interfaceBody:
	| STORAGE ID COLON type_ok SEMI {TypeAssign (Id($2), $4)}
	| MAP ID COLON LPAREN types_ok RPAREN MAPASSIGN types_ok SEMI{MapAssign (Id($2), Mapstruct($5, $8))}
	| EVENT ID ASSIGN ID OF LPAREN types_ok RPAREN SEMI {Event ($2, $7)}
	| CONSTRUCTOR ID COLON type_ok ARROW type_ok SEMI{Constructorexpr ($2, $4, $6)}
	| METHOD ID COLON LPAREN types_ok RPAREN ARROW type_ok SEMI{Methodexpr (Id($2), $5, $8)} 


constructordecl:
	CONSTRUCTOR id_ok LPAREN param_list RPAREN LBRACE STORAGE constructor_bodylist RETURNS type_ok SEMI RBRACE
	{
		{
			name = $2;
			params = $4;
			consturctor_body = $8;
			return_type = $10;
		}
	}
constructor_bodylist:
		{ [] }
	|constructor_body constructor_bodylist { $1::$2 }

constructor_body:
	| id_ok PASSIGN id_ok SEMI {PointAssign($1, $3)}
	| id_ok LBRACK id_ok POINT id_ok RBRACK PASSIGN id_ok SEMI {EnvironmentAssign($1, $3, $5, $8)}

methoddecls:
		{ [] }
	|	methoddecl methoddecls {$1 :: $2 }

methoddecl:
	METHOD id_ok LPAREN param_list RPAREN LBRACE 
	GUARD LBRACE guard_bodylist RBRACE 
	STORAGE LBRACE storage_bodylist RBRACE
	EFFECTS LBRACE effects_bodylist RBRACE
	RETURNS type_ok SEMI RBRACE
	{
		{
			methodname = $2;
			params = $4;
			guard_body = $9;
			storage_body = $13;
			effects_body = $17;
			returns = $20;
		}
	}

guard_bodylist:
		{ [] }
	|guard_body guard_bodylist { $1::$2 }
		
guard_body:
	   | id_ok LGT NUMLITERAL SEMI { Binop($1, LGT, NumLit($3)) }
		 | id_ok EQ NUMLITERAL SEMI { Binop($1, Equal, NumLit($3)) }
		 /* | id_ok POINT id_ok NUMLITERAL SEMI {ENVRbinop} */
		 | id_ok RGT NUMLITERAL SEMI  { Binop($1, RGT, NumLit($3)) }
		 | id_ok LGTEQ NUMLITERAL SEMI { Binop($1, LGTEQ, NumLit($3)) }
		 | id_ok RGTEQ NUMLITERAL SEMI { Binop($1, RGTEQ, NumLit($3)) }
		 | id_ok POINT id_ok LGT NUMLITERAL SEMI { EnvironmentBinop($1, $3, LGT, NumLit($5)) }

storage_bodylist:
		{ [] }
	|storage_body storage_bodylist { $1::$2 }

storage_body:
	| id_ok PASSIGN id_ok SEMI {PointAssign($1, $3)}
	| id_ok LBRACK id_ok POINT id_ok RBRACK PASSIGN id_ok SEMI {EnvironmentAssign($1, $3, $5, $8)}

effects_bodylist:
		{ [] }
	|effects_body effects_bodylist { $1::$2 }

effects_body:
	| LOGS id_ok LPAREN arg_list RPAREN SEMI { Logexpr($2, $4) }