%{ open Ast
%}


%token SIGNATURE STROAGE EVENT OF METHOD CONSTRUCTOR ENVIRONMENT LOGS 
%token GUARD EFFECTS RETURNS
%token INTTYPE UINTTYPE MAP BOOL ADDRESS
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI PASSIGN COMMA PLUS MINUS
%token MUL DIV POINT
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
/*  this part need to be revised  */
%token EQ NEQ LT AND OR
%token IF ELSE
%token <int> NUMLITERAL 
%token <string> ID
%token <unit> UNIT 
%token <bool> BOOLit
/*  end of the part  */
%token EOF

/*   left .......... left associativity    */
%start program
%type <Ast.program> program

%right ASSIGN PASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS
%left DIV MUL
%right POINT
%%

program:
	SIGNATURE ID LBRACE InterfacesDefs RBRACE Implements EOF { $2, $4 }

/* (owner : Address, spender : Address) */
vdecl_list:
  { [] } /*nothing*/
  | vdecl COMMA vdecl_list  { $1 :: $3 }

vdecl:
  ID COLON builtintype { bind($1, $3) }

InterfacesDefs:
	 { [] }   /* nothing */ 
	| InterfacesDef InterfacesDefs { $1 :: $2 } 

InterfacesDef:
	InterfaceBody { $1 }

Implements:
	{ [] }
	| Implement Implements { $1 :: $2 }

Implement:
	ImplementationConstructor { $1 }
	| ImplementationMethod 		{ $1 }

builtintype:
	 BOOL	      	{ Bool } 
	| INTTYPE 		{ Int }
	| UINTTYPE 		{ UInt }
	| ADDRESS 		{ Address }
	| MAP			{ Mapstruct }

builtintype_list:
  { [] } /*nothing*/ 
  | builtintype COMMA builtintype_list  { $1 :: $3 }

builtintype_paren:
	builtintype                      	{ $1 }
	| LPAREN builtintype_list RPAREN 	{ $2 }

InterfaceBody:
	STROAGE ID COLON builtintype_paren SEMI					        { StorageInterface ($2, $4) }
	| MAP ID COLON builtintype_paren MAPASSIGN builtintype_paren SEMI		{ MapInterface $2, $4, $6 }
	| EVENT ID ASSIGN ID OF builtintype_paren SEMI					{ EventInterface $2, $4, $6 }
	| CONSTRUCTOR ID COLON builtintype_paren ARROW builtintype SEMI			{ ConstructorInterface $2, $4, $6 }
	| METHOD ID COLON builtintype_paren ARROW builtintype SEMI			{ MethodInterface $2, $4, $6 }

ImplementationConstructor:
	CONSTRUCTOR ID LPAREN vdecl_list RPAREN LBRACE Body RBRACE
	{
		{
			fname: $2;
			formals: $4;
			body: $7;
		}
	}

ImplementationMethod:
	METHOD ID LPAREN vdecl_list RPAREN LBRACE Body RBRACE
	{
		{
			fname: $2;
			formals: $4;
			body: $7;
		}
	}
	| METHOD ID LPAREN vdecl_list RPAREN COLON builtintype LBRACE Body RBRACE
	{
		{
			fname: $2;
			formals: $4;
			body: $9;
		}
	}


Body:
	{ [] }
	| Field Body { $1 :: $2 } 

Field:
	GUARD LBRACE Stmts RBRACE 			{ Guard($3) }
	| STROAGE LBRACE Stmts RBRACE 	{ Storage($3) }
	| EFFECTS LBRACE Stmts RBRACE 	{ Effects($3) }
	| RETURNS lexpr SEMI							{ Returns($2) }

Stmts:
	{ [] }
	| Stmt Stmts { $1 :: $2 }

Stmt:
    lexpr SEMI                               { Expr($1)      }
  | LBRACE Stmts RBRACE                 		{ Block($2) 		}
  /* if (condition) { Block1} else {Block2} */
  /* if (condition) Stmt else Stmt */
  | IF LPAREN lexpr RPAREN Stmt ELSE Stmt    { If($3, $5, $7) }
	| LOGS ID lexpr SEMI												{ Logs($2, $3) 	 }


args_opt:
   { [] }
  | args { $1 }

args:
  lexpr  { [$1] }
  | lexpr COMMA args { $1::$3 }

lexpr:
	expr														{ $1 }
	| UNIT													{ UnitLit($1) }
	| BOOLit												{ BooLit($1) }
	| NUMLITERAL										{	NumLit($1) }
	| expr LPAREN args_opt RPAREN  	{ Call($1, $3) }
	| expr ASSIGN lexpr 							{ Assign($1, $3) }
	| expr PASSIGN lexpr 						{ PointAssign($1, $3) }
	| LPAREN expr RPAREN 						{ $2                   }
	| expr PLUS   lexpr 							{ Binop($1, Add,   $3)   }
  | expr MINUS  lexpr 							{ Binop($1, Sub,   $3)   }
  | expr EQ     lexpr 							{ Binop($1, Equal, $3)   }
  | expr NEQ    lexpr 							{ Binop($1, Neq,	 $3)   }
  | expr LT     lexpr 							{ Binop($1, Less,  $3)   }
  | expr AND    lexpr 							{ Binop($1, And,   $3)   }
  | expr OR     lexpr 							{ Binop($1, Or,    $3)   }
	| expr MUL    lexpr 							{ Binop($1, Mul,   $3)   }
	| expr DIV    lexpr 							{ Binop($1, Div,   $3)   }

expr:
	ID															{ Var($1) }
	| expr LBRACK LPAREN args_opt RPAREN RBRACK { AddressLit($1, $4) }
	| ENVIRONMENT POINT ID				{ Env($3) }

