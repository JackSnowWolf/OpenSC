{open Parser}

let digits = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  	[' ' '\t' '\r' '\n'] { token lexbuf }
  	|'-' 				 {comment 1 lexbuf} (* comment *)
  	|"/-"				 {multicomment 1 lexbuf} (* multiple comment *)
  	| '('             	 { LPAREN }
	| ')'             	 { RPAREN }
	| '{'             	 { LBRACE }
	| '}'             	 { RBRACE }
	| '['             	 { LBRACK }
	| ']'             	 { RBRACK }
	| ';'				 { SEMI }
	| ':'             	 { COLON }
	| "->"				 { ARROW }
	| "|->"				 { PASSIGN }
	| '='				 { ASSIGN }
	| "end"				 { END }
	| "storage"			 { STROAGE }
	| "event"			 { EVENT }
	| "map"				 { MAP }
	| "signature"		 { SIGNATURE }		
	| "UInt"			 { UINT }
	| "of"				 { OF }
	| "Address"			 { ADDRESS }
	| ':'             	 { COLON }
	| "True"          	 { BLIT(true)  }
	| "False"         	 { BLIT(false) }
	| "Bool"          	 { BOOL }
	| "method"			 { METHOD }
	| "constructor"		 { CONSTRUCTOR }
	| "Env"				 { ENVIRONMENT }
	| '.'				 { POINT }
	| "()"				 { UNIT }
	| "=="				 { EQ }
	| "!="				 { NEQ }
	| "guard"			 { GUARD }
	| "effects"			 { EFFECTS }
	| "logs"			 { LOGS }
	| "returns"			 { RETURNS }
	| ">"				 { LGT }
	(* IF ? *)
	(* WHILE ? *)
	| digit+ as lem  { LITERAL(int_of_string lem) }
	| letter (digit | letter | '_')* as lem { ID(lem) }
	| eof { EOF }

	and comment lvl = parse
	  "\n"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf  }
	| _     { comment lvl lexbuf }

	and multicomment lvl = parse
	  "-/"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf  }
	| _     { multicomment lvl lexbuf }