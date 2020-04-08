{open Parser}

let digits = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
   [' ' '\t' '\r' '\n'] 	{ token lexbuf }
  |'-' 				 					{comment 1 lexbuf} (* comment *)
  |"/-"				 					{multicomment 1 lexbuf} (* multiple comment *)
  | '('             	 { LPAREN }
	| ')'             	 { RPAREN }
	| '{'             	 { LBRACE }
	| '}'             	 { RBRACE }
	| '['             	 { LBRACK }
	| ']'             	 { RBRACK }
	(*  General op *)
	| "=="				 			 { EQ }
	| "!="				 			 { NEQ }
	| ">"				 				 { LGT } 
	| "+"				 				 { ADD }
	| "-"				 				 { SUB }
	| "*"				 				 { MUL }
	| "/"				 				 { DIV }
	| "and"				 				 { AND }
	| "or"				 				 { OR }			
	(* end of general ops *)
	(*  Types *)
	| "Int"              { INTTYPE } 
	| "UInt"			 			 { UINTTYPE }
	| "Unit"						 { UNITTYPE }
	| "True"          	 { BOOLit(true)  }
	| "False"         	 { BOOLit(false) }
	| "Bool"          	 { BOOL }
	| "Address"			 		 { ADDRESS }
	| "map"				 			 { MAP } (* as hash table *)
	| "null"				 			 { UNIT(unit) }
	(* end of types *)
	(* type of assignement*)
	| "->"				 			 { ARROW }
	| "|->"				 			 { PASSIGN }
	| "=>"							 { MAPASSIGN }
	| '='				 				 { ASSIGN }
	| ':'             	 { COLON } (* Type declaration *)
	(* end of type of assignments *)
	| '.'				 				 { POINT } (* Point for extract information *)
	| ';'								 { SEMI }
	| ','								 { COMMA } 
	(*  ==========================================================  *)
	| "signature"		 		 { SIGNATURE }		
	(* | "end"				       { END("END") }	separation op *)
	| "storage"			     { STROAGE }
	| "event"			 			 { EVENT }
	| "of"				 			 { OF }
	| "method"			 		 { METHOD }
	| "constructor"		 	 { CONSTRUCTOR }
	| "Env"				 			 { ENVIRONMENT }
	| "guard"			 			 { GUARD }
	| "effects"				 	 { EFFECTS }
	| "logs"			  		 { LOGS }
	| "returns"			 		 { RETURNS }
	| digits+ as lem  { NUMLITERAL(int_of_string lem) }
	| letter (digits | letter | '_')* as lem { ID(lem) }
	| eof { EOF }

	and comment lvl = parse
	  "\n"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf  }
	| _     { comment lvl lexbuf }

	and multicomment lvl = parse
	  "-/"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf  }
	| _     { multicomment lvl lexbuf }