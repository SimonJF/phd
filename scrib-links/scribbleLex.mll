(* Scribble Lexer *)
{

open Lexing

let keywords = [
  "module", 	MODULEKW;
  "import", 	IMPORTKW;
  "type", 	TYPEKW;
  "protocol", 	PROTOCOLKW;
  "global", 	GLOBALKW;
  "local", 	LOCALKW;
  "role", 	ROLEKW;
  "self", 	SELFKW;
  "sig", 	SIGKW;
  "instantiates", 	INSTANTIATESKW;
  "as", 	ASKW;

  "from", 	FROMKW;
  "to", 	TOKW;
  "choice", 	CHOICEKW;
  "at", 	ATKW;
  "or", 	ORKW;
  "rec", 	RECKW;
  "continue", 	CONTINUEKW;
  "par", 	PARKW;
  "and", 	ANDKW;
  "interruptible", 	INTERRUPTIBLEKW;
  "with", 	WITHKW;
  "by", 	BYKW;
  "throws", 	THROWSKW;
  "catches", 	CATCHESKW;
  "do", 	DOKW;
  "new",   NEWKW
]


}

let whitespace = ('\t'|' '|'\r'|'\n')+
let comment = "/*"(_)*"*/"
let line_comment = "//"[^'\n''\r']*'\r'?'\n'
let ident = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*
let integer = (['1'-'9'] ['0'-'9']* | '0')

rule lex ctxt nl = parse
  | eof     {END}
  | line_comment {lex ctxt nl lexbuf}
  | comment         {lex ctxt nl lexbuf}
  | ident as var    { try List.assoc var keywords
                      with Not_found | NotFound _ ->
                        IDENT var
                    }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '('      { LEFT_BRACKET }
  | ')'      { RIGHT_BRACKET }
  | '['      { LEFT_SQUARE_BRACKET }
  | ']'      { RIGHT_SQUARE_BRACKET }
  | ':'      { COLON }
  | '/'      { FORWARD_SLASH }
  | '\\'     { BACK_SLASH }
  | '.'      { DOT }
  | ','      { COMMA }
  | '#'      { HASH }
  | '&'      { AMPERSAND }
  | '?'      { QUESTION_MARK }
  | '!'      { EXLAMATION_MARK }
  | '_'      { UNDERSCORE }
  | ';'      { SEMICOLON }
  | '<'      { LESS_THAN }
  | '>'      { GREATER_THAN }


