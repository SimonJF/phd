(* Scribble Lexer *)
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
  "do", 	DOKW
]

let whitespace = (\t|\s|\r|\n)+
let comment = /\*.*\*/
let line_comment = //[^\n\r]*\r?\n
