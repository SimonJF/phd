%{

open ScribbleAST

%}

%token MODULEKW
%token IMPORTKW
%token TYPEKW
%token PROTOCOLKW
%token GLOBALKW
%token LOCALKW
%token ROLEKW
%token SELFKW
%token SIGKW
%token INSTANTIATESKW
%token ASKW

%token FROMKW
%token TOKW
%token CHOICEKW
%token ATKW
%token ORKW
%token RECKW
%token CONTINUEKW
%token PARKW
%token ANDKW
%token INTERRUPTIBLEKW
%token WITHKW
%token BYKW
%token THROWSKW
%token CATCHESKW
%token DOKW
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_SQUARE_BRACKET
%token RIGHT_SQUARE_BRACKET
%token COLON
%token FORWARD_SLASH
%token BACK_SLASH
%token DOT
%token COMMA
%token HASH
%token AMPERSAND
%token QUESTION_MARK
%token EXLAMATION_MARK
%token UNDERSCORE
%token SEMICOLON
%token LESS_THAN
%token GREATER_THAN
%token NEWKW
%token EOF

%token <string> IDENT

%start scribble_module
%type <ScribbleAST.scribble_module> scribble_module


%%

identifier:
  IDENT { $1 }
;

modulename:
| identifier module_ident_inners   { $1 ^ $2 }
;

module_ident_inners:
| { "" }
| module_ident_inner module_ident_inners   { $1 ^ $2 }
;

module_ident_inner:
| DOT identifier   { "." ^ $2 }
;

membername:
| simplemembername   { $1 }
| fullmembername   { $1 }
;

simplemembername:
| identifier   { $1 }
;

fullmembername:
| modulename DOT simplemembername   { $1 ^ "." ^ $3 }
;

scribble_module:
| moduledecl importdecls payloadtypedecls protocoldecls EOF { ($1, $2, $3, $4) }
;

importdecls:
|    { [] }
| importdecl importdecls  { $1 :: $2 }
;

payloadtypedecls:
|    { [] }
| payloadtypedecl payloadtypedecls   { $1 :: $2 }
;

protocoldecls:
|    { [] }
| protocoldecl protocoldecls   { $1 :: $2 }
;

moduledecl:
| MODULEKW modulename SEMICOLON   { $2 }
;

importdecl:
| importmodule   { $1 }
| importmember   { $1 }
;

importmodule:
| IMPORTKW modulename SEMICOLON   { `ImportBasic $2 }
| IMPORTKW modulename ASKW identifier SEMICOLON { `ImportAlias ($2, $4) }
;

importmember:
| FROMKW modulename IMPORTKW simplemembername SEMICOLON  {`ImportMember ($2, $4) }
| FROMKW modulename IMPORTKW simplemembername ASKW identifier SEMICOLON
  { `ImportMemberAlias ($2, $4, $6) }
;

payloadtypedecl:
| TYPEKW LESS_THAN identifier GREATER_THAN identifier FROMKW identifier ASKW identifier SEMICOLON
  {  ($3, $5, $7, $9)  }
;

messagesignature:
| LEFT_BRACKET payload RIGHT_BRACKET { ("", $2) }
| identifier LEFT_BRACKET RIGHT_BRACKET { ($1, []) }
| identifier LEFT_BRACKET payload RIGHT_BRACKET { ($1, $3) }
;

payload:
| payloadelement payloadelementlist   { $1 :: $2 }
;

payloadelementlist:
|    { [] }
| COMMA payloadelement payloadelementlist   { $2 :: $3 }
;

payloadelement:
| identifier   { $1 }
| identifier COLON identifier   { $1 ^ ":" ^ $3 }
;

protocoldecl:
| globalprotocoldecl   { $1 }
;

/*
Not considering local protocols at the moment -- we *shouldn't* need to parse them
in this tool
| localprotocoldecl   { $1 }
*/

globalprotocoldecl:
| globalprotocolheader globalprotocoldefinition
  {
    let (name, params, roles) = $1 in `GlobalProtocol (name, params, roles, $2)
  }
;

/*
Fine for now
| globalprotocolheader globalprotocolinstance :
  {Name, Params, Roles} = $1,
  {InstProt, Args, InstRoles} = $2,
  scribble_ast:global_protocol_instance(Name, Params, Roles, InstProt, Args, InstRoles).

  globalprotocolinstance:
| INSTANTIATESKW membername roleinstantiationlist SEMICOLON  { ($2, [], $3) }
| INSTANTIATESKW membername argumentlist roleinstantiationlist SEMICOLON { ($2, $3, $4) }
;

argumentlist:
| LESS_THAN argument argumentlistinner GREATER_THAN { $2 :: $3 }
;

argumentlistinner:
|    { [] }
| COMMA argument argumentlistinner   { $2 :: $3 }
;


argument:
| messagesignature   { `MessageSigArg $1 }
| messagesignature ASKW identifier { `MessageSigArgAlias ($1, $3) }
| identifier   { `IdentArg $1 }
| identifier ASKW identifier { `IdentAliasArg ($1, $3) }
;


*/


globalprotocolheader:
| GLOBALKW PROTOCOLKW identifier roledecllist  { ($3, [], $4) }
| GLOBALKW PROTOCOLKW identifier parameterdecllist roledecllist { ($3, $4, $5) }
;

roledecllist:
| LEFT_BRACKET roledecl roledecllistinner RIGHT_BRACKET   { $2 :: $3 }
;


roledecllistinner:
|    { [] }
| COMMA roledecl roledecllistinner   { $2 :: $3 }
;

roledecl:
| ROLEKW identifier   { `RoleDecl $2 }
| ROLEKW identifier ASKW identifier   { `RoleDeclAlias ($2, $4) }
;

parameterdecllist:
| LESS_THAN parameterdecl parameterdecllistinner GREATER_THAN { $2 :: $3 }
;

parameterdecllistinner:
|    { [] }
| COMMA parameterdecl parameterdecllistinner   { $2 :: $3 }
;

parameterdecl:
| TYPEKW identifier   { `TypeParam $2 }
| TYPEKW identifier ASKW identifier { `TypeParamAlias ($2, $4) }
| SIGKW identifier   { `SigParam $2 }
| SIGKW identifier ASKW identifier  { `SigParamAlias ($2, $4) }
;

globalprotocoldefinition:
| globalprotocolblock   { $1 }
;



roleinstantiationlist:
| LEFT_BRACKET roleinstantiation roleinstantiationlistinner RIGHT_BRACKET { $2 :: $3 }
;

roleinstantiationlistinner:
|    { [] }
| COMMA roleinstantiation roleinstantiationlistinner { $2 :: $3 }
;


roleinstantiation:
| identifier   { `RoleInstantiation $1 }
| NEWKW identifier   { `RoleInstantiationNew $2 }
| identifier ASKW identifier   { `RoleInstantiationAlias ($1, $3) }
;

globalprotocolblock:
| LEFT_BRACE globalinteractionsequence RIGHT_BRACE  { $2 }
;

globalinteractionsequence:
|   { [] }
| globalinteraction globalinteractionsequence  { $1 :: $2 }
;

globalinteraction:
| globalmessagetransfer   { $1 }
| globalchoice   { $1 }
| globalrecursion   { $1 }
| globalcontinue   { $1 }
| globalparallel   { $1 }
| globalinterruptible   { $1 }
| globaldo   { $1 }
;

globalmessagetransfer:
| message FROMKW identifier TOKW identifier identifierlist SEMICOLON
  { `GlobalMessageTransfer ($1, $3, $5 :: $6) }
;

identifierlist:
|    { [] }
| COMMA identifier identifierlist   { $2 :: $3 }
;

message:
| messagesignature   { $1 }
| identifier   { ($1, []) }
;

globalchoice:
| CHOICEKW ATKW identifier globalprotocolblock globalchoiceinner { `GlobalChoice ($3, $4 :: $5) }
;

globalchoiceinner:
| { [] }
| ORKW globalprotocolblock globalchoiceinner  { $2 :: $3 }
;

globalrecursion:
| RECKW identifier globalprotocolblock { `GlobalRecursion ($2, $3) }
;

globalcontinue:
| CONTINUEKW identifier SEMICOLON { `GlobalContinue $2 }
;

globalparallel:
| PARKW globalprotocolblock globalparallelinner { `GlobalParallel ($2 :: $3) }
;

globalparallelinner:
|    { [] }
| ANDKW globalprotocolblock globalparallelinner   { $2 :: $3 }
;


globalinterruptible:
| INTERRUPTIBLEKW globalprotocolblock WITHKW LEFT_BRACE globalinterruptlist RIGHT_BRACE
  { `GlobalInterruptible ($2, None, $5) }
| INTERRUPTIBLEKW identifier COLON globalprotocolblock WITHKW LEFT_BRACE globalinterruptlist RIGHT_BRACE
  { `GlobalInterruptible ($4, Some $2, $7) }
;

globalinterruptlist:
|    { [] }
| globalinterrupt globalinterruptlist   { $1 :: $2 }
;

messagelist:
|    { [] }
| COMMA message messagelist   { $2 :: $3 }
;

globalinterrupt:
| message messagelist BYKW identifier SEMICOLON { ($2, $4) }
;

argumentinstantiationlist:
| LESS_THAN argumentinstantiationlistinner GREATER_THAN { $2 }
;

argumentinstantiationlistinner:
|   { [] }
| messagesignature argumentinstantiationlist { $1 :: $2 }
;


globaldo:
| DOKW membername roleinstantiationlist SEMICOLON { `GlobalDo ($2, [], $3) }
/*
| DOKW membername argumentinstantiationlist roleinstantiationlist SEMICOLON
  { `GlobalDo ($2, $3, $4) }
*/
;

%%

