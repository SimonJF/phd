(* Scribble AST in OCaml *)

type name = string
type alias = string
type ext_name = string
type src = string
type payload_ty = string

type import = [
  | `ImportBasic of name
  | `ImportAlias of (name * alias)
  | `ImportMember of (name * name)
  | `ImportMemberAlias of (name * name * alias)
]

type role = [
  | `RoleDecl of name
  | `RoleDeclAlias of alias
  | `SelfRoleDecl of name
]

type role_instantiation = [
  | `RoleInstantiation of name
  | `RoleInstantiationNew of name
  | `RoleInstantiationAlias of (name * alias)
]

type param = [
  | `TypeParam of name
  | `TypeParamAlias of name * alias
  | `SigParam of name
  | `SigParamAlias of name * alias
]

type argument = [
  | `MessageSigArg of message
  | `MessageSigArgAlias of (message * alias)
  | `IdentArg of name
  | `IdentAliasArg  of (name * alias)
]

type payload = (payload_ty * ext_name * src * name)

(* Message: operator name, and a list of payloads *)
type message = (name * payload list)
type from_name = name
type to_names = name list

type argument_instantiation = message

type protocol = [
  | `GlobalProtocol of global_protocol
  | `LocalProtocol of local_protocol
]

(* === Global types === *)

(* Message transfer: a message, sender name, and list of receivers *)
type message_transfer = (message * from_name * to_names)

type global_interrupt = (message * name)

(* Global interaction: the "meat" of the protocol *)
type global_interaction = [
  | `GlobalMessageTransfer of unit
  | `GlobalChoice of (name * interaction_block list)
  | `GlobalRecursion of (name * global_interaction_block)
  | `GlobalContinue of name
  | `GlobalParallel of global_interaction_block list
  | `GlobalInterruptible of (global_interaction_block * name option * (global_interrupt list))
  | `GlobalDo of (name * (argument_instantiation list) option * role_instantiation list)
]
and global_interaction_block = global_interaction list

type global_protocol = (name * param list * role list * global_interaction_block)



(* === Local Types === *)

type local_interaction = [
  | `LocalSend of (message * to_names)
  | `LocalReceive of (message * from_name)
  | `LocalChoice of (name * local_interaction_block list)
  | `LocalRecursion of (name * local_interaction_block)
  | `LocalContinue of name
  | `LocalParallel of local_interaction_block list
  (* | `LocalInterruptible of (local_interaction_block * local_interaction_block) *)
  | `LocalDo of (name * (argument_instantiation list) option * role_instantiation list)
]
and local_interaction_block = local_interaction list

type local_protocol = (name * param_list * role list * local_interaction_block)

type scribble_module = (name * import list * payload list * protocol list)
