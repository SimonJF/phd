(* Scribble AST in OCaml *)

type name = string
  [@@deriving show]

type alias = string
  [@@deriving show]
type ext_name = string
  [@@deriving show]
type src = string
  [@@deriving show]

type payload_ty = string
  [@@deriving show]

(* Message: operator name, and a list of payload types *)
type message = (name * payload_ty list)
  [@@deriving show]

type import = [
  | `ImportBasic of name
  | `ImportAlias of (name * alias)
  | `ImportMember of (name * name)
  | `ImportMemberAlias of (name * name * alias)
] [@@deriving show]

type role = [
  | `RoleDecl of name
  | `RoleDeclAlias of (name * alias)
  | `SelfRoleDecl of name
] [@@deriving show]

type role_instantiation = [
  | `RoleInstantiation of name
  | `RoleInstantiationNew of name
  | `RoleInstantiationAlias of (name * alias)
] [@@deriving show]

type param = [
  | `TypeParam of name
  | `TypeParamAlias of name * alias
  | `SigParam of name
  | `SigParamAlias of name * alias
] [@@deriving show]

type argument = [
  | `MessageSigArg of message
  | `MessageSigArgAlias of (message * alias)
  | `IdentArg of name
  | `IdentAliasArg  of (name * alias)
] [@@deriving show]

type payload = (payload_ty * ext_name * src * name)
  [@@deriving show]

type from_name = name
  [@@deriving show]

type to_names = name list
  [@@deriving show]

type argument_instantiation = message
  [@@deriving show]

(* === Global types === *)

(* Message transfer: a message, sender name, and list of receivers *)
type message_transfer = (message * from_name * to_names)
  [@@deriving show]

type global_interrupt = (message list * name)
  [@@deriving show]

(* Global interaction: the "meat" of the protocol *)
type global_interaction = [
  | `GlobalMessageTransfer of message_transfer
  | `GlobalChoice of (name * global_interaction_block list)
  | `GlobalRecursion of (name * global_interaction_block)
  | `GlobalContinue of name
  | `GlobalParallel of global_interaction_block list
  | `GlobalInterruptible of (global_interaction_block * name option * (global_interrupt list))
  | `GlobalDo of (name * (argument_instantiation list) * role_instantiation list)
]
  [@@deriving show]
and global_interaction_block = global_interaction list
  [@@deriving show]

type global_protocol = (name * param list * role list * global_interaction_block)
  [@@deriving show]



(* === Local Types === *)

type local_interaction = [
  | `LocalSend of (message * to_names)
  | `LocalReceive of (message * from_name)
  | `LocalChoice of (name * local_interaction_block list)
  | `LocalRecursion of (name * local_interaction_block)
  | `LocalContinue of name
  | `LocalParallel of local_interaction_block list
  (* | `LocalInterruptible of (local_interaction_block * local_interaction_block) *)
  | `LocalDo of (name * (argument_instantiation list) * role_instantiation list)
] [@@deriving show]
and local_interaction_block = local_interaction list
  [@@deriving show]

type local_protocol = (name * param list * role list * local_interaction_block)
  [@@deriving show]

type protocol = [
  | `GlobalProtocol of global_protocol
  | `LocalProtocol of local_protocol
] [@@deriving show]

type scribble_module = (name * import list * payload list * protocol list)
  [@@deriving show]

