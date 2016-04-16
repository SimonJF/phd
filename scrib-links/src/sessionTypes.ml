(* Session types:
    - Formal global
    - Formal local
    - Binary
*)
open ScribbleAST

type role_to = string
  [@@deriving show]

type role_from = string
  [@@deriving show]

type base_ty = string
  [@@deriving show]

type name = string
  [@@deriving show]

type label = string
  [@@deriving show]

type char_name = string
  [@@deriving show]

type var_name = string
  [@@deriving show]

(* "Formal" global types *)
type global_type = [
  | `FGTBranch of role_from * fgt_branch_interaction list
  | `FGTMu of (name * global_type)
  | `FGTRecVar of string
  | `FGTEnd
] [@@deriving show]

and fgt_branch_interaction =
    (role_to list * message * global_type)
  [@@deriving show]


(* Local types *)
type local_type = [
  | `FLTSend of (role_to list * message * local_type) list
  | `FLTReceive of (role_from * message * local_type) list
  | `FLTMu of (name * local_type)
  | `FLTRecVer of string
  | `FLTEnd
] [@@deriving show]

(* Binary session types *)
type binary_session_type = [
  | `STBaseTy of base_ty
  | `STMu of (name * binary_session_type)
  | `STSend of (binary_session_type * binary_session_type)
  | `STReceive of (binary_session_type * binary_session_type)
  | `STOffer of (name * binary_session_type) list
  | `STChoose of (name * binary_session_type) list
  | `STRecTyVar of name
  | `STEnd
] [@@deriving show]

(* Binary Duality *)
let rec dualof = function
  | `STBaseTy bty -> `STBaseTy bty
  (* TODO: Fix the recursion stuff once I understand it *)
  | `STMu (n, bty) -> `STMu (n, (dualof bty))
  | `STSend (payload, cont) -> `STReceive (payload, dualof cont)
  | `STReceive (payload, cont) -> `STSend (payload, dualof cont)
  | `STOffer xs -> `STChoose ((List.map (fun (lbl, cont) -> (lbl, dualof cont))), xs)
  | `STChoice xs -> `STOffer ((List.map (fun (lbl, cont) -> (lbl, dualof cont))), xs)
  | `STRecTyVar n -> `STRecTyVar n
  | `STEnd -> `STEnd


