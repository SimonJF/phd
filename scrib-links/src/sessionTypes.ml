(* Session types:
    - Formal global
    - Formal local
    - Binary
*)

type role_to = string
type role_from = string

(* "Formal" global types *)
type global_type = [
  | `FGTBranch global_interaction
  | `FGTMu name global_type
  | `FGTRecVar string
  | `FGTEnd
] 
and global_interaction =
    role_from * (role_to list * label * payload_type list * global_type) list


(* Local types *)
type local_type = [
  | `FLTSend (roles_to list * label * payload_type list * local_type) list
  | `FLTReceive (role_from * label * payload_type list * local_type) list
  | `FLTMu name local_type
  | `FLTRecVer string
  | `FLTEnd
]

(* Binary session types *)
type base_ty = string
type name = string
type label = string
type char_name = string
type var_name = string

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
let dualof = function
  | `STBaseTy bty -> `STBaseTy bty
  (* TODO: Fix the recursion stuff once I understand it *)
  | `STMu (n, bty) -> `STMu (n, (dualof bty))
  | `STSend (payload, cont) -> `STReceive (payload, dualof cont)
  | `STReceive (payload, cont) -> `STSend (payload, dualof cont)
  | `STOffer xs -> `STChoose (List.map (fun (lbl, cont) -> (lbl, dualof cont))) xs
  | `STChoice xs -> `STOffer (List.map (fun (lbl, cont) -> (lbl, dualof cont))) xs
  | `STRecTyVar n -> `STRecTyVar n
  | `STEnd -> `STEnd


