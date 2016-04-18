(* Session types:
    - Formal global
    - Formal local
    - Binary
*)
open ScribbleAST
open Core.Std
open PP

type role_name = string
  [@@deriving show]

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
  | `FLTReceive of (role_from * (message * local_type) list)
  | `FLTMu of (name * local_type)
  | `FLTRecVar of string
  | `FLTEnd
] [@@deriving show]

(* Binary session types *)
type binary_session_type = [
  | `STMu of (name * binary_session_type)
  | `STSend of (message * binary_session_type)
  | `STReceive of (message * binary_session_type)
  | `STOffer of (name * binary_session_type) list
  | `STChoose of (name * binary_session_type) list
  | `STRecVar of name
  | `STEnd
] [@@deriving show]

(* Binary Duality *)
let rec dualof = function
  | `STMu (n, bty) -> `STMu (n, (dualof bty))
  | `STSend (msg, cont) -> `STReceive (msg, dualof cont)
  | `STReceive (msg, cont) -> `STSend (msg, dualof cont)
  | `STOffer xs -> `STChoose (List.map ~f:(fun (lbl, cont) -> (lbl, dualof cont)) xs)
  | `STChoose xs -> `STOffer (List.map ~f:(fun (lbl, cont) -> (lbl, dualof cont)) xs)
  | `STRecVar n -> `STRecVar n
  | `STEnd -> `STEnd


(* Pretty printing *)
(* Global types *)
let roles_doc roles_to =
    text("[") ^^ (doc_concat (text ",") (List.map ~f:text roles_to)) ^^ text("] ")

let msg_doc label payloads =
    let payloads_doc = doc_concat (text ", ") (List.map ~f:text payloads) in
    (text (label ^ ", [")) ^^ payloads_doc ^^ (text "]")

let rec pp_global_type = function
  | `FGTBranch (from, interactions) ->
      let interaction_docs = List.map ~f:pp_branch_interaction interactions in
      text(from ^ " → (")
        ^^ nest 2 ((doc_concat break interaction_docs))
        ^^ text(")") ^^ break
  | `FGTMu (mu_name, gt) -> (text ("μ " ^ mu_name ^ ". ")) ^^ (pp_global_type gt)
  | `FGTRecVar x -> text(x)
  | `FGTEnd -> text("end")
and pp_branch_interaction (roles_to, (name, payloads), gt) =
    (roles_doc roles_to) ^^ (msg_doc name payloads) ^^ (nest 2 (break ^^ (pp_global_type gt)))

(* Local types *)

let rec pp_local_type = function
  | `FLTSend interactions ->
      let pp_send (roles_to, (name, payloads), cont) =
        text "(" ^^ roles_doc roles_to ^^ msg_doc name payloads ^^ text ") . " ^^
        (nest 2 (break ^^ (pp_local_type cont))) in
      let body =
        List.map ~f:pp_send interactions
        |> doc_concat break in
      text "!<" ^^ (nest 2 (break ^^ body)) ^^ text ">"
  | `FLTReceive (from, interactions) ->
      let pp_recv ((name, payloads), cont) =
        (text "(") ^^ (msg_doc name payloads) ^^ text ") . " ^^
        (nest 2 (break ^^ (pp_local_type cont))) in
      let body = List.map ~f:pp_recv interactions |> doc_concat break in
      (text (from ^ "?(")) ^^ (nest 2 (break ^^ body)) ^^ (text ")")
  | `FLTMu (mu_name, lt) -> (text ("μ " ^ mu_name ^ ". ")) ^^ (pp_local_type lt)
  | `FLTRecVar x -> text(x)
  | `FLTEnd -> text("end")

