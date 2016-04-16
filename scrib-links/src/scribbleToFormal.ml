open ScribbleAST
open SessionTypes

let rec join_types : global_type -> global_type -> global_type = fun t1 t2 ->
  match t1, t2 with
    | `FGTBranch (from, gis), t2 ->
        let joined_gis = List.map (fun (rs, msg, gt) ->
          (rs, msg, join_types gt t2)) gis in
        `FGTBranch (from, joined_gis)
    | (`FGTMu (mu1, t1)), t2 -> `FGTMu (mu1, (join_types t1 t2))
    | (`FGTRecVar rv), _ -> `FGTRecVar rv
    | `FGTEnd, t2 -> t2


 (* Translate a block of interactions into a "formal" global type,
 * in particular the one in Hu & Yoshida (2016).
 * In this form, we don't distinguish between message sends / choices,
 * and we have a merged form for all continuations on branches.
 * This should make generating binary types easier. *)
let rec scrib_to_formal : global_interaction_block -> global_type = function
  | [] -> `FGTEnd
  | (`GlobalMessageTransfer (msg, name_from, names_to)) :: xs ->
      let xs_gt = scrib_to_formal xs in
      let interaction = (name_from, [(names_to, msg, xs_gt)]) in
      `FGTBranch interaction
  | (`GlobalChoice (n, gibs)) :: xs ->
      (* Firstly, get a global type for the continuation *)
      let xs_gt = scrib_to_formal xs in
      (* Making the assumption that the GT is well-formed, we have that each
       * branch has the same FromRole. *)
      let from_role = get_from_role gibs in
      (* Next, we have a bunch of different blocks. In Scribble, the first
       * interaction communicates the choice.
       * Our global type is of the form
       *   FromRole -> (ToRoles, Message, Continuation)
       * so we want to consolidate the blocks into this form. *)
      let fgt_interactions =
        List.map choice_to_branch gibs
      (* Next, we need to join each block with the continuation *)
        |> List.map (fun (names_to, msg, gt) ->
             let gt_joined = join_types gt xs_gt in
             (names_to, msg, gt_joined)) in
      (* Finally, we can package it all up into a branch construct *)
      `FGTBranch (from_role, fgt_interactions)
  | ((`GlobalRecursion (mu_name, gib)) :: xs) ->
      let xs_gt = scrib_to_formal xs in
      join_types (`FGTMu (mu_name, (scrib_to_formal gib))) xs_gt
  | ((`GlobalContinue mu_name) :: _) ->
      (* Can assume no interactions follow continue due to
       * tail-recursive requirement *)
      `FGTRecVar mu_name
  | (`GlobalParallel _) :: _ -> failwith "Parallel unsupported at present"
  | (`GlobalInterruptible _) :: _ -> failwith "Interruptible unsupported"
  | (`GlobalDo _) :: _ -> failwith "Do unsupported at present"
(* Translate a choice block to a `FGTBranch by inspecting first interaction *)
and choice_to_branch : global_interaction_block -> fgt_branch_interaction = function
  | [] -> failwith "Empty choice block!?"
  | `GlobalMessageTransfer (msg, name_from, names_to) :: xs ->
      (names_to, msg, scrib_to_formal xs)
  | _ -> failwith "Ill-formed choice block: first interaction isn't a message"
and get_from_role : global_interaction_block list -> role_from = fun blocks ->
  let block1 = List.hd blocks in
  match block1 with
  | (`GlobalMessageTransfer (_, from_role, _)) :: _ -> from_role
  | _ -> failwith "Ill-formed choice block: first interaction isn't a message"
