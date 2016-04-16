open SessionTypes

let project : global_type -> name -> local_type = fun gt role ->
  match gt with
    | `FGTBranch (role_from, branches) ->
        if role_from = role then
          project_send role branches
        else
          (* If we're not the sender, we may or may not be a recipient in
           * one of the branches. Check each branch, if we're in it, then
           * we have a receive, if not, then project the inner GT.
           * We assume there's already been a WF check, therefore that we
           * can assume that there exists a merge-based projection.
           *)
          project_poss_receives role branches
    | `FGTMu name gt_inner -> `FLTMu name (project name gt_inner)
    | `FGTRecVar rec_var -> `FLTRecVar rec_var
    | `FGTEnd -> `FLTEnd
and project_send role branches =
  let projected_branches =
    List.map ~f:(fun (to_roles, msg, gt) ->
      (to_roles, msg, project gt role)) branches in
  `FLTSend projected_branches
and project_poss_receives from_role proj_role branches =
  (* We're either in *all* of the receive sets, or *none* of them.
   * If we're in all of them, project a receive, with merge-based
   * projection of each continuation.
   * If we're in none of them, just project the merge of all branches.
   * I think, anyway? We can change if this is too restrictive. *)
  let project_receives_inner = function
    [] -> []
    (roles_to, msg, gt) :: xs ->
      if List.mem roles_to proj_role then
        `FLTReceive (from_role, msg, project gt) :: (project_receives_inner xs)
      else
        project gt :: (project_receives_inner xs) in
  project_receives_inner branches

