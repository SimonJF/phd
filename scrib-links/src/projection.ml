open SessionTypes
open Core.Std

let project : global_type -> name -> local_type = fun gt role ->
  match gt with
    | `FGTBranch (role_from, branches) ->
        if role_from = role then
          project_send role branches
        else
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
  let contained_in_branches =
      match List.hd branches with
        | Some br -> List.mem branches from_role
        | None -> failwith "IMPOSSIBLE: Empty choice branch set" in
  (* Project each receive block, giving us an `FLTReceive *)
  let project_receives_inner = 
      List.map 
        (fun (from, msg, gt) -> (from, msg, (project gt proj_role)) in
  if contained_in_branches then
      `FLTReceive (project_receives_inner branches)
  else
      let projections = 
          List.map ~f:(fun (_, _, gt) -> project gt role) branches in
      List.fold_left
        ~f:merge
        ~init:List.hd projections
        List.tl projections

let merge b1 b2 = failwith "unimplemented"
