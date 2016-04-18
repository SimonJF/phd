open SessionTypes
open Core.Std
open Printf
open Util

let rec merge t1 t2 =
  match t1, t2 with
    (* End mergeable with itself *)
    | `FLTEnd, `FLTEnd -> `FLTEnd
    (* Recursion variables must match in RecVar and Mu cases *)
    | `FLTRecVar x, `FLTRecVar y ->
        if x = y then `FLTRecVar x else
          failwith (sprintf "Merge error: recursion variables 
           %s and %s unmergeable" x y)
    | `FLTMu (x, t1_inner), `FLTMu (y, t2_inner) ->
        if x = y then `FLTMu (x, (merge t1_inner t2_inner)) else
          failwith (sprintf "Merge error: recursion variables 
           %s and %s unmergeable" x y)
    (* Sends mergeable only if they're identical *)
    | `FLTSend xs, `FLTSend ys ->
        if xs = ys then t1 else
          failwith (sprintf "Merge error: local send types 
           %s and %s unmergeable" (show_local_type t1) (show_local_type t2))
    (* Receives are slightly more involved.
     * We can have different branches, but if branches share the same label
     * then the types that they guard must be mergeable. *)

    (* Partition both lists: (In other branch set, Not in other branch set)
     * Map merge over first set, set union of second set.*)
    (* Strip continuations from branches *)
    | `FLTReceive (from1, xs), `FLTReceive (from2, ys) ->
      if from1 <> from2 then
        failwith (sprintf "Unable to merge: 'from' roles different (%s, %s)"
          from1 from2)
        else
          let (t1_labels, t2_labels) =
            (List.map ~f:(fun ((lbl, _), _) -> lbl) xs,
             List.map ~f:(fun ((lbl, _), _) -> lbl) ys) in

          let (t1_label_in, t1_label_not_in) =
            List.partition_tf ~f:(fun ((lbl, _), _) -> List.mem t2_labels lbl) xs in
          let (t2_label_in, t2_label_not_in) =
            List.partition_tf ~f:(fun ((lbl, _), _) -> List.mem t1_labels lbl) ys in

          (* We should just be able to sort and zip on the first, as label sets are the same. *)
          let sort_by_label =
            List.sort
              ~cmp:(fun ((lbl1, _), _) ((lbl2, _), _) -> String.ascending lbl1 lbl2) in

          let t1_in_sorted = sort_by_label t1_label_in in
          let t2_in_sorted = sort_by_label t2_label_in in
          let zipped = List.zip_exn t1_in_sorted t2_in_sorted in
          let merged = List.map ~f:check_and_merge zipped in
          `FLTReceive (from1, (merged @ t1_label_not_in @ t2_label_not_in))
    | x1, x2 -> failwith (sprintf "Unable to merge incompatible local types %s\n and \n%s\n"
          (pp_local_type x1 |> PP.pretty 180) (pp_local_type x2 |> PP.pretty 180))
(* Check if the merge is safe, if so, perform the merge.
 * (branch * branch) -> branch *)
and check_and_merge branch_pair =
  let (b1, b2) = branch_pair in
  let ((lbl1, pls1), cont1) = b1 in
  let ((lbl2, pls2), cont2) = b2 in
    (* Invariant to this function is that the labels *must* match *)
    if lbl1 <> lbl2 then
      failwith (sprintf "Internal error: label mismatch in check_and_merge! (%s, %s)"
        lbl1 lbl2)
    else if pls1 <> pls2 then
      failwith (sprintf "Unable to merge %s and %s: 'payload' sets different (%s, %s)"
        lbl1 lbl2 (format_list pls1) (format_list pls2))
    else
      ((lbl1, pls1), (merge cont1 cont2))

let rec project gt role =
  match gt with
    | `FGTBranch (role_from, branches) ->
        if role_from = role then
          project_send role branches
        else
          project_poss_receives role_from role branches
    | `FGTMu (name, gt_inner) -> `FLTMu (name, (project gt_inner role))
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
        | Some (to_roles, _, _) -> List.mem to_roles proj_role
        | None -> failwith "Error: Empty choice branch set" in
  (* Project each receive block, giving us an `FLTReceive *)
  let projected_branches =
      List.map
        ~f:(fun (_, msg, gt) -> (msg, (project gt proj_role))) branches in
  if contained_in_branches then
      `FLTReceive (from_role, projected_branches)
  else
      let projections =
          List.map ~f:(fun (_, _, gt) -> project gt proj_role) branches in
      List.fold_left
        ~f:merge
        ~init:(List.hd_exn projections)
        (List.tl_exn projections)


