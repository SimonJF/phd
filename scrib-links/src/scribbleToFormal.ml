
let rec join_types : global_type -> global_type -> global_type = fun t1 t2 ->
  match t1, t2 with
    | `FGTBranch (from, gis) -> 
        let joined_gis = List.map (fun (rs, l, pts, gt) ->
          (rs, l, pts, join_types gt t2)) gis in
        `FGTBranch (from, joined_gis)
    | (`FGTMu mu1 gt1), t2 -> `FGTMu mu1 (join_types gt1 gt2)
    | (`FGTRecVar rv), _ -> `FGTRecVar rv
    | `FGTEnd, t2 -> t2
and join_interaction : global_interaction -> global_type -> global_interaction =
  fun (from, branches) gt ->
    (from, (List.map (fun (to_roles, lbl, types, gt_inner) ->
      (to_roles, lbl, types, (join_types gt_inner gt)))))


(* Translate a block of interactions into a "formal" global type,
 * in particular the one in Hu & Yoshida (2016).
 * In this form, we don't distinguish between message sends / choices,
 * and we have a merged form for all continuations on branches.
 * This should make generating binary types easier. *)
let rec scrib_to_formal : global_interaction_block -> global_type = function
  | `GlobalMessageTransfer (message * from_name * to_name) :: xs -> ()
  | `GlobalChoice (n, gibs) :: xs ->
      let xs_gt = scrib_to_formal xs in
      let gibs_gts =
        List.map (fun gib -> join_types (scrib_to_formal gib) xs_gt) gibs in
      `FGTBranch (n, gibs_gts)
  | (`GlobalRecursion mu_name gib) xs ->
      let xs_gt = scrib_to_formal xs in
      join_types (`FGTMu mu_name (scrib_to_formal gib)) xs_gt
  | (`GlobalContinue mu_name) _ -> 
      (* Can assume no interactions follow continue due to
       * tail-recursive requirement *)
      `FGTRecVar rv
  | `GlobalParallel _ :: _ -> failwith "Parallel unsupported at present"
  | `GlobalInterruptible _ :: _ -> failwith "Interruptible unsupported"
  | `GlobalDo _ :: _ -> failwith "Do unsupported at present"

