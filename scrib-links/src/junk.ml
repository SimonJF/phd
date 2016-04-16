
(* Step 1 *)
(* Outer Function: Global Type -> Role -> [(BranchName, SessionType)] *)
let genBin gt role = gen_bin_inner gt role []


(* Inner Function: Global interaction list -> Role -> 
 *  [(RecName, SessionTyName)] -> [(RecName, MaxIndex)] ->
 *  (SessionType, [(TyName, SessionType)]) *)
let rec genBinInner gis role recEnv =
    (* GI -> (SessionType, [(RecName, MaxIndex)],
     * [(TyName, SessionType)] *)
    let genBinGI gi gis =
        match gi with
            | `GlobalMessageTransfer (msg, fromName, toNames) ->
                    (* If the role is involved in the GMT, then add the
                     * relevant session type fragment. Otherwise continue
                     * projecting. *)
                    if fromName == role then
                        (* TODO: Is there a way to make this tail-recursive? *)
                        let (st_cont, stEnv) =
                            genBinInner gis role recEnv in
                        (`STSend (`STBaseTy msgName) st_cont, stEnv)
                    else if (List.mem role toNames) then
                        let (st_cont, stEnv) =
                            genBinInner gis role recEnv recMaxEnv in
                        (`STReceive (`STBaseTy msgName) st_cont, stEnv)
                    else genBinInner gis role recEnv recMaxEnv
            | `GlobalChoice (chooser, gis) -> [] 
            | `GlobalRecursion (recName, recGis) ->
                    let newSTName = genSTName role recName in
                    let (newST, newSTs) = genBinInner gis role in
                    (* Scribble global protocols are tail-recursive, so
                     * there shouldn't be anything after the recursion block? *)
                    (`STRecTyVar newSTName, (newSTName, newST) :: newSTs)


            | `GlobalContinue recName -> []
            | `GlobalParallel gis ->
                    failwith "SJF TODO: This should be possible to support"
            | `GlobalInterruptible (gis, nameOpt, interrupts) ->
                    failwith "Not implemented -- not a priority, but possibly could be supported?"
            | `GlobalDo (protocolName, argInsts, roleInsts) ->
                    failwith "SJF TODO: This is a Very Useful Thing To Have" in
    match gis with
        | [] -> []
        | (x :: xs) -> genBinGI x r branchNames



