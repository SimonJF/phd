(* Backend-agnostic intermediate representation of binary session types
 * and processes. *)

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



(* IR Processes.
 * Processes will send values to other processes.
 * What will these values be? They'll be specified by the user on the whole,
 * but sometimes they'll be fixed (especially in the case of the arbiter
 * process).
 *
 * Different types of backend will do things in different ways: for example,
 * in the parameterised monad backend, we won't need to specify the particular
 * session type being used. In the GV / Links backends however, we'll need to
 * keep track of channel names.
 *
 * Each role will have its own session connected to the arbiter. Thus, from
 * a global type, we'll need:
 *
 * - Session types for each role to the arbiter, from the POV of the role
 *   (we can dualise them automatically for the arbiter in any case)
 *
 * - An arbiter process extracted from the session type, interacting with each
 *   session in turn.
 *
 * - A process for each role, with skeleton sends / receives.
 *
 *
 * Thus the structure will be:
 *
 *  - IR: (Role name |-> Session type, Role name |-> process)
 *  - Process; (name * action)
 *  - Action:
 *    | Send chan_name var_name
 *    | Receive chan_name var_name
 *    | Branch chan_name ((label * process) list)
 *    | Choose chan_name label
 *    | Looping ---- Doable, but will have to think more
 *
 *
 *  TODO: We'll probably have to make this a little more complicated, actually:
 *        recursion (at least) and branching (possible) will induce the need to
 *        have separate functions for different recursion scopes.
 *)



type ir_process = [
  | `IRPSend of (chan_name * var_name * ir_process )
  | `IRPReceive of (chan_name * var_name * ir_process )
  | `IRPBranch of (chan_name * ((label * ir_process) list))
  | `IRPChoose of (chan_name * label)
]

type ht_role_sty = binary_session_type String.Hashtbl.t
type ht_role_proc = ir_process String.Hashtbl.t
type ir = (ht_role_sty * ht_role_proc)


(*
 * Things to do:
 *   - Step 1:
 *     - For each role, extract the binary session type (and by extension the
 *       dual for the arbiter)
 *
 *   - Step 2:
 *     - Now that all session types have been extracted, we can generate the arbiter
 *       process from the global type.
 *
 *   - Step 3:
 *     - Now we have the arbiter process generated, we can generate skeleton processes
 *       for each of the roles. In case of selections, we'll take the design decision to
 *       expand the *first* choice. In the case of branches, of course we'll have to do
 *       each one.
 *)





(* Symbol environment. Given a role name and recursion branch name,
 * generates a unique session type name. *)
let symEnv = object(self)
    val sym_env = Hashtbl.create 30

    method genName role recName =
        try
            let curMax = Hashtbl.find (role, recName) sym_env in
            let newMax = curMax + 1 in
            Hashtbl.replace (role, recName) newMax sym_env;
            recName ^ (string_of_int newMax)
        with Not_found ->
            Hashtbl.replace (role, recName) 0 sym_env;
            recName
    end
    
let genSTName role recName = symEnv#genName role recName

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



