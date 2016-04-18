
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
