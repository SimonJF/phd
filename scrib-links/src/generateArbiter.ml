open Ir
open Core.Std
open SessionTypes

type mu_name = string
type process_name = string
type role_chan_map = chan_name String.Map.t
type mu_env = (mu_name * process_name) list


let chan_for_role role_chan_map role =
  match (Map.find role_chan_map role) with
    | Some x -> x
    | None ->
        failwith (sprintf "Unable to generate arbiter: 
          %s not in channel map\n" role)

let sym_env = Util.create_sym_env
let fresh_msg_var () = Util.gen_sym sym_env "m"

let generate_arbiter role_chan_map proto_name gt =
  let sym_env = Util.create_sym_env in
  let gen_ir_proc_name mu_name =
    "Arb" ^ proto_name ^ (Util.gen_sym sym_env mu_name) in

  let generate_receive : string -> string -> ir_process -> ir_process = fun role msg_var cont ->
    let chan = chan_for_role role_chan_map role in
    `IRReceive (chan, msg_var, cont) in

  let rec generate_send_sequence msg_var cont = function
    | [] -> cont
    | role :: roles ->
        let chan = chan_for_role role_chan_map role in
        let send_cont = generate_send_sequence msg_var cont roles in
        `IRSend (chan, msg_var, send_cont) in

  let rec generate_select_sequence lbl cont = function
    | [] -> cont
    | role :: roles ->
        let chan = chan_for_role role_chan_map role in
        let cont_ir = generate_select_sequence lbl cont roles in
        `IRChoose (chan, lbl, cont_ir) in

  let rec generate_arbiter_inner : mu_env -> global_type ->
    (ir_process * (name * ir_process) list) = fun env gt ->

    let rec generate_offer_branches = fun from_role roles ->
    (* : role_name -> role_name list ->
     ((name, ir_process) * (name, ir_process) list) =  *)
      match roles with
        | [] -> ([], [])
        | (roles_to, (lbl, _), cont) :: xs ->
            let payload_msg_var = fresh_msg_var () in
            let chooser_chan = chan_for_role role_chan_map from_role in
            let (cont_ir, procs1) = generate_arbiter_inner env cont in
            let send_seq = generate_send_sequence payload_msg_var cont_ir roles_to in
            let select_seq = generate_select_sequence lbl send_seq roles_to in
            let recv = generate_receive from_role payload_msg_var select_seq in
            let (rest, procs2) = generate_offer_branches from_role xs in
            ((lbl, recv) :: rest, procs1 @ procs2) in

      match gt with
        | `FGTBranch (from, interactions) ->
            let msg_var = fresh_msg_var () in
            (match interactions with
              | [(roles_to, msg, cont)] ->
                  (* Simple send: receive from from role, send
                   * to all recipients *)
                  let (cont_ir, procs1) = generate_arbiter_inner env cont in
                  let send_seq = generate_send_sequence msg_var cont_ir roles_to in
                  ((generate_receive from msg_var send_seq), procs1)

              | xs ->
                  (* Here we have a choice. We offer all of the labels, and in each
                   * branch we convey selects to all recipients, followed by sending
                   * a message with the given payload *)
                  let (offer_branches, procs1) = generate_offer_branches from xs in
                  let from_role = chan_for_role role_chan_map from in
                  (`IRBranch (from_role, offer_branches), procs1))

        | `FGTMu (mu_name, cont) ->
            let proc_name = gen_ir_proc_name mu_name in
            let (ir_proc, processes) =
              generate_arbiter_inner ((mu_name, proc_name) :: env) cont in
            (`IRCall proc_name, ((proc_name, ir_proc) :: processes))
        | `FGTRecVar mu_name ->
            (match List.Assoc.find env mu_name with
              | Some proc_name -> (`IRCall proc_name, [])
              | None -> failwith (sprintf "Unable to generate arbiter: 
                %s is not in mu environment" mu_name))
        | `FGTEnd -> (`IREnd, []) in
  let (arb, arbs) = generate_arbiter_inner [] gt in
  (("Arb" ^ proto_name, arb) :: arbs)
