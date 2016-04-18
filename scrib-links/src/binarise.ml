open Core.Std
open ScribbleAST
open SessionTypes

(* Symbol environment. Given a role name and recursion branch name,
 * generates a unique session type name. *)
let symEnv = object(self)
    val sym_env = Hashtbl.Poly.create ()

    method genName role rec_name =
      let cur_max_opt = Hashtbl.find sym_env (role, rec_name) in
      match cur_max_opt with
        | Some cur_max ->
            let new_max = cur_max + 1 in
            Hashtbl.set ~key:(role, rec_name) ~data:new_max sym_env;
            rec_name ^ (string_of_int new_max)
        | None ->
            Hashtbl.set ~key:(role, rec_name) ~data:0 sym_env;
            rec_name
    end
let gen_st_name proto_name role rec_name =
  proto_name ^ "_" ^ symEnv#genName role rec_name

let binarise proto_name role_name lty =
  let rec binarise_inner ty_name tys env = function
    | `FLTSend interactions ->
        (match interactions with
           | [(_, msg, cont)] ->
               `STSend (msg, (binarise_inner ty_name tys env cont))
           | xs ->
               (* Generate a choose guarded by each label, followed by a send *)
               let binarise_interaction (_, (lbl, payloads), cont) =
                 (* We need to be smarter about this function. This should be an accumulator for a fold *)
                 (lbl, `STSend ((lbl, payloads), binarise_inner ty_name tys env cont)) in
               (* This needs to be a fold!!!!!
                * We need the ENVIRONMENT to be different (which will be handled by Mu)
                * but we absolutely need the types to be tracked by this.
                *)
               `STChoose (List.map ~f:binarise_interaction interactions))
    | `FLTReceive (from, interactions) ->
        (match interactions with
           | [(msg, cont)] ->
               `STReceive (msg, (binarise_inner ty_name tys env))
           | xs ->
               (* Generate a branch for each label, followed by a receive *)
               let binarise_interaction (_, (lbl, payloads), cont) =
                 (lbl, `STReceive ((lbl, payloads), binarise_inner ty_name tys env cont)) in
               `STOffer (List.map ~f:binarise_interaction interactions))
    | `FLTMu (mu_name, cont) ->
        let st_name = (gen_st_name proto_name role_name mu_name) in
        let ty = `STMu (st_name, binarise_inner st_name tys ((mu_name, st_name) :: env)) in
        (`STRecVar st_name, ty :: tys)
    | `FLTRecVar x ->
        let st_name_opt = List.assoc.find tys x in
        (match st_name_opt with
           | Some st_name -> `STRecVar st_name
           | None -> failwith ("Rec var " ^ x ^ " not in environment"))
    | `FLTEnd -> `STEnd in
  binarise_inner proto_name [] []

