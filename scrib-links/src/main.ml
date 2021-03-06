(* Simple test skeleton for now: takes the name of a Scribble
 * global protocol, lexes and parses, and then prints the AST
 * representation *)
open Printf
open Core.Std
open ScribbleAST
open SessionTypes
open Projection
open Binarise
open Ir
open GenerateArbiter
open Util

type projection_map = local_type String.Map.t

(* Reads a Scribble protocol, parses it, returns the Scribble module.
 * If there's an error, prints it and quits. *)
let parse : string -> scribble_module = fun name ->
  let file = In_channel.create name in
  protect ~f:(fun () ->
    let file_contents = In_channel.input_all file in
    (* printf "File:\n%s\n" file_contents; *)
    let lexbuf = Lexing.from_string file_contents in
    try
      Parser.scribble_module Lexer.token lexbuf
    with
    (* | Lexer.Error msg ->
        Printf.eprintf "%s%!" msg *)
    | Parsing.Parse_error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.sprintf "At line %d, token %s: syntax error.\n%!" line tok |> failwith
  )
  ~finally: (fun () -> In_channel.close file)

let module_to_globals : scribble_module -> global_protocol list =
    fun (_, _, _, protos) ->
  let rec module_to_globals_inner = function
     | [] -> []
     | `GlobalProtocol gp :: xs -> gp :: module_to_globals_inner xs
     | _ :: xs -> module_to_globals_inner xs in
  module_to_globals_inner protos

let global_proto_name : global_protocol -> string =
  fun (name, _, _, _) -> name

let global_proto_to_formal : global_protocol -> global_type =
  fun (_, _, _, gib) -> ScribbleToFormal.scrib_to_formal gib


let global_proto_roles : global_protocol -> role_name list = fun (_, _, roles, _) ->
  List.map ~f:(fun x ->
    match x with
      | `RoleDecl x -> x
      | `RoleDeclAlias (_, x) -> x
      | `SelfRoleDecl x -> x) roles

let generate_projections : global_type -> role_name list -> projection_map =
  fun global_ty roles ->
    printf "Role list: %s\n" (format_list roles);
    let rec generate_projections_map map = function
      | [] -> map
      | role :: roles ->
          let upd_map = (Map.add map ~key:role ~data:(project global_ty role)) in
          generate_projections_map upd_map roles in
    generate_projections_map String.Map.empty roles



let print_projections : projection_map -> unit = fun proj_map ->
  Map.to_alist proj_map
  |> List.iter ~f:(fun (role, ty) ->
       printf "Local type for %s: \n %s \n" role
         (SessionTypes.pp_local_type ty |> PP.pretty 100))

let binarise_local_types : name  -> projection_map ->
    (name * (name * binary_session_type) list) list =
  fun proto_name proj_map ->
    Map.to_alist proj_map
    |> List.map ~f:(fun (role, ty) ->
        (role, Binarise.binarise proto_name role ty))

let print_binary_types : (name * (name * binary_session_type) list) list -> unit = fun xs ->
  xs |>
  List.iter ~f:(fun (role, bin_tys) ->
    printf "Binary types for role %s:\n" role;
    bin_tys |>
    List.iter ~f:(fun (name, bty) ->
      printf "type %s = %s\n" name (SessionTypes.pp_binary_session_type bty |> PP.pretty 100)))

let print_arbiter_processes =
  List.iter ~f:(fun (name, proc) -> printf "%s: %s \n" name (Ir.show_ir_process proc))

let print_global_and_projections : global_protocol -> unit = fun global_proto ->
  let (_, _, roles, _) = global_proto in
  let role_list = global_proto_roles global_proto in
  let global_ty = global_proto_to_formal global_proto in
  let projections = generate_projections global_ty role_list in
  let proto_name = global_proto_name global_proto in
  printf "Global type for %s: \n %s \n"
    proto_name
    (global_proto_to_formal global_proto
     |> SessionTypes.pp_global_type
     |> PP.pretty 100);
  print_projections projections;
  binarise_local_types proto_name projections |> print_binary_types;
  let role_chan_map =
    List.mapi ~f:(fun i role -> (role, ("c" ^ (string_of_int i)))) role_list
    |> Map.of_alist_exn ~comparator:String.comparator in
  generate_arbiter role_chan_map proto_name global_ty |> print_arbiter_processes


let (<<) f g x = f(g(x))

let main () =
  let argv = Array.to_list Sys.argv in
  match argv with
    | [] -> failwith "impossible"
    | [_] -> printf "Syntax: ./scribbleToLinks <scribble global protocol file>\n";
             exit (-1)
    | _ :: proto_name :: _ ->
        let parsed_module = parse proto_name in
        printf "AST:\n%s.\n" (ScribbleAST.show_scribble_module parsed_module);
        List.iter ~f:print_global_and_projections (module_to_globals parsed_module)



let () = main ()


