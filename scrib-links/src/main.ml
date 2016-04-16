(* Simple test skeleton for now: takes the name of a Scribble
 * global protocol, lexes and parses, and then prints the AST
 * representation *)
open Printf
open Core.Std
open ScribbleAST
open SessionTypes

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

let print_formal : global_protocol -> unit = fun global_proto ->
  printf "Global type for %s: \n %s \n"
    (global_proto_name global_proto)
    (global_proto_to_formal global_proto
     |> ScribbleToFormal.pp_global_type
     |> PP.pretty 180)

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
        List.iter (module_to_globals parsed_module) print_formal



let () = main ()


