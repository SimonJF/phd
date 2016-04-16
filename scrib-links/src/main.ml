(* Simple test skeleton for now: takes the name of a Scribble
 * global protocol, lexes and parses, and then prints the AST
 * representation *)
open Printf
open Core.Std
open ScribbleAST
open SessionTypes
open ScribbleToFormal

let parse_and_print name =
  let file = In_channel.create name in
  protect ~f:(fun () ->
    let file_contents = In_channel.input_all file in
    printf "File:\n%s\n" file_contents;
    let lexbuf = Lexing.from_string file_contents in
    try
      let parsed = Parser.scribble_module Lexer.token lexbuf in
      printf "AST:\n%s.\n" (ScribbleAST.show_scribble_module parsed)
    with
    (* | Lexer.Error msg ->
        Printf.eprintf "%s%!" msg *)
    | Parsing.Parse_error ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.eprintf "At line %d, token %s: syntax error.\n%!" line tok (* (Lexing.lexeme_start lexbuf) *)
  )
  ~finally: (fun () -> In_channel.close file)

let main () =
  let argv = Array.to_list Sys.argv in
  match argv with
    | [] -> failwith "impossible"
    | [_] -> printf "Syntax: ./scribbleToLinks <scribble global protocol file>\n";
             exit (-1)
    | _ :: proto_name :: _ -> parse_and_print proto_name

let () = main ()


