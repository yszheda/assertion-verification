(* file: main.ml *)
open Global
open Printf
open List
(* exception End_of_def *)
let output_string = ref ""

let def_in = 
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin

let handle_tableDef () =
  try
    let lexbuf = Lexing.from_channel def_in in
    while true do
      TableDef.input TableDefLexer.token lexbuf;
    done;
    (*
  with End_of_def ->
    let cin = 
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    while true do
      Assertion.input AssertionLexer.token lexbuf
    done
    *)
  with End_of_file ->
    output_string := !tupleType_exp ^ !assertion_exp ^ !operation_exp

let assertion_in =
  if Array.length Sys.argv > 2
  then open_in Sys.argv.(2)
  else stdin

let handle_assertion () =
  try
    let lexbuf = Lexing.from_channel assertion_in in
    while true do
      Assertion.input AssertionLexer.token lexbuf;
    done;
  with End_of_file ->
    output_string := !tupleType_exp ^ !assertion_exp ^ !operation_exp
      

let handle_insert () =
  try
    let op_in = 
      if Array.length Sys.argv > 3
      then open_in Sys.argv.(3)
      else stdin
    in
    let lexbuf = Lexing.from_channel op_in in
    while true do
      Insert.input InsertLexer.token lexbuf;
    done;
  with 
    | End_of_file ->
    output_string := !tupleType_exp ^ !assertion_exp ^ !operation_exp;
    | Parsing.Parse_error -> ()

let handle_delete () =
  try
    let op_in = 
      if Array.length Sys.argv > 3
      then open_in Sys.argv.(3)
      else stdin
    in
    let lexbuf = Lexing.from_channel op_in in
    while true do
      Delete.input DeleteLexer.token lexbuf;
    done;
  with 
    | End_of_file ->
    output_string := !tupleType_exp ^ !assertion_exp ^ !operation_exp;
    | Parsing.Parse_error -> ()

let handle_update () =
  try
    let op_in = 
      if Array.length Sys.argv > 3
      then open_in Sys.argv.(3)
      else stdin
    in
    let lexbuf = Lexing.from_channel op_in in
    while true do
      Update.input UpdateLexer.token lexbuf;
    done;
  with 
    | End_of_file ->
    output_string := !tupleType_exp ^ !assertion_exp ^ !operation_exp;
    | Parsing.Parse_error -> ()

(* let _ = Printexc.print main () *)
 let _ = 
   (* printf "Please enter the SQL phrase of table definition\n"; *)
   handle_tableDef ();
   (* printf "Please enter the SQL assertion\n"; *)
   handle_assertion ();
   (*
               Hashtbl.iter (fun n vt ->
                printf "%s => " n;
                for i = 0 to (length vt)-1
                do
                let v = (nth vt i) in
                    (printf "%s+%s\n") v.attribute_name v.attribute_type
                  done
                ) tupleType_list ;
    *)
   handle_insert (); 
   handle_delete (); 
   handle_update (); 
   printf "%s\n" !output_string;
   let header = 
     "module Test\n \
      use import int.Int\n \
      use import list.List\n \
      use import list.Mem\n \
      use import list.Append\n"
   in
     output_string := header ^ !output_string;
     output_string := !output_string ^ "\n end";
   let file = 
      if Array.length Sys.argv > 4
      then open_out Sys.argv.(4)
      else open_out "test.mlw" in
   fprintf file "%s\n" !output_string
