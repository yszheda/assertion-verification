(* file: updateLexer.mll *)
(* Lexical analyzer for SQL UPDATE statement.
 * It skips all blanks and tabs, and unknown characters
 * and raises End_of_file on EOF. *)
{
    open Update
    open Char
    open String
}
let digit = ['0'-'9']
let chr = ['a'-'z' 'A'-'Z']
let chr_num = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
    | ';'   { ENDOFSQL }
    | [' ' '\t' '\n']   { token lexbuf }
    | "UPDATE"  { UPDATE }
    | "SET"     { SET }
    | "FROM"    { FROM }
    | "WHERE"   { WHERE }
    | '('   { LPAREN }
    | ')'   { RPAREN }
    | "OR"  { OR }
    | "AND" { AND }
    | "NOT" { NOT }
    | "EXISTS" { EXISTS }
    | "SELECT" { SELECT }
    | "="   { EQU }
    | "<>"  { NEQ }
    | "<"   { LT }
    | "<="  { LEQ }
    | ">"   { GT }
    | ">="  { GEQ }
    | "+"   { PLUS }
    | "-"   { MINUS }
    | "*"   { STAR }
    | "/"   { SLASH }
    | "BETWEEN" { BETWEEN }
    | "IN"  { IN }
    | ","   { COMMA }
    | "."   { DOT }
    | "IS"  { IS }
    | "NULL"{ NULL }
    | "'"   { SINGLEQUOTE } 
    | "\""  { DOUBLEQUOTE }
    | "'" (chr as c) "'" 
        { CHAR (string_of_int (code c)) } 
    | digit+ as num
        { NUM (num) }
    | chr chr_num* as str 
        { STR (str) } 
    | eof   { raise End_of_file }
