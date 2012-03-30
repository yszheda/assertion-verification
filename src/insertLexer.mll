(* file: insertLexer.mll *)
(* Lexical analyzer for SQL INSERT statement.
 * It skips all blanks and tabs, and unknown characters
 * and raises End_of_file on EOF. *)
{
    open Insert
    open Char
    open String
}
let digit = ['0'-'9']
let chr = ['a'-'z' 'A'-'Z']
let chr_num = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
    | ';'   { ENDOFSQL }
    | [' ' '\t' '\n']   { token lexbuf }
    | "INSERT"  { INSERT }
    | "INTO" { INTO }
    | "VALUES" { VALUES }
    | '('   { LPAREN }
    | ')'   { RPAREN }
    | "+"   { PLUS }
    | "-"   { MINUS }
    | ","   { COMMA }
    | "'"   { SINGLEQUOTE } 
    | "\""  { DOUBLEQUOTE }
    | "'" (chr as c) "'" 
        { CHAR (string_of_int (code c)) } 
    | digit+ as num
        { NUM (num) }
    | chr chr_num*
    | chr chr_num* "." chr_num+  as str
        { STR (str) }
    | eof   { raise End_of_file }
