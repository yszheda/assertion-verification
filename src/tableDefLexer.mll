(* file: tableDefLexer.mll *)
(* Lexical analyzer for SQL "CREATE TABLE" phrase.
 * It skips all blanks and tabs, and unknown characters
 * and raises End_of_def on EOF. *)
{
    open TableDef
    (* exception End_of_def *)
}
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
    | ';'   { ENDOFSQL }
    | [' ' '\t' '\n']   { token lexbuf }
    | "CREATE"  { CREATE }
    | "TABLE" { TABLE }
    | '('   { LPAREN }
    | ')'   { RPAREN }
    | ","   { COMMA }
    | "INTEGER"   { INTEGER }
    | "SMALLINT"  { SMALLINT }
    | "FLOAT"     { FLOAT }
    | "NUMERIC"   { NUMERIC }
    | "BOOLEAN"   { BOOLEAN }
    | "CHAR"      { CHAR }
    | chr+ as str { STR (str) }
    | _     { token lexbuf }
    | eof   
    { 
            (* raise End_of_def *)
            raise End_of_file
    }
