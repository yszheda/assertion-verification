
{
    open Insert
}
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
    | ';'   { ENDOFSQL }
    | [' ' '\t' '\n']   { token lexbuf }
    | "INSERT"  { INSERT }
    | "INTO" { INTO }
    | "VALUES" { VALUES }
    | '('   { LPAREN }
    | ')'   { RPAREN }
    | ","   { COMMA }
    | chr+
    | chr+ "." chr*  as str
        { STR (str) }
    | eof   { raise End_of_file }
