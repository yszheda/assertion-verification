
{
    open Delete
}
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
rule token = parse
    | ';'   { ENDOFSQL }
    | [' ' '\t' '\n']   { token lexbuf }
    | "DELETE"  { DELETE }
    | "FROM"    { FROM }
    | "WHERE"   { WHERE }
    | "USING"   { USING }
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
    | chr+ as str
(*    | chr+ "." chr*  as str *)
    { STR (str) }
    | eof   { raise End_of_file }
