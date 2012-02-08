/* file: insert.mly */

%{
open Printf
open List
open Global
let value_list = ref []
let name_list = ref []
let arguments = ref ""

(* generate the arguments of INSERT function *)
let gen_arguments () =
        let argument_list = StringSet.elements !op_argument_set in
        for i = 0 to (length argument_list)-1
        do
                arguments := " ( " ^ (List.nth argument_list i) ^ ": list tupleType_"
                ^ (List.nth argument_list i) ^ " ) " ^ !arguments;
        done;
        !arguments

(* generate precondition for the INSERT operation, returns the string
 * expression of the precondition *)
let gen_precondition table = 
        let precondition = ref "" in
        for i = 0 to (length !assertion_list)-1
        do
                precondition := !precondition ^ fst ( nth !assertion_list i );
                let argument_list = StringSet.elements ( snd ( nth !assertion_list i ) ) in
                for j = 0 to (length argument_list)-1
                do
                        precondition := !precondition ^ " " ^ ( nth argument_list j );
                done;
                (* precondition := !precondition ^ ( nth !assertion_list i ) ^ "
                 * " ^ table; *)
                if i < (length !assertion_list)-1 then
                        precondition := !precondition ^ " /\\ "
        done;
        !precondition

(* generate postcondition for the INSERT operation, returns the string
 * expression of the postcondition *)
let gen_postcondition table = 
        let postcondition = ref "" in
        for i = 0 to (length !assertion_list)-1
        do
                postcondition := !postcondition ^ fst ( nth !assertion_list i );
                let argument_list = StringSet.elements ( snd ( nth !assertion_list i ) ) in
                for j = 0 to (length argument_list)-1
                do
                        if ( nth argument_list j ) = table then
                                postcondition := !postcondition ^ " result"
                        else
                                postcondition := !postcondition ^ " " ^ ( nth argument_list j );
                done;
                if i < (length !assertion_list)-1 then
                        postcondition := !postcondition ^ " /\\ "
        done;
        !postcondition

(* generate the default attribute name list if it is not explicitly specified in
 * the INSERT operation *)
let gen_df_name_list table =
        let attribute_list = Hashtbl.find tupleType_list table in
        for i = 0 to (length attribute_list)-1
        do
                name_list := append !name_list [ (nth attribute_list i).attribute_name ]
        done

(* default new column without specifying the column names*)
let rec new_column_dft vlist =  
        match vlist with
        | v1::[] -> "att = " ^ v1
        | v1::v2 -> "att = " ^ v1 ^ "; " ^ ( new_column_dft v2 )
        | [] -> ""

let new_column vlist nlist =  
        let new_column_string = ref "" in
        (*
        for i = 0 to (length nlist)-2 
        do
                new_column_string := !new_column_string ^ ( nth nlist i ) ^ " = "
                ^ ( nth vlist i ) ^ "; "
        done;
        new_column_string := !new_column_string ^ (nth nlist ( ( length nlist )-1 ) ) ^ " = "
        ^ (nth vlist ( ( length nlist )-1 ) );
        *)
        for i = 0 to (length nlist)-1
        do
                new_column_string := !new_column_string ^ ( nth nlist i ) ^ " = " ^ ( nth vlist i );
                if i < (length nlist)-1 then
                        new_column_string := !new_column_string ^ "; "
        done;
        !new_column_string
%}


/* Ocamlyacc Declarations */
%token ENDOFSQL 
%token INSERT INTO
%token <string> STR
%token LPAREN RPAREN
%token COMMA
%token VALUES


%start input
%type <unit> input
%type <unit> column_value_list
%type <unit> column_name_list

/* Grammar follows */
%%
input:	
    | /* empty */	{ }
	| input line	
    {
            arguments := "";
            value_list := [];
            name_list := [];
    }
;
line: 
    | ENDOFSQL    { }
    | insert_stmt ENDOFSQL 
    { 
            (* printf "insert!\n"; *)
            operation_exp := !operation_exp ^ $1;
            (* printf "insert: %s\n" !operation_exp *)
    }
;
insert_stmt: 
    | INSERT INTO table_name VALUES LPAREN column_value_list RPAREN
    {
            gen_df_name_list $3;
            (* "let insert ( " ^ $3 ^ ": list tupleType_" ^ $3 ^ " ) =\n" *)
            "let insert " ^ gen_arguments () ^ " = \n"
            ^ "{ " ^ gen_precondition $3 ^ " }\n"
            ^ $3 ^ " ++ " ^ "( Cons {| " ^ new_column !value_list !name_list
            ^ " |} Nil)" ^ "\n"
            ^ "{ " ^ gen_postcondition $3 ^ " }\n"
            (*
            "let insert ( " ^ $3 ^ ": list tupleType_" ^ $3 ^ " ) =\n"
            ^ "{ assertion " ^ $3 ^ " }\n"
            ^ $3 ^ " ++ " ^ "( Cons {| " ^ new_column_dft !value_list
            ^ " |} Nil)" ^ "\n"
            ^ "{ assertion result }\n"
            *)
    }
    | INSERT INTO table_name LPAREN column_name_list RPAREN VALUES LPAREN column_value_list RPAREN
    {
            (* "let insert ( " ^ $3 ^ ": list tupleType_" ^ $3 ^ ") =\n" *)
            "let insert " ^ gen_arguments () ^ " = \n"
            ^ "{ " ^ gen_precondition $3 ^ " }\n"
            ^ $3 ^ " ++ " ^ "( Cons {| " ^ new_column !value_list !name_list ^ " |} Nil)" ^ "\n"
            ^ "{ " ^ gen_postcondition $3 ^ " }\n"
    }
;
column_name_list: 
    | column_name 
    { 
            name_list := append !name_list [$1]; 
    }
    | column_name_list COMMA column_name 
    { 
            name_list := append !name_list [$3]; 
    }
;
column_name: STR { $1 }
;
table_name: STR { $1 }
;
column_value_list: 
    | column_value 
    {
            value_list := append !value_list [$1];
            (* iter (printf "vl: %s\n") !value_list; *)
    }
    | column_value_list COMMA column_value
    { 
            value_list := append !value_list [$3]; 
    }
;
column_value: STR { $1 }

%%
