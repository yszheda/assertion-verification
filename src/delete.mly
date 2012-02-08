/* file: delete.mly */

%{
(* open Str *)
open Printf
open List
open Global
let binders = ref ""
let target_table = ref ""
(* let value_list = ref [""] *)
(* column name list *)
let col_name_list = ref []
let arguments = ref ""
let rec_arguments = ref ""
let table_list = ref []
(* type column = { table_name: string; attribute_name: string } *)
(* the set of corresponding columns *)
(* let cor_column_list = ref [] *)
let cor_column_set = ref StringSet.empty
(* argument set of the multi-table DELETE function *)
let multi_argument_set = ref StringSet.empty
(* argument string of the corresponding multi-table DELETE function *)
let multi_arguments = ref ""
let multi_delete_arg = ref ""
let multi_delete_arg_set = ref StringSet.empty
let multi_delete_fun = ref ""

(* generate the arguments of DELETE function *)
let gen_arguments () =
        arguments := "";
        let argument_list = StringSet.elements !op_argument_set in
        for i = 0 to (length argument_list)-1
        do
                arguments := " ( " ^ (List.nth argument_list i) ^ ": list tupleType_"
                ^ (List.nth argument_list i) ^ " ) " ^ !arguments;
        done;
        !arguments

(* generate the caller's arguments of the recursive DELETE function *)
let gen_rec_arguments target_table =
        rec_arguments := "";
        let argument_list = StringSet.elements !op_argument_set in
        for i = 0 to (length argument_list)-1
        do
                if (List.nth argument_list i) = target_table then
                        rec_arguments := " l" ^ !rec_arguments
                else
                        rec_arguments := " " ^ (List.nth argument_list i) ^ !rec_arguments;
        done;
        !rec_arguments

let gen_caller_arg arg_set old_arg new_arg =
        let caller_arguments = ref "" in
        let argument_list = StringSet.elements !arg_set in
        for i = 0 to (length argument_list)-1
        do
                let current_arg = (nth argument_list i) in
                if current_arg = old_arg then
                begin
                        caller_arguments := " " ^ new_arg ^ !caller_arguments;
                        arg_set := StringSet.remove old_arg !arg_set;
                        arg_set := StringSet.add new_arg !arg_set
                end
                else
                        caller_arguments := " " ^ current_arg ^ !caller_arguments;
        done;
        !caller_arguments

let remove_cor_table table = 
        let cor_column_list = StringSet.elements !cor_column_set in
        for i = 0 to (length cor_column_list)-1
        do
                let cor_column = (nth cor_column_list i) in
                if Str.string_match ( Str.regexp table ) cor_column 0 = true
                then cor_column_set := StringSet.remove cor_column !cor_column_set 
        done


let gen_multi_argument_set table = 
        remove_cor_table table;
        (* multi_argument_set := StringSet.union !op_argument_set !cor_column_set; *)
        multi_argument_set := !cor_column_set;
        multi_argument_set := StringSet.add table !multi_argument_set;
        multi_argument_set := StringSet.add !target_table !multi_argument_set

let gen_multi_arguments table =
        multi_arguments := "";
        (* gen_multi_argument_set table; *)
        let argument_list = StringSet.elements !multi_argument_set in
        for i = 0 to (length argument_list)-1
        do
                (* printf "test_set: %s\n" (nth argument_list i) ; *)
                multi_arguments := " " ^ (nth argument_list i) ^ !multi_arguments;
        done;
        !multi_arguments

(* generate postcondition for the DELETE operation, returns the string
 * expression of the postcondition *)
let gen_postcondition table = 
        let postcondition = ref "( " in
        for i = 0 to (length !assertion_list)-1
        do
                postcondition := !postcondition ^ fst ( nth !assertion_list i );
                let argument_list = StringSet.elements ( snd ( nth !assertion_list i ) ) in
                for j = 0 to (length argument_list)-1
                do
                        postcondition := !postcondition ^ " " ^ ( nth argument_list j );
                done;
                if i < (length !assertion_list)-1 then
                        postcondition := !postcondition ^ " /\\ "
        done;
        postcondition := !postcondition ^ " ) -> ( ";
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
        postcondition := !postcondition ^ " ) ";
        !postcondition

(* generate the default attribute name list if it is not explicitly specified in
 * the DELETE operation *)
let gen_df_col_name_list table =
        col_name_list := [];
        let attribute_list = Hashtbl.find tupleType_list table in
        for i = 0 to (length attribute_list)-1
        do
                col_name_list := append !col_name_list [ (nth attribute_list i).attribute_name ]
        done

(* generate the expression of columns: (<column name> = <column value>)+ *)
let rec gen_column_exp nlist table = 
        match nlist with
        | n1::[] -> n1 ^ " = " ^ table ^ "_" ^ n1 ^ "_value"
        | n1::n2 -> n1 ^ " = " ^ table ^ "_" ^ n1 ^ "_value; " ^ ( gen_column_exp n2 table )
        | [] -> "" 

let gen_iter_funs () = 
        let fun_def = ref "" in
        let pre_fun = ref "" in
        let pre_fun_arg = ref "" in
        let pre_fun_arg_set = ref StringSet.empty in
        let postcondition = ref "" in
        for i = 0 to (length !table_list)-1
        do
                if i = 0 then
                begin
                        pre_fun := !multi_delete_fun;
                        pre_fun_arg := !multi_delete_arg;
                        pre_fun_arg_set := !multi_delete_arg_set;
                end;
                let old_table = 
                        if i = 0 then !target_table
                        else nth !table_list (i-1)
                in
                let table = (nth !table_list i) in
                let fun_name = "iter_" ^ table in
                (*
                let postcondition = 
                        if i < (length !table_list)-1 then "true"
                        else gen_postcondition !target_table
                in
                *)
                let caller_arguments = ref "" in
                gen_df_col_name_list table;
                gen_multi_argument_set table;
                multi_argument_set := StringSet.add old_table !multi_argument_set;
                if i < (length !table_list)-1 then
                begin
                        postcondition := "true"
                end
                else begin
                        postcondition := gen_postcondition !target_table;
                        multi_argument_set := StringSet.union !op_argument_set !cor_column_set;
                end;
                gen_multi_arguments table;
                let arg_set = multi_argument_set in
                caller_arguments := gen_caller_arg arg_set table "l";
                caller_arguments := gen_caller_arg arg_set !target_table 
                ("(" ^ !pre_fun ^ !pre_fun_arg ^ ")");
                (*
                printf "caller_arguments: %s\n" !caller_arguments;
                printf "table: %s\n" table;
                caller_arguments := ( Str.global_replace 
                ( Str.regexp (table^"[^_]") ) "l " ) !multi_arguments;
                printf "caller_arguments: %s\n" !caller_arguments;
                caller_arguments := ( Str.global_replace 
                ( Str.regexp (!target_table^"[^_]") ) (!pre_fun ^ !pre_fun_arg) ) !caller_arguments;
                printf "caller_arguments: %s\n" !caller_arguments;
                *)
                fun_def := !fun_def ^ 
                "let rec " ^ fun_name ^ !multi_arguments ^ " =\n"
                ^ "{ true }\n"
                ^ "match " ^ table ^ " with\n"
                ^ "| Nil -> " ^ !target_table ^ "\n"
                ^ "| Cons {| " ^ gen_column_exp !col_name_list table ^ " |} l -> "
                ^ fun_name ^ !caller_arguments ^ "\n"
                ^ "end\n"
                ^ "{ " ^ !postcondition ^ " }\n";
                pre_fun := fun_name;
                pre_fun_arg := gen_multi_arguments table;
                pre_fun_arg_set := !multi_argument_set;

        done;
        !fun_def


%}


/* Ocamlyacc Declarations */
%token ENDOFSQL 
%token DELETE FROM WHERE
%token USING
%token <string> STR
%token LPAREN RPAREN
%token OR AND
%token NOT
%token EXISTS
%token SELECT
%token EQU NEQ LT LEQ GT GEQ
%token PLUS MINUS STAR SLASH
%token BETWEEN
%token IN
%token COMMA
%token DOT
%token IS NULL


%start input
%type <unit> input

/* Grammar follows */
%%
input:
    | /* empty */	{ }
	| input line	
    {
            arguments := "";
            rec_arguments := "";
            col_name_list := [];
            table_list := [];
    }
;
line: 
    | ENDOFSQL    { }
    | delete_stmt ENDOFSQL 
    {
            operation_exp := !operation_exp ^ $1; 
    }
;
delete_stmt: 
    | DELETE FROM table_name
    {
            gen_df_col_name_list $3;
            "let rec delete " ^ gen_arguments () ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $3 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ (gen_column_exp !col_name_list $3) ^ " |} l -> (delete" 
            ^ gen_rec_arguments $3 ^ ")\n"
            ^ "end\n"
            ^ "{ " ^ gen_postcondition $3 ^ " }\n"
    }
    | DELETE FROM table_name WHERE search_condition
    {
            gen_df_col_name_list $3;
            "let rec delete " ^ gen_arguments () ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $3 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ (gen_column_exp !col_name_list $3) ^ " |} l -> if " ^ $5 
            ^ "then (delete" ^ gen_rec_arguments $3 ^ ")\n"
            ^ "else Cons {| " ^ (gen_column_exp !col_name_list $3) ^ " |} (delete" ^ gen_rec_arguments $3 ^ ")\n"
            ^ "end\n"
            ^ "{ " ^ gen_postcondition $3 ^ " }\n"
    }
    | DELETE FROM target_table_name USING table_reference_list WHERE search_condition
    {
            gen_df_col_name_list $3;
            gen_multi_argument_set $3;
            gen_multi_arguments $3;
            multi_delete_arg := !multi_arguments;
            multi_delete_arg_set := !multi_argument_set;
            multi_delete_fun := "delete_" ^ $3;
            (* let caller_arguments = ( Str.global_replace ( Str.regexp ($3^"[^_]") ) "l " ) in *)
            let caller_arguments = gen_caller_arg multi_delete_arg_set $3 "l" in
            let delete_fun = "let rec " ^ !multi_delete_fun ^ !multi_delete_arg ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $3 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ (gen_column_exp !col_name_list $3) ^ " |} l -> if " ^ $7
            ^ "then (" ^ !multi_delete_fun ^ caller_arguments ^ ")\n"
            ^ "else Cons {| " ^ (gen_column_exp !col_name_list $3)
            ^ " |} (" ^ !multi_delete_fun ^ caller_arguments ^ ")\n"
            ^ "end\n"
            ^ "{ true }\n" in
            delete_fun ^ gen_iter_funs () 
    }
;
target_table_name: STR
    {
            target_table := $1;
            $1
    }
;
table_reference_list: 
    /*
    | table_name COMMA table_reference_list
    {
            if $1 <> !target_table then
                    table_list := append !table_list [$1]
    }
    */
    | table_reference_list COMMA table_name
    {
            if $3 <> !target_table then
                    table_list := append !table_list [$3]
    }
    | table_name 
    {
            if $1 <> !target_table then
                    table_list := append !table_list [$1]
    }
;
/* use left recursion */
search_condition:   
    | boolean_term    { $1 }
    | search_condition OR boolean_term  { $1 ^ " || "^ $3 }
;
boolean_term:   
    | boolean_factor  { $1 }
    | boolean_term AND boolean_factor   { $1 ^ " && " ^ $3 }
;
boolean_factor:  
    | predicate       { $1 }
    | LPAREN search_condition RPAREN    { " ( " ^ $2 ^ " ) " }
    | NOT LPAREN search_condition RPAREN    { " not ( " ^ $3 ^ " ) " }
;
predicate:  
 /* | exists_predicate      { $1 } */
    | comparison_predicate  { $1 }
    | between_predicate     { $1 }
    | in_predicate          { $1 }
    | null_predicate        { $1 }
;
/*
exists_predicate: 
    | EXISTS LPAREN SELECT STAR FROM table_list WHERE search_condition RPAREN 
    { "exists " ^ !binders ^ $6 ^ " /\\ " ^ $8 }
    | NOT EXISTS LPAREN SELECT STAR FROM table_list WHERE search_condition RPAREN 
    { "not ( exists " ^ !binders ^ $7 ^ " /\\ " ^ $9 ^ " ) " }
;
table_list: 
    | table_name tuple_name COMMA table_list 
    {
            binders := $2 ^ ": tupleType_" ^ $1 ^ ", " ^ !binders;
            " mem " ^ $2 ^ " " ^ $1 ^ " /\\ " ^ $4
    }
    | table_name tuple_name 
    {
            binders := $2 ^ ": tupleType_" ^ $1 ^ "." ^ !binders;
            " mem " ^ $2 ^ " " ^ $1
    }
;
*/
table_name: STR { $1 }
;
tuple_name: STR { $1 }
;
comparison_predicate: 
    | exp EQU exp   { $1 ^ " = " ^ $3 }
    | exp NEQ exp   { $1 ^ " <> " ^ $3 }
    | exp LT exp    { $1 ^ " < " ^ $3 }
    | exp LEQ exp   { $1 ^ " <= " ^ $3 }
    | exp GT exp    { $1 ^ " > " ^ $3 }
    | exp GEQ exp   { $1 ^ " >= " ^ $3 }
;
exp: 
    | term { $1 }
    | exp PLUS term { $1 ^ " + " ^ $3 }
    | exp MINUS term { $1 ^ " - " ^ $3 }
;
term: 
    | factor { $1 }
    | term STAR factor  { $1 ^ " * " ^ $3 }
    | term SLASH factor { $1 ^ " / " ^ $3 }
;
factor: 
    | LPAREN exp RPAREN { " ( " ^ $2 ^ " ) " }
    | constant { $1 }
    | column { $1 }
;
constant: 
    | STR   { $1 }
    | PLUS STR  { $2 }
    | MINUS STR { "-" ^ $2 }
;
column: 
    | table_name DOT attribute_name 
    {
            if $1 != !target_table then
                    cor_column_set := StringSet.add ($1 ^ "_" ^ $3 ^ "_value") !cor_column_set;
                    (* cor_column_list := append !cor_column_list [ { table_name
                     * = $1; attribute_name = $3 } ]; *)
            $1 ^ "_" ^ $3 ^ "_value" 
    }
    | PLUS table_name DOT attribute_name 
    { 
            if $2 <> !target_table then
                    cor_column_set := StringSet.add ($2 ^ "_" ^ $4 ^ "_value") !cor_column_set;
                    (* cor_column_list := append !cor_column_list [ { table_name
                     * = $2; attribute_name = $4 } ]; *)
            $2 ^ "_" ^ $4 ^ "_value" 
    }
    | MINUS table_name DOT attribute_name 
    { 
            if $2 <> !target_table then
                    cor_column_set := StringSet.add ($2 ^ "_" ^ $4 ^ "_value") !cor_column_set;
                    (* cor_column_list := append !cor_column_list [ { table_name
                     * = $2; attribute_name = $4 } ]; *)
            "-" ^ $2 ^ "_" ^ $4 ^ "_value" 
    }
;
attribute_name: STR { $1 }
;
between_predicate: 
    | exp BETWEEN constant AND constant 
    { " ( " ^ $1 ^ " >= " ^ $3 ^ " ) /\\ ( " ^ $1 ^ " <= " ^ $5 ^ " ) " }
    | exp NOT BETWEEN constant AND constant 
    { " ( " ^ $1 ^ " < " ^ $4 ^ " ) \/ ( " ^ $1 ^  " > " ^ $6 ^ " ) " }
;
in_predicate: 
    | exp IN LPAREN in_value_list RPAREN 
    { " mem " ^ $1 ^ " ( " ^ $4 ^ " ) " }
    | exp NOT IN LPAREN in_value_list RPAREN 
    { " not ( mem " ^ $1 ^ " ( " ^ $5 ^ " ) ) " }
;
in_value_list: 
    | constant 
    { " (Cons " ^ $1 ^ " Nil) " }
    | constant COMMA in_value_list 
    { " (Cons " ^ $1 ^ $3 ^ ") " }
;
null_predicate: 
    | column IS NULL { "(Cons " ^ $1 ^ " Nil)" ^ " = Nil " }
    | column IS NOT NULL { "(Cons " ^ $1 ^ " Nil)" ^ " <> Nil " }


%%
