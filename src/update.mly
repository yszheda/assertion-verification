/* file: update.mly */

%{
open Printf
open List
open Global
let binders = ref ""
let target_table = ref ""
(* let value_list = ref [""] *)
let col_name_list = ref []
let arguments = ref ""
let rec_arguments = ref ""
let column_exp = ref ""
type attribute = { name: string; value: string }
let update_list = ref []

let table_list = ref []
let cor_column_set = ref StringSet.empty
(* argument set of the multi-table UPDATE function *)
let multi_argument_set = ref StringSet.empty
(* argument string of the corresponding multi-table UPDATE function *)
let multi_arguments = ref ""
let multi_update_arg = ref ""
let multi_update_arg_set = ref StringSet.empty
let multi_update_fun = ref ""

(*
let rec gen_column_exp nlist = 
        match nlist with
        | n1::[] -> column_exp := !column_exp ^ n1 ^ " = " ^ n1 ^ "_value"
        | n1::n2 -> column_exp := !column_exp ^ n1 ^ " = " ^ n1 ^ "_value; " ^ ( gen_column_exp n2 )
        | [] -> column_exp := !column_exp;
        !column_exp
*)

(* generate the arguments of UPDATE function *)
let gen_arguments () =
        let argument_list = StringSet.elements !op_argument_set in
        for i = 0 to (length argument_list)-1
        do
                arguments := " ( " ^ (List.nth argument_list i) ^ ": list tupleType_"
                ^ (List.nth argument_list i) ^ " ) " ^ !arguments;
        done;
        !arguments

(* generate the callor's arguments of the recursive UPDATE function *)
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
        multi_argument_set := !cor_column_set;
        multi_argument_set := StringSet.add table !multi_argument_set;
        multi_argument_set := StringSet.add !target_table !multi_argument_set

let gen_multi_arguments table =
        multi_arguments := "";
        let argument_list = StringSet.elements !multi_argument_set in
        for i = 0 to (length argument_list)-1
        do
                multi_arguments := " " ^ (nth argument_list i) ^ !multi_arguments;
        done;
        !multi_arguments

(* generate postcondition for the UPDATE operation, returns the string
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
                col_name_list := append !col_name_list [ ( nth attribute_list i ).attribute_name ]
        done

(* generate the expression of updated columns: 
 * (<column name> = (<old value>|<new value>) )+ *)
let update_column ulist table =
        let column_new = ref "" and replace_flag = ref false in
        for j = 0 to (length !col_name_list)-1 do
                replace_flag := false;
                for i = 0 to (length ulist)-1 do
                        if (String.compare (nth !col_name_list j) (nth ulist i).name) = 0 then
                        begin
                                column_new := !column_new ^ (nth !col_name_list j) ^
                                " = " ^ (nth ulist i).value;
                                replace_flag := true;
                        end
                done;
                if !replace_flag = false then
                        column_new := !column_new ^ (nth !col_name_list j) ^ " = " ^
                        table ^ "_" ^ (nth !col_name_list j) ^ "_value";
                if j < (length !col_name_list)-1 then
                        column_new := !column_new ^ "; "
        done;
        !column_new 

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
                        pre_fun := !multi_update_fun;
                        pre_fun_arg := !multi_update_arg;
                        pre_fun_arg_set := !multi_update_arg_set;
                end;
                let old_table = 
                        if i = 0 then !target_table
                        else nth !table_list (i-1)
                in
                let table = (nth !table_list i) in
                let fun_name = "iter_" ^ table in
                
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
%token UPDATE SET WHERE
%token <string> STR
%token LPAREN RPAREN
%token OR AND
%token NOT
%token EXISTS
%token SELECT FROM
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
            update_list := [];
            col_name_list := [];
            table_list := [];
    }
;
line: 
    | ENDOFSQL    { }
    | update_stmt ENDOFSQL 
    { 
            operation_exp := !operation_exp ^ $1;
    }
;
update_stmt: 
    | UPDATE table_name SET set_clause_list
    {
            gen_df_col_name_list $2;
            "let rec update " ^ gen_arguments () ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $2 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ ( gen_column_exp !col_name_list $2 ) ^ " |} l -> Cons {| " 
            ^ ( update_column !update_list $2 ) 
            ^ " |} (update" ^ gen_rec_arguments $2 ^ ")\n"
            ^ "end\n"
            ^ "{ " ^ gen_postcondition $2 ^ " }\n"
    }
    | UPDATE table_name SET set_clause_list WHERE search_condition
    {
            gen_df_col_name_list $2;
            (* "let rec update (" ^ $2 ^ ": list tupleType_" ^ $2 ^ " ) =\n" *)
            "let rec update " ^ gen_arguments () ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $2 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ ( gen_column_exp !col_name_list $2 ) ^ " |} l -> if " ^ $6 
            ^ "then Cons {| " ^ ( update_column !update_list $2 ) 
            ^ " |} (update" ^ gen_rec_arguments $2 ^ ")\n"
            ^ "else Cons {| "
            (* ( update_column ) *)
            ^ ( gen_column_exp !col_name_list $2 )
            ^ " |} (update" ^ gen_rec_arguments $2 ^ ")\n"
            ^ "end\n"
            ^ "{ " ^ gen_postcondition $2 ^ " }\n"
    }
    | UPDATE target_table_name SET set_clause_list FROM table_reference_list WHERE search_condition
    {
            gen_df_col_name_list $2;
            gen_multi_argument_set $2;
            gen_multi_arguments $2;
            multi_update_arg := !multi_arguments;
            multi_update_arg_set := !multi_argument_set;
            multi_update_fun := "update_" ^ $2;
            (* let caller_arguments = ( Str.global_replace ( Str.regexp ($2^"[^_]") ) "l " ) in *)
            let caller_arguments = gen_caller_arg multi_update_arg_set $2 "l" in
            let update_fun = "let rec " ^ !multi_update_fun ^ !multi_update_arg ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $2 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ (gen_column_exp !col_name_list $2) ^ " |} l -> if " ^ $8
            ^ "then Cons {| " ^ ( update_column !update_list $2 ) ^ " |} (" 
            (* ^ !multi_update_fun ^ caller_arguments !multi_update_arg ^ ")\n" *)
            ^ !multi_update_fun ^ caller_arguments ^ ")\n"
            ^ "else Cons {| " ^ (gen_column_exp !col_name_list $2)
            ^ " |} (" ^ !multi_update_fun ^ caller_arguments ^ ")\n"
            ^ "end\n"
            ^ "{ true }\n" in
            update_fun ^ gen_iter_funs () 
    }
;
target_table_name: STR
    {
            target_table := $1;
            $1
    }
;
table_reference_list: 
    | table_name COMMA table_reference_list
    {
            if $1 <> !target_table then
                    table_list := append !table_list [$1]
    }
    | table_name 
    {
            if $1 <> !target_table then
                    table_list := append !table_list [$1]
    }
;
table_name: STR { $1 }
;
set_clause_list:
    | set_clause { $1 }
    | set_clause_list COMMA set_clause { $1; $3 } 
;
set_clause: set_column EQU constant
    {
            update_list := append !update_list [{ name = $1; value = $3 }];
            (* 
            for i = 0 to (length !update_list)-1 do
                   printf "name = %s, value = %s\n" (nth !update_list i).name
                   (nth !update_list i).value
            done;
            *)
            $1 ^ "=" ^ $3
    }
;
set_column: 
    | STR { $1 }
    | STR DOT STR { $3 }
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
/*  | exists_predicate      { $1 } */
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
    | term STAR factor { $1 ^ " * " ^ $3 }
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
    | tuple_name DOT attribute_name 
    { 
            if $1 != !target_table then
                    cor_column_set := StringSet.add ($1 ^ "_" ^ $3 ^ "_value") !cor_column_set;
            $1 ^ "_" ^ $3 ^ "_value" 
    }
    | PLUS tuple_name DOT attribute_name 
    { 
            if $2 <> !target_table then
                    cor_column_set := StringSet.add ($2 ^ "_" ^ $4 ^ "_value") !cor_column_set;
            $2 ^ "_" ^ $4 ^ "_value" 
    }
    | MINUS tuple_name DOT attribute_name 
    { 
            if $2 <> !target_table then
                    cor_column_set := StringSet.add ($2 ^ "_" ^ $4 ^ "_value") !cor_column_set;
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
