/* file: update.mly */

%{
open Printf
open List
open Global
let binders = ref ""
let target_table = ref ""
(* let value_list = ref [""] *)
(* list of column names *)
let col_name_list = ref []
let arguments = ref ""
let rec_arguments = ref ""
let column_exp = ref ""
type attribute = { name: string; value: string }
let update_list = ref []

let table_list = ref []
let cor_column_set = ref StringSet.empty
(* set of arguments in the multi-table UPDATE functions *)
let multi_argument_set = ref StringSet.empty
(* string of parameters used in the corresponding multi-table UPDATE functions *)
let multi_parameters = ref ""
(* string of arguments in the final update function when there are multiple tables *)
let multi_update_arg = ref ""
(* set of arguments in the final update function when there are multiple tables *)
let multi_update_arg_set = ref StringSet.empty
(* prototype of the final update function when there are multiple tables *)
let multi_update_fun = ref ""

(* search condition statement *)
let sc_stmt = ref ""
(* search condition predicate *)
let sc_predicate = ref ""
(* the list of tables in the current or previous iterations *)
let iter_table_list = ref []
type column = { table: string; attribute: string }
(* list of arguments in the <search condition> predicate in the Why3ML program *)
let predicate_arg_list = ref []
let last_iter_fun = ref ""
let last_iter_fun_arg = ref ""
let last_iter_table_list = ref []

(* 
 * gen_df_col_name_list table:
 * table: string of the table name
 * Generate the default attribute name list if it is not explicitly specified in
 * the UPDATE operation.
 *)
let gen_df_col_name_list table =
        col_name_list := [];
        let attribute_list = Hashtbl.find tupleType_list table in
        for i = 0 to (length attribute_list)-1
        do
                col_name_list := append !col_name_list [ (nth attribute_list i).attribute_name ]
        done

(* 
 * fun gen_single_args (): 
 * Generate the arguments for update function if there is only one table in the SQL UPDATE statement. 
 *)
let gen_single_args () =
        arguments := "";
        let argument_list = StringSet.elements !op_argument_set in
        for i = 0 to (length argument_list)-1
        do
                arguments := " ( " ^ (List.nth argument_list i) ^ ": list tupleType_"
                ^ (List.nth argument_list i) ^ " ) " ^ !arguments;
        done;
        !arguments

(* 
 * gen_column_exp nlist table:
 * nlist: list of the column names
 * table: string of the table name
 * Generate the expression of columns in the form: 
 * <column name> = <column value>(; <column name> = <column value>)*
 * The expression will be used in the pattern matching part in the Why3ML program.
 *)
let rec gen_column_exp nlist table = 
        match nlist with
        | n1::[] -> n1 ^ " = " ^ table ^ "_" ^ n1 ^ "_value"
        | n1::n2 -> n1 ^ " = " ^ table ^ "_" ^ n1 ^ "_value; " ^ ( gen_column_exp n2 table )
        | [] -> "" 

(* 
 * gen_column_exp nlist table:
 * nlist: list of the column names
 * table: string of the table name
 * op: comparison operator
 * conn: connective symbol
 * Generate the expression of columns in the form: 
 * <column name> = <column value>(; <column name> = <column value>)*
 * The expression will be used in the pattern matching part in the Why3ML program.
 *)
let rec gen_column_exp nlist table op conn = 
        match nlist with
        | n1::[] -> n1 ^ " " ^ op ^ " " ^ table ^ "_" ^ n1 ^ "_value"
        | n1::n2 -> n1 ^ " " ^ op ^ " " ^ table ^ "_" ^ n1 ^ "_value " ^ conn ^
        " " ^ ( gen_column_exp n2 table op conn )
        | [] -> "" 

(* 
 * gen_single_postcondition table:
 * table: string of the table name
 * Generate postcondition for the UPDATE operation, returns the string
 * expression of the postcondition. 
 * *)
let gen_single_postcondition table = 
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


(********************************************************************************
 * If multiple tables are involved in the UPDATE operation, a set of iteration
 * functions will be generated, and <search condition> in the WHERE part will be
 * used as a predicate in the Why3ML program.
 ********************************************************************************)

(* 
 * get_predicate_arg_list (): 
 * Generate the predicate_arg_list from the set of the corresponding columns,
 * note that redundancy is already removed in the list of arguments.
 *)
let get_predicate_arg_list () = 
        let cor_column_list = StringSet.elements !cor_column_set in
        for i = 0 to (length cor_column_list)-1
        do
                let split_result = Str.split (Str.regexp "_") (nth cor_column_list i) in
                let table = (nth split_result 0) in
                let attribute = (nth split_result 1) in
                let cor_column = { table = table; attribute = attribute } in
                predicate_arg_list := append !predicate_arg_list [cor_column];
        done

(* 
 * get_attribute_type column: 
 * column: corresponding string of column in the form of "#table_#attribute_value".
 * Return the type of the attribute from the given column.
 *)
let get_attribute_type column =
        let attribute_type = ref "" in
        let split_result = Str.split (Str.regexp "_") column in
        let table = (nth split_result 0) in
        let attribute = (nth split_result 1) in
        let attribute_list = Hashtbl.find tupleType_list table in
        for i = 0 to (length attribute_list)-1
        do
                if (nth attribute_list i).attribute_name = attribute then
                        attribute_type := (nth attribute_list i).attribute_type
        done;
        !attribute_type

(* 
 * gen_predicate (): 
 * Generate the predicate definition of <search condition> when there are multiple tables in the UPDATE statement.
 *)
let gen_predicate () =
        get_predicate_arg_list ();
        sc_predicate := "predicate sc_predicate";
        let cor_column_list = StringSet.elements !cor_column_set in
        for i = 0 to (length cor_column_list)-1
        do
                let cor_column = (nth cor_column_list i) in
                sc_predicate := !sc_predicate ^ " ( " ^ (nth cor_column_list i)
                ^ ": " ^ get_attribute_type cor_column ^ " )"
        done;
        let condition = Str.global_replace (Str.regexp "/\\") "&&" !sc_stmt in
                sc_predicate := !sc_predicate ^ " =\n" ^ condition ^ "\n";
        !sc_predicate
        
(* 
 * gen_iter_binders cor_table_list: 
 * cor_table_list: the list of corresponding tables involved in the exists predicate in the postcondition of iteration functions
 * Generate the binders for the postcondition of iteration functions in the form: 
 * "exists x_#table: tupleType_#table(, x_#table: tupleType_#table)*."
 *)
let gen_iter_binders cor_table_list = 
        let binders = ref "exists " in
        for i = 0 to (length cor_table_list)-1
        do
                let iter_table = (nth cor_table_list i) in
                binders := !binders ^ "x_" ^ iter_table ^ ": tupleType_" ^ iter_table;
                if i < (length cor_table_list)-1 then
                        binders := !binders ^ ", "
                else
                        binders := !binders ^ ". " 
        done;
        !binders
                
(* 
 * gen_binder_mem cor_table_list:
 * cor_table_list: the list of corresponding tables involved in the exists predicate in the postcondition of iteration functions
 * Generate the membership of binders in the postcondition of the iteration functions in the form: 
 * "mem x_#table #table( /\ mem x_#table #table )*"
 *)
let gen_binder_mem cor_table_list = 
        let binder_mem = ref "" in
        for i = 0 to (length cor_table_list)-1
        do
                let iter_table = (nth cor_table_list i) in
                binder_mem := !binder_mem ^ "mem x_" 
                ^ iter_table ^ " " ^ iter_table ^ " /\\ ";
        done;
        !binder_mem

(* 
 * gen_multi_predicate cor_table_list:
 * cor_table_list: the list of corresponding tables involved in the exists predicate in the postcondition of iteration functions
 * Generate the predicate which will be used in the postcondition of functions when
 * there are multiple tables in the UPDATE statement. 
 *)
let gen_multi_predicate cor_table_list =
        let iter_predicate = ref "( sc_predicate" in
        for i = 0 to (length !predicate_arg_list)-1
        do
                let predicate_arg = (nth !predicate_arg_list i) in
                if (mem predicate_arg.table cor_table_list) then
                begin
                        if (predicate_arg.table = !target_table) && (mem !target_table !iter_table_list) then
                                iter_predicate := !iter_predicate ^ " old_x_" ^
                                predicate_arg.table ^ "." ^ predicate_arg.attribute 
                        else
                        iter_predicate := !iter_predicate ^ " x_" ^ predicate_arg.table ^ "." ^
                        predicate_arg.attribute 
                end
                else
                        iter_predicate := !iter_predicate ^ " " ^ predicate_arg.table ^ "_" ^
                        predicate_arg.attribute ^ "_value"
        done;
        iter_predicate := !iter_predicate ^ " )";
        !iter_predicate

(* 
 * gen_cor_assertions ():
 * Generate the corresponding expressions of assertions according to the tables
 * which have been already iterated ( iter_table_list ). An assertion will be
 * chosen only when all its corresponding tables are from iter_table_list.
 *)
let gen_cor_assertions () =
        let cor_assertions = ref "" in
        let is_cor = ref true in
        let assertion_stmt = ref "" in
        for i = 0 to (length !assertion_list)-1
        do
                is_cor := true;
                assertion_stmt := "";
                let assertion = (nth !assertion_list i) in
                let argument_list = StringSet.elements (snd assertion) in
                assertion_stmt := fst assertion;
                for j = 0 to (length argument_list)-1
                do
                        assertion_stmt := !assertion_stmt ^ " " ^ ( nth argument_list j );
                        if not (mem (nth argument_list j) !iter_table_list)
                        then is_cor := false;
                done;
                if !is_cor = true then
                begin
                        if i > 0 then
                                cor_assertions := !cor_assertions ^ " /\\ ";
                        cor_assertions := !cor_assertions ^ !assertion_stmt;
                end
        done;
        !cor_assertions

(* 
 * gen_caller_arg arg_set old_arg new_arg: 
 * arg_set: the set of the arguments in the caller function
 * old_arg: the parameter in the definition of the caller function which will be replaced
 * new_arg: the argument which will replace old_arg
 * Generate the arguments of the caller function when it is called.
 *)
let gen_caller_arg arg_set old_arg new_arg =
        let caller_arguments = ref "" in
        let argument_list = StringSet.elements !arg_set in
        for i = 0 to (length argument_list)-1
        do
                let current_arg = (nth argument_list i) in
                if current_arg = old_arg then
                begin
                        caller_arguments := " " ^ new_arg ^ !caller_arguments;
                end
                else
                        caller_arguments := " " ^ current_arg ^ !caller_arguments;
        done;
        !caller_arguments

(* 
 * remove_cor_table table: 
 * table: string of the table name
 * Remove the corresponding columns in cor_column_set according to the given table.
 *)
let remove_cor_table table = 
        let cor_column_list = StringSet.elements !cor_column_set in
        for i = 0 to (length cor_column_list)-1
        do
                let cor_column = (nth cor_column_list i) in
                if Str.string_match ( Str.regexp (table^"_") ) cor_column 0 = true
                then cor_column_set := StringSet.remove cor_column !cor_column_set 
        done

(*
 * gen_multi_argument_set table:
 * table: the table which is currently iterated
 * Generate the argument set for the functions in the target Why3ML program when
 * there are multiple tables in the UPDATE statement, and then it will be used in
 * the prototype of functions. Normally, parameters which are columns of the
 * given table should be removed and the given table itself should be added into
 * the argument set.
 *)
let gen_multi_argument_set table = 
        remove_cor_table table;
        multi_argument_set := !cor_column_set;
        multi_argument_set := StringSet.add table !multi_argument_set

(*
 * gen_multi_parameters ():
 * Generate the parameters of the prototype of functions in the target Why3ML
 * program when there are multiple tables in the UPDATE statement.
 *)
let gen_multi_parameters () =
        multi_parameters := "";
        let argument_list = StringSet.elements !multi_argument_set in
        for i = 0 to (length argument_list)-1
        do
                multi_parameters := " " ^ (nth argument_list i) ^ !multi_parameters;
        done;
        !multi_parameters

(* 
 * update_column ulist table:
 * ulist: list of columns which are updated
 * table: string of the table name
 * Generate the expression of all columns of the table in the form: 
 * "<column name> = (<old value>|<new value>)(; <column name> = (<old value>|<new value>) )*"
 * And it will be used in the recursive part of the update function.
 *)
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

(* 
 * gen_updated_col ulist table:
 * ulist: list of columns which are updated
 * table: string of the table name
 * Generate the expression of only the columns which are updated in the form:
 * "x_#table.<updated column> = <updated value>( \/ x_#table.<updated column> = <updated value>)*"
 * And it will be used in the postcondition of the update function when there
 * are multiple tables in the UPDATE statement.
 *)
let gen_updated_col ulist table =
        let updated_col = ref "" and replace_flag = ref false in
        for j = 0 to (length !col_name_list)-1 do
                replace_flag := false;
                for i = 0 to (length ulist)-1 do
                        if (String.compare (nth !col_name_list j) (nth ulist i).name) = 0 then
                        begin
                                if j > 0 then
                                        updated_col :=
                                                !updated_col ^ " \/ ";
                                updated_col := !updated_col 
                                ^ "x_" ^ table ^ "."
                                ^ (nth !col_name_list j) ^
                                " = " ^ (nth ulist i).value;
                                replace_flag := true;
                        end
                done;
        done;
        !updated_col

(* 
 * gen_iter_funs (): 
 * Generate iteration functions that will be used as assistances for the update
 * function when there are multiple tables in the UPDATE statement.
 *)
let gen_iter_funs () = 
        let iter_fun_def = ref "" in
        let pre_fun = ref "" in
        let pre_fun_arg = ref "" in
        let pre_fun_arg_set = ref StringSet.empty in
        let iter_postcondition = ref "" in
        let check_condition = ref "" in
        for i = 0 to (length !table_list)-1
        do
                let table = (nth !table_list i) in
                let fun_name = "iter_" ^ table in
                let caller_arguments = ref "" in
                if i = 0 then
                begin
                        pre_fun := !multi_update_fun;
                        pre_fun_arg := !multi_update_arg;
                        pre_fun_arg_set := !multi_update_arg_set;
                        check_condition := !sc_stmt;
                end
                else begin
                        check_condition := "( " ^ !pre_fun ^ !pre_fun_arg ^ " )";
                end;
                iter_table_list := append !iter_table_list [table];
                gen_df_col_name_list table;
                gen_multi_argument_set table;
                if i > 0 then
                begin
                        let old_table = (nth !table_list (i-1)) in
                        multi_argument_set := StringSet.add old_table !multi_argument_set;
                end;
                gen_multi_parameters ();
                let arg_set = multi_argument_set in
                caller_arguments := gen_caller_arg arg_set table "l";
                iter_postcondition := gen_cor_assertions ()
                ^ " /\\ ( result = True <->  "
                ^ gen_iter_binders !iter_table_list 
                ^ gen_binder_mem !iter_table_list
                ^ gen_multi_predicate !iter_table_list
                ^ " )";
                iter_fun_def := !iter_fun_def ^ 
                "let rec " ^ fun_name ^ !multi_parameters ^ " =\n"
                ^ "{ " ^ gen_cor_assertions () ^ " }\n"
                ^ "match " ^ table ^ " with\n"
                ^ "| Nil -> False\n"
                ^ "| Cons {| " ^ gen_column_exp !col_name_list table "=" ";" 
                ^ " |} l -> if " ^ !check_condition ^ " then True\n"
                ^ "else ( " ^ fun_name ^ !caller_arguments ^ " )\n"
                ^ "end\n"
                ^ "{ " ^ !iter_postcondition ^ " }\n";
                if i = (length !table_list)-1 then
                begin
                        last_iter_fun := fun_name;
                        last_iter_fun_arg := !multi_parameters;
                        last_iter_table_list := !iter_table_list;
                end;
                pre_fun := fun_name;
                pre_fun_arg := gen_multi_parameters ();
                pre_fun_arg_set := !multi_argument_set;
        done;
        !iter_fun_def

(* 
 * gen_multi_update_fun (): 
 * Generate the final update function that will be used for verification when there're multiple tables in the UPDATE statement.
 *)
let gen_multi_update_fun () =
        let multi_update_fun = ref "" in
        let check_condition = "( " ^ !last_iter_fun ^ !last_iter_fun_arg ^ " )" in
        let fun_name = "update_" ^ !target_table in
        let pre_iter_table_list = last_iter_table_list in
        let caller_arguments = ref "" in
        iter_table_list := append !iter_table_list [!target_table];
        gen_df_col_name_list !target_table;
        gen_multi_argument_set !target_table;
        for i = 0 to (length !table_list)-1
        do
                multi_argument_set := StringSet.add (nth !table_list i) !multi_argument_set;
        done;
        gen_multi_parameters ();
        let arg_set = multi_argument_set in
        caller_arguments := gen_caller_arg arg_set !target_table "l";
        let binders = "forall x_" ^ !target_table 
        ^ ": tupleType_" ^ !target_table 
        ^ ", old_x_" ^ !target_table
        ^ ": tupleType_" ^ !target_table ^ "." in
        let update_postcondition = gen_cor_assertions ()
        ^ " /\\ " ^ binders
        ^ " mem x_" ^ !target_table ^ " " ^ !target_table
        ^ " -> mem old_x_" ^ !target_table ^ " ( old " ^ !target_table ^ " ) "
        ^ " -> " ^ gen_updated_col !update_list !target_table
        (* ^ gen_column_exp !col_name_list !target_table "<>" "&&" *)
        ^ " \/ not ("
        ^ gen_iter_binders !pre_iter_table_list
        ^ gen_binder_mem !pre_iter_table_list
        ^ gen_multi_predicate !iter_table_list
        ^ ")" in
        multi_update_fun := "let rec " ^ fun_name ^ !multi_parameters ^ " =\n"
        ^ "{ " ^ gen_cor_assertions () ^ " }\n"
        ^ "match " ^ !target_table ^ " with\n"
        ^ "| Nil -> Nil\n"
        ^ "| Cons {| " ^ gen_column_exp !col_name_list !target_table "=" ";" 
        ^ " |} l -> if " ^ check_condition 
        ^ " then Cons {| " ^ update_column !update_list !target_table
        ^ " |} ( " ^ fun_name ^ !caller_arguments ^ " )\n"
        ^ "else Cons {| " ^ gen_column_exp !col_name_list !target_table "=" ";"
        ^ " |} ( " ^ fun_name ^ !caller_arguments ^ " )\n"
        ^ "end\n"
        ^ "{ " ^ update_postcondition ^ " }\n";
        !multi_update_fun

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
            "let rec update " ^ gen_single_args () ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $2 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ ( gen_column_exp !col_name_list $2  "=" ";" ) ^ " |} l -> Cons {| " 
            ^ ( update_column !update_list $2 ) 
            ^ " |} (update" ^ gen_rec_arguments $2 ^ ")\n"
            ^ "end\n"
            ^ "{ " ^ gen_single_postcondition $2 ^ " }\n"
    }
    | UPDATE table_name SET set_clause_list WHERE search_condition
    {
            gen_df_col_name_list $2;
            (* "let rec update (" ^ $2 ^ ": list tupleType_" ^ $2 ^ " ) =\n" *)
            "let rec update " ^ gen_single_args () ^ " = \n"
            ^ "{ true }\n"
            ^ "match " ^ $2 ^ " with\n"
            ^ "| Nil -> Nil\n"
            ^ "| Cons {| " ^ ( gen_column_exp !col_name_list $2 "=" ";" ) ^ " |} l -> if " ^ $6 
            ^ "then Cons {| " ^ ( update_column !update_list $2 ) 
            ^ " |} (update" ^ gen_rec_arguments $2 ^ ")\n"
            ^ "else Cons {| "
            (* ( update_column ) *)
            ^ ( gen_column_exp !col_name_list $2 "=" ";" )
            ^ " |} (update" ^ gen_rec_arguments $2 ^ ")\n"
            ^ "end\n"
            ^ "{ " ^ gen_single_postcondition $2 ^ " }\n"
    }
    | UPDATE target_table_name SET set_clause_list FROM table_reference_list WHERE search_condition
    {
            sc_stmt := $8;
            let sc_predicate = gen_predicate () in
            let iter_fun_def = gen_iter_funs () in
            let multi_update_fun = gen_multi_update_fun () in
            sc_predicate ^ iter_fun_def ^ multi_update_fun
            (*
            gen_df_col_name_list $2;
            gen_multi_argument_set $2;
            gen_multi_parameters ();
            multi_update_arg := !multi_parameters;
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
            *)
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
            cor_column_set := StringSet.add ($1 ^ "_" ^ $3 ^ "_value") !cor_column_set;
            $1 ^ "_" ^ $3 ^ "_value" 
    }
    | PLUS table_name DOT attribute_name 
    { 
            cor_column_set := StringSet.add ($2 ^ "_" ^ $4 ^ "_value") !cor_column_set;
            $2 ^ "_" ^ $4 ^ "_value" 
    }
    | MINUS table_name DOT attribute_name 
    { 
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
