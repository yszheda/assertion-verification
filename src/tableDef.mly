/* file: tabledef.mly */

%{
open Printf
open Global
open List
open Hashtbl
(* type attribute_def = { attribute_name: string; attribute_type: string } *)
let attribute_list = ref []
%}


/* Ocamlyacc Declarations */
%token ENDOFSQL 
%token CREATE TABLE
%token <string> STR
%token LPAREN RPAREN
%token COMMA
%token INTEGER
%token SMALLINT
%token FLOAT
%token NUMERIC
%token BOOLEAN
%token CHAR

%start input
%type <unit> input

/* Grammar follows */
%%
input:
    | /* empty */	{ }
	| input line	
    {
            attribute_list := []; 
    }
;
line: 
    | ENDOFSQL    { }
    | table_definition ENDOFSQL 
    { 
            tupleType_exp := !tupleType_exp ^ $1
    }
;
table_definition:
    CREATE TABLE table_name LPAREN table_element_list RPAREN 
    { 
            add tupleType_list $3 !attribute_list;
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
            "type tupleType_" ^ $3 ^ " = {| " ^ $5 ^ " |}\n" 
    }
;
table_name: STR { $1 }
;
table_element_list: 
    | table_element { $1 }
    | table_element_list COMMA table_element { $1 ^ "; " ^ $3 }
;
table_element: column_name data_type 
    {
            attribute_list := append !attribute_list [ { attribute_name = $1; attribute_type = $2 } ];
            $1 ^ ": " ^ $2 
    }
;
column_name: STR { $1 }
;
/* translation from SQL type to Why3ML type */
data_type: 
    | INTEGER   { "int" }
    | SMALLINT  { "int" }
    | FLOAT     { "real" }
    | NUMERIC   { "real" }
    | BOOLEAN   { "bool" }
    | CHAR      { "char" }

%%
