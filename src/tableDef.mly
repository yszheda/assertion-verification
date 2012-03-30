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
%token INT INTEGER BIGINT SMALLINT
%token REAL FLOAT DOUBLE NUMERIC DEC DECIMAL 
%token BOOLEAN
%token CHAR CHARATER VARCHAR 

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
    | INT       
    {
            lib_set := StringSet.add "int.Int" !lib_set; 
            "int" 
    }
    | INTEGER   
    {
            lib_set := StringSet.add "int.Int" !lib_set; 
            "int" 
    }
    | BIGINT    
    {
            lib_set := StringSet.add "int.Int" !lib_set; 
            "int" 
    }
    | SMALLINT  
    {
            lib_set := StringSet.add "int.Int" !lib_set; 
            "int" 
    }
    | REAL      
    {
            (*
            lib_set := StringSet.add "real.Real" !lib_set; 
            *)
            "real" 
    }
    | FLOAT     
    {
            (*
            lib_set := StringSet.add "real.Real" !lib_set; 
            *)
            "real" 
    }
    | DOUBLE    
    {
            (*
            lib_set := StringSet.add "real.Real" !lib_set; 
            *)
            "real" 
    }
    | NUMERIC   
    {
            (*
            lib_set := StringSet.add "real.Real" !lib_set; 
            *)
            "real" 
    }
    | DEC       
    {
            (*
            lib_set := StringSet.add "real.Real" !lib_set; 
            *)
            "real" 
    }
    | DECIMAL   
    {
            (*
            lib_set := StringSet.add "real.Real" !lib_set; 
            *)
            "real" 
    }
    | BOOLEAN   
    {
            lib_set := StringSet.add "bool.Bool" !lib_set; 
            "bool" 
    }
    | CHAR      
    {
            lib_set := StringSet.add "module string.Char" !lib_set; 
            "char" 
    }
    | CHARATER  
    {
            lib_set := StringSet.add "module string.Char" !lib_set; 
            "char" 
    }
    | VARCHAR   
    {
            lib_set := StringSet.add "module string.Char" !lib_set; 
            lib_set := StringSet.add "list.List" !lib_set;
            "list char" 
    }
    | CHAR LPAREN STR RPAREN  
    {
            lib_set := StringSet.add "module string.Char" !lib_set; 
            lib_set := StringSet.add "list.List" !lib_set;
            "list char" 
    }

%%
