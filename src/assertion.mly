/* file: assertion.mly */

%{
open Printf
open List
open Global
let binders = ref ""
let arguments = ref ""
let argument_set = ref StringSet.empty
let assertion_name = ref ""

(* generate the arguments of the assertion *)
let gen_arguments () =
        let argument_list = StringSet.elements !argument_set in
        for i = 0 to (length argument_list)-1
        do
                arguments := !arguments ^ " ( " ^ (List.nth argument_list i) ^ ": list tupleType_"
                ^ (List.nth argument_list i) ^ " ) ";
        done;
        assertion_list := append !assertion_list [(!assertion_name, !argument_set)];
        !arguments

%}


/* Ocamlyacc Declarations */
%token ENDOFSQL 
%token CREATE ASSERTION CHECK
%token <string> STR
%token LPAREN RPAREN
%token OR AND
%token NOT
%token EXISTS
%token SELECT FROM WHERE
%token EQU NEQ LT LEQ GT GEQ
%token PLUS MINUS STAR SLASH
%token BETWEEN
%token IN
%token COMMA
%token DOT
%token IS NULL


%start input
//%type <string> input
%type <unit> input

/* Grammar follows */
%%
input:
    | /* empty */   { }	
	| input line	
    {
            arguments := "";
            binders := "";
            argument_set := StringSet.empty;
    }
;
line:	
    | ENDOFSQL  { }
    | assertion ENDOFSQL 
    { 
            (* Printf.printf "inside: %s\n" $1; *)
            assertion_exp := !assertion_exp ^ $1;
            (* Printf.printf "inside: %s\n" !assertion_exp *)
    }
;
assertion:  
    CREATE ASSERTION assertion_name CHECK search_condition 
    { 
            "predicate " ^ $3 ^ gen_arguments () ^ " = \n" ^ $5 ^ "\n"
    }
;
assertion_name: STR
    {
            assertion_name := $1;
            (* add assertion_list $1 gen_arguments (); *)
            (* assertion_list := append !assertion_list [$1]; *)
            (* iter (printf "assertion_list: %s\n") !assertion_list; *)
            $1 
    }
;
/*
search_condition:   
    | STR   { $1 }
    | LPAREN search_condition RPAREN { " ( " ^ $2 ^ " ) " }
    | search_condition OR search_condition { $1 ^ " \/ "^ $3 }
    | search_condition AND search_condition { $1 ^ " /\\ " ^ $3 }
;
*/

/* use left recursion */
search_condition:   
    | boolean_term { $1 }
    | search_condition OR boolean_term { $1 ^ " \/ "^ $3 }
;
boolean_term:   
    | boolean_factor    { $1 }
    | boolean_term AND boolean_factor { $1 ^ " /\\ " ^ $3 }
;
boolean_factor:  
    | predicate { $1 }
    | LPAREN search_condition RPAREN { " ( " ^ $2 ^ " ) " }
    | NOT LPAREN search_condition RPAREN { " not ( " ^ $3 ^ " ) " }
;
predicate:  
    | exists_predicate      { $1 }
    | comparison_predicate  { $1 }
    | between_predicate     { $1 }
    | in_predicate          { $1 }
    | null_predicate        { $1 }
;
exists_predicate: 
    | EXISTS LPAREN SELECT STAR FROM table_list WHERE search_condition RPAREN 
    { "exists " ^ !binders ^ $6 ^ " /\\ " ^ $8 }
    | NOT EXISTS LPAREN SELECT STAR FROM table_list WHERE search_condition RPAREN 
    { "not ( exists " ^ !binders ^ $7 ^ " /\\ " ^ $9 ^ " ) " }
;
table_list: 
    /*
    | table_name tuple_name COMMA table_list 
    {
            binders := $2 ^ ": tupleType_" ^ $1 ^ ", " ^ !binders;
            (* arguments := "( " ^ $1 ^ ": list tupleType_" ^ $1 ^" )" ^ !arguments; *)
            (* Printf.printf "binders: %s\n" !binders; *)
            " mem " ^ $2 ^ " " ^ $1 ^ " /\\ " ^ $4
    }
    */
    | table_list COMMA table_name tuple_name 
    {
            binders := $4 ^ ": tupleType_" ^ $3 ^ ", " ^ !binders;
            " mem " ^ $4 ^ " " ^ $3 ^ " /\\ " ^ $1
    }
    | table_name tuple_name 
    {
            binders := $2 ^ ": tupleType_" ^ $1 ^ "." ^ !binders;
            (* arguments := "( " ^ $1 ^ ": list tupleType_" ^ $1 ^" )" ^ !arguments; *)
            (* Printf.printf "binders: %s\n" !binders; *)
            " mem " ^ $2 ^ " " ^ $1
    }
;
table_name: STR 
    {
            argument_set := StringSet.add $1 !argument_set;
            op_argument_set := StringSet.add $1 !op_argument_set;
            $1 
    }
;
tuple_name: STR { $1 }
;
comparison_predicate: 
    | expression EQU expression   { $1 ^ " = " ^ $3 }
    | expression NEQ expression   { $1 ^ " <> " ^ $3 }
    | expression LT expression    { $1 ^ " < " ^ $3 }
    | expression LEQ expression   { $1 ^ " <= " ^ $3 }
    | expression GT expression    { $1 ^ " > " ^ $3 }
    | expression GEQ expression   { $1 ^ " >= " ^ $3 }
;
expression: 
    | term  { $1 }
    | expression PLUS term  { $1 ^ " + " ^ $3 }
    | expression MINUS term { $1 ^ " - " ^ $3 }
;
term: 
    | factor { $1 }
    | term STAR factor  { $1 ^ " * " ^ $3 }
    | term SLASH factor { $1 ^ " / " ^ $3 }
;
factor: 
    | LPAREN expression RPAREN { " ( " ^ $2 ^ " ) " }
    | constant  { $1 }
    | column    { $1 }
;
constant: 
    | STR   { $1 }
    | PLUS STR  { $2 }
    | MINUS STR { "-" ^ $2 }
;
column: 
    | tuple_name DOT attribute_name { $1 ^ "." ^ $3 }
    | PLUS tuple_name DOT attribute_name { $2 ^ "." ^ $4 }
    | MINUS tuple_name DOT attribute_name { "-" ^ $2 ^ "." ^ $4 }
;
attribute_name: STR { $1 }
;
between_predicate: 
    | expression BETWEEN constant AND constant 
    { 
            " ( " ^ $1 ^ " >= " ^ $3 ^ " ) /\\ ( " ^ $1 ^ " <= " ^ $5 ^ " ) " 
    }
    | expression NOT BETWEEN constant AND constant 
    { 
            " ( " ^ $1 ^ " < " ^ $4 ^ " ) \/ ( " ^ $1 ^  " > " ^ $6 ^ " ) " 
    }
;
in_predicate: 
    | expression IN LPAREN in_value_list RPAREN 
    { 
            " mem " ^ $1 ^ " ( " ^ $4 ^ " ) " 
    }
    | expression NOT IN LPAREN in_value_list RPAREN 
    { 
            " not ( mem " ^ $1 ^ " ( " ^ $5 ^ " ) ) " 
    }
;
in_value_list: 
    | constant { " (Cons " ^ $1 ^ " Nil) " }
    | constant COMMA in_value_list { " (Cons " ^ $1 ^ $3 ^ ") " }
;
null_predicate: 
    | column IS NULL { "(Cons " ^ $1 ^ " Nil)" ^ " = Nil " }
    | column IS NOT NULL { "(Cons " ^ $1 ^ " Nil)" ^ " <> Nil " }


%%
