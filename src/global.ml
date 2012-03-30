(* file: global.ml *)
let tupleType_exp = ref ""
let assertion_exp = ref ""
let operation_exp = ref ""
module StringSet = Set.Make(String)
let assertion_list = ref []
let op_argument_set = ref StringSet.empty
type attribute_def = { attribute_name: string; attribute_type: string }
type item = { item_value: string; item_type: string }
let tupleType_list = Hashtbl.create 3
let lib_set = ref StringSet.empty;;
lib_set := StringSet.add "list.List" !lib_set;
lib_set := StringSet.add "list.Mem" !lib_set;
lib_set := StringSet.add "list.Append" !lib_set;

