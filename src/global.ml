(* file: global.ml *)
let tupleType_exp = ref ""
let assertion_exp = ref ""
let operation_exp = ref ""
module StringSet = Set.Make(String)
let assertion_list = ref []
let op_argument_set = ref StringSet.empty
type attribute_def = { attribute_name: string; attribute_type: string }
let tupleType_list = Hashtbl.create 3


