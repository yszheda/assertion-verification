(* file: global.mli *)
val tupleType_exp: string ref 
val assertion_exp: string ref 
val operation_exp: string ref 
module StringSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
(* val assertion_list: (string, StringSet) Hashtbl.t *)
(* val assertion_list: string list ref *)
val assertion_list: (string * StringSet.t) list ref
val op_argument_set : StringSet.t ref
type attribute_def = { attribute_name: string; attribute_type: string }
val tupleType_list: (string, attribute_def list) Hashtbl.t 

