(* question 1 *)

datatype tree = empty | tree of int * (tree list)

(* question 2 *)

fun tree_to_str empty = "[]" |
	tree_to_str (tree(a, l)) = Int.toString(a) ^ "[" ^ (List.foldr (fn (t,s) => (tree_to_str t) ^ "," ^ s) "" l) ^ "]"

(* question 3 *)

fun str_to_tree "" = empty |
	str_to_tree s = 
let
	val items = Scan.sscanf "%d[%s]" s
	(* no fkn clue.... no clue *)

fun str_to_tree_list s = 
	
