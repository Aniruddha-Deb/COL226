(* Notes:
 * - Liberty has been taken in I/O, i.e. input prompts are prefixed with 'input:'
 *   and output is not prefixed
 * - The function `interpret` does not return unit. To make debugging easier, 
 *   it returns the memory dictionary (a list of (key,value) tuples), representing
 *   the memory at the end of execution of the program
 *)

(* parsing related functions *)
fun command_from_list (a::b::c::d::[]) = (a,b,c,d)

fun sep a = (a = #"(" orelse a = #")" orelse a = #"," orelse a = #"\n")

fun parse_cmd str = List.map valOf (List.map Int.fromString (String.tokens sep str))
	
fun read_file_to_vector in_stream = 
let
	val in_str = TextIO.inputLine in_stream
in
	(* the str = "\n" or "" case is to avoid ambiguities about the extra line at the end. 
	   This code works irrespective of whether there's an extra line at the end or not*)
	case in_str of 
	  SOME str => (if str = "\n" orelse str = ""  then [] else command_from_list (parse_cmd str) :: read_file_to_vector in_stream)
	| NONE => []
end

exception NotExist;

(* since memory access is in the positive integer domain, a dictionary is used
 * to represent memory. I've implemented it as a simple list of (key, value) pairs
 *)
fun assign ((k':int,v':int)::d) (k:int,v:int) = if k < 0 then raise NotExist else if k' = k then (k,v)::d else (k',v')::(assign d (k,v))
| assign [] (k:int, v:int) = (k,v)::[];
fun access ((k':int,v':int)::d) (k:int) = if k < 0 then raise NotExist else if k' = k then v' else access d k 
| access [] (k:int) = raise NotExist;

(* opcode related functions *)

fun inp mem opd1 tgt = 
let
	val prompt = print "input: "
	val inp_val = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
in
	assign mem (tgt,inp_val)
end

fun set mem opd1 tgt = assign mem (tgt,(access mem opd1));

fun int_not 0 = 1 | int_not 1 = 0 | int_not a = 0;
fun int_and 0 0 = 0 | int_and 0 1 = 0 | int_and 1 0 = 0 | int_and 1 1 = 1 | int_and a b = 0;
fun int_or 0 0 = 0 | int_or 0 1 = 1 | int_or 1 0 = 1 | int_or 1 1 = 1 | int_or a b = 1;

fun bnot mem opd1 tgt = assign mem (tgt,int_not (access mem opd1));
fun band mem opd1 opd2 tgt = assign mem (tgt,int_and (access mem opd1) (access mem opd2));
fun bor mem opd1 opd2 tgt = assign mem (tgt,int_or (access mem opd1) (access mem opd2));

fun add mem opd1 opd2 tgt = assign mem (tgt,((access mem opd1) + (access mem opd2)));
fun sub mem opd1 opd2 tgt = assign mem (tgt,((access mem opd1) - (access mem opd2)));
fun mul mem opd1 opd2 tgt = assign mem (tgt,((access mem opd1) * (access mem opd2)));
fun divide mem opd1 opd2 tgt = assign mem (tgt,((access mem opd1) div (access mem opd2))) 
fun modulo mem opd1 opd2 tgt = assign mem (tgt,((access mem opd1) mod (access mem opd2))) 

fun beq mem opd1 opd2 tgt = assign mem (tgt,(if (access mem opd1) = (access mem opd2) then 1 else 0));
fun bgt mem opd1 opd2 tgt = assign mem (tgt,(if (access mem opd1) > (access mem opd2) then 1 else 0));

fun pval mem opd1 = (print (Int.toString(access mem opd1) ^ "\n") ; mem)

(* the main interpreter. Runs recursively, and operation depends on opcode. *)
fun interpret_rec (code: (int * int * int * int) vector) (pc: int) mem =
let 
	val (opn,opd1,opd2,tgt) = (Vector.sub(code,pc))
in
	case opn of
	  0 => mem
	| 1 => interpret_rec code (pc+1) (inp mem opd1 tgt)
	| 2 => interpret_rec code (pc+1) (set mem opd1 tgt)
	| 3 => interpret_rec code (pc+1) (bnot mem opd1 tgt)
	| 4 => interpret_rec code (pc+1) (bor mem opd1 opd2 tgt)
	| 5 => interpret_rec code (pc+1) (band mem opd1 opd2 tgt)
	| 6 => interpret_rec code (pc+1) (add mem opd1 opd2 tgt)
	| 7 => interpret_rec code (pc+1) (sub mem opd1 opd2 tgt)
	| 8 => interpret_rec code (pc+1) (mul mem opd1 opd2 tgt)
	| 9 => (interpret_rec code (pc+1) (divide mem opd1 opd2 tgt) handle Div => mem)
	| 10 => (interpret_rec code (pc+1) (modulo mem opd1 opd2 tgt) handle Div => mem)
	| 11 => interpret_rec code (pc+1) (beq mem opd1 opd2 tgt)
	| 12 => interpret_rec code (pc+1) (bgt mem opd1 opd2 tgt)
	| 13 => interpret_rec code (if (access mem opd1) = 1 then tgt else pc+1) mem
	| 14 => interpret_rec code tgt mem
	| 15 => interpret_rec code (pc+1) (pval mem opd1)
	| 16 => interpret_rec code (pc+1) (assign mem (tgt, opd1))
	| k => mem

	handle NotExist => mem
end

(* the primary function for interpreting and running BDIM files *)

fun interpret file =
let
	val in_stream = TextIO.openIn file
	val instructions = Vector.fromList (read_file_to_vector in_stream)
in
	interpret_rec instructions 0 [] 
end
