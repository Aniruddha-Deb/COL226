fun command_from_list lst =
	(List.nth(lst,0), List.nth(lst,1), List.nth(lst,2), List.nth(lst,3));

fun stream_to_int_list stream ctr =
	if ctr = 4 then nil else
	case TextIO.scanStream (Int.scan StringCvt.DEC) stream of
		SOME num => num :: stream_to_int_list stream (ctr+1)
	  | NONE => nil;
		
fun read_file_to_vector in_stream =
let
	val lst = stream_to_int_list in_stream 0 
in
	if List.length lst <> 4 then [] else command_from_list lst :: read_file_to_vector in_stream
end

exception NotExist;

fun assign ((k':int,v':int)::d) (k:int,v:int) = if k' = k then (k,v)::d else (k',v')::(assign d (k,v))
| assign [] (k:int, v:int) = (k,v)::[];
fun access ((k':int,v':int)::d) (k:int) = if k' = k then v' else access d k 
| access [] (k:int) = raise NotExist;

fun inp mem opd1 tgt = 
let
	val prompt = print "input: "
	val inp_val = valOf (Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)))
in
	assign mem (tgt,inp_val)
end

fun terminate msg = 
let
	val pt = print msg
in
	OS.Process.exit(OS.Process.success)
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
handle Div => terminate "Nothing to do now sorri";
fun modulo mem opd1 opd2 tgt = assign mem (tgt,((access mem opd1) mod (access mem opd2))) 
handle Div => terminate "mod to do now sorri";

fun beq mem opd1 opd2 tgt = assign mem (tgt,(if (access mem opd1) = (access mem opd2) then 1 else 0));
fun bgt mem opd1 opd2 tgt = assign mem (tgt,(if (access mem opd1) > (access mem opd2) then 1 else 0));

fun pval mem opd1 = 
let
	val u = print ("output: " ^ Int.toString(access mem opd1) ^ "\n")
in
	mem
end

fun interpret (code: (int * int * int * int) vector) (pc: int) mem =
let 
	val (opn,opd1,opd2,tgt) = (Vector.sub(code,pc))
in
	case opn of
	  0 => mem
	| 1 => interpret code (pc+1) (inp mem opd1 tgt)
	| 2 => interpret code (pc+1) (set mem opd1 tgt)
	| 3 => interpret code (pc+1) (bnot mem opd1 tgt)
	| 4 => interpret code (pc+1) (bor mem opd1 opd2 tgt)
	| 5 => interpret code (pc+1) (band mem opd1 opd2 tgt)
	| 6 => interpret code (pc+1) (add mem opd1 opd2 tgt)
	| 7 => interpret code (pc+1) (sub mem opd1 opd2 tgt)
	| 8 => interpret code (pc+1) (mul mem opd1 opd2 tgt)
	| 9 => interpret code (pc+1) (divide mem opd1 opd2 tgt)
	| 10 => interpret code (pc+1) (modulo mem opd1 opd2 tgt)
	| 11 => interpret code (pc+1) (beq mem opd1 opd2 tgt)
	| 12 => interpret code (pc+1) (bgt mem opd1 opd2 tgt)
	| 13 => interpret code (if (access mem opd1) = 1 then tgt else pc+1) mem
	| 14 => interpret code tgt mem
	| 15 => interpret code (pc+1) (pval mem opd1)
end

fun read_bdim file =
let
	val in_stream = TextIO.openIn file
	val instructions = Vector.fromList (read_file_to_vector in_stream)
in
	interpret instructions 0 [] 
end


