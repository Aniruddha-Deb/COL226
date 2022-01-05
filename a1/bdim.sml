fun read_file_to_vector in_stream =
let
	fun tuple_from_list lst =
		(List.nth(lst,0), List.nth(lst,1), List.nth(lst,2), List.nth(lst,3))
	fun stream_to_int_list stream ctr =
		if ctr = 4 then nil else
		case TextIO.scanStream (Int.scan StringCvt.DEC) stream of
			SOME num => num :: stream_to_int_list stream (ctr+1)
		  | NONE => nil
	val lst = stream_to_int_list in_stream 0 
in
	if List.length lst <> 4 then [] else tuple_from_list lst :: read_file_to_vector in_stream
end
	

fun read_bdim file =
let
	val in_stream = TextIO.openIn file
in
	Vector.fromList (read_file_to_vector in_stream)
end
	
