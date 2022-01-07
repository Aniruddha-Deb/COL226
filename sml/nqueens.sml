exception Backtrack;

fun safe (x,y) [] = true |
    safe (x,y) ((x',y')::L) = x <> x' andalso y' <> y andalso (x-x') <> (y-y') andalso (x-x') <> (y'-y) andalso safe (x,y) L;

fun nextqueen n 0 L 0 = (0,0) |
    nextqueen n m L i =
    if i >= n then raise Backtrack
    else if safe (i,m) L then (i,m)
    else nextqueen n m L (i+1);

fun pqueens n 0 L i = pqueens n 1 [nextqueen n 0 L i] 0 |
    pqueens n m ((x,y)::L) i = 
    if m >= n then ((x,y)::L) 
    else pqueens n (m+1) ((nextqueen n m ((x,y)::L) i)::((x,y)::L)) 0
    handle Backtrack => pqueens n (m-1) L (x+1)

fun queens n = pqueens n 0 [] 0
