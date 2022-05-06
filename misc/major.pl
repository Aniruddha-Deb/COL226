prefix([],_).
prefix([X|L1],[X|L2]) :- prefix(L1,L2).

suffix([],_).
suffix(L1,L2) :- reverse(L1,L1R), reverse(L2,L2R), prefix(L1R,L2R).
