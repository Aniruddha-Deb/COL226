type(unit).
type(int).
type(bool).
type(real).
type(pair(T1,T2)).
type(map(T1,T2)).

pair(T1,T2) :- type(T1), type(T2).
map(T1,T2) :- type(T1), type(T2).

subtype(unit,_).
subtype(int,real).
subtype(T1,pair(T1,_)).
subtype(map(T1,T2),map(T1P,T2P)) :- subtype(T1P,T1), subtype(T2,T2P).
