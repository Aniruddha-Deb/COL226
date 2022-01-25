/*
integer binary tree
author: Aniruddha Deb
*/

ibt(empty).
ibt(node(N,L,R)) :- integer(N), ibt(L), ibt(R).

/* make a test binary tree and a test bst*/
leaf(T,N) :- T = node(N,empty,empty).

testibt(T) :- leaf(L5,5), leaf(L4,4), leaf(L7,7), T = node(1,node(2,empty,L4),node(3,L5,node(6,L7,empty))).
testbst(T) :- leaf(L1,1), leaf(L4,4), leaf(L7,7), leaf(L9,9), leaf(L12,12), leaf(L14,14),
              T = node(6,node(3,node(2,L1,empty),node(5,L4,empty)),node(11,node(8,L7,L9),node(13,L12,L14))).

/* functions in assignment */

size(empty,0).
size(node(_,L,R),N) :- size(L,T), size(R,U), N is (1 + T + U).

height(empty,0).
height(node(_,L,R),N) :- height(L,T), height(R,U), N is (1 + max(T,U)).

preorder(empty,[]).
preorder(node(N,L,R), T) :- preorder(L,PL), preorder(R,PR), append(PL, PR, K), append([N], K, T).

postorder(empty,[]).
postorder(node(N,L,R), T) :- postorder(L,PL), postorder(R,PR), append(PL, PR, K), append(K, [N], T).

inorder(empty,[]).
inorder(node(N,L,R), T) :- inorder(L,PL), inorder(R,PR), append([N], PR, K), append(PL, K, T).

trPreorder(empty,[]).
trPreorder(BT, T) :- trPreorderAcc([BT],[],T).
trPreorderAcc([(node(N,L,R))|S], TP, T) :- append([L,R],S,S1), append(TP,[N],TPP), trPreorderAcc(S1, TPP, T).
trPreorderAcc([empty|S], TP, T) :- trPreorderAcc(S, TP, T).
trPreorderAcc([],TP,TP).

trPostorder(empty,[]).
trPostorder(BT, T) :- trPostorderAcc([BT],[],T).
trPostorderAcc([(node(N,L,R))|S], TP, T) :- append([R,L],S,S1), append([N],TP,TPP), trPostorderAcc(S1, TPP, T).
trPostorderAcc([empty|S], TP, T) :- trPostorderAcc(S, TP, T).
trPostorderAcc([],TP,TP).

trInorder(empty,[]).
trInorder(BT, T) :- trInorderAcc([BT],[],[],T).
trInorderAcc([node(N,L,R)|LS], PS, TP, T) :- trInorderAcc([L|LS], [node(N,L,R)|PS], TP, T).
trInorderAcc([empty|LS], PS, TP, T) :- trInorderAcc(LS, PS, TP, T).
trInorderAcc([],[node(N,_,R)|PS],TP,T) :- append(TP,[N],TPP), trInorderAcc([R], PS, TPP, T).
trInorderAcc([],[],TP,TP).

eulerTour(empty,[]).
eulerTour(node(N,L,R),T) :- eulerTour(L,ETL), eulerTour(R,ETR), append([N],ETL,ETLL), append([N],ETR,ETRR), append(ETRR,[N],ETRF), append(ETLL, ETRF, T).

getCount([],_1,_2) :- false.
getCount([(X,Y)|L],N,C) :- ((X == N) -> C is Y) ; getCount(L,N,C).

incrCount([],N,LF) :- LF = [(N,1)].
incrCount([(X,Y)|L],N,LF) :- ((X == N) -> Yp1 is Y+1, LF = [(X,Yp1)|L]) ; (incrCount(L,N,LP), LF = [(X,Y)|LP]).

extractFromET([],_1,_2,F,F).
extractFromET([N|L], Cnt, Num, E, F) :- 
    (incrCount(Cnt, N, CntNew), getCount(CntNew,N,C), (C == Num -> append(E,[N],E1); E1=E), extractFromET(L,CntNew,Num,E1,F)). 

preET(BT,L) :- eulerTour(BT,T), extractFromET(T,[],1,[],L).
inET(BT,L) :- eulerTour(BT,T), extractFromET(T,[],2,[],L).
postET(BT,L) :- eulerTour(BT,T), extractFromET(T,[],3,[],L).

toString(empty,"()").
toString(node(N,L,R), S) :- toString(L, Lstr), toString(R, Rstr), 
                            number_string(N, Nstr), 
                            string_concat("(", Nstr, S1),
                            string_concat(S1, ",", S2),
                            string_concat(S2, Lstr, S3),
                            string_concat(S3, ",", S4),
                            string_concat(S4, Rstr, S5),
                            string_concat(S5, ")", S).
isBalanced(empty).
isBalanced(node(_,L,R)) :- isBalanced(L), isBalanced(R), height(L,Lh), height(R, Rh), abs(Lh-Rh) < 2.

allNodes(empty,[]).
allNodes(node(N,L,R), T) :- allNodes(L, Ln), allNodes(R, Rn), append(Ln,Rn,T1), T = [N|T1].

allLessThan([],_).
allLessThan([V|L],N) :- V < N, allLessThan(L,N).

allGreaterThan([],_).
allGreaterThan([V|L],N) :- V > N, allGreaterThan(L,N).

isBST(empty).
isBST(node(N,L,R)) :- allNodes(L,Ln), allNodes(R,Rn), allLessThan(Ln,N), allGreaterThan(Rn,N).


split([N],[],N,[]).
split(L,A,N,B) :- length(L,Len), divmod(Len,2,LA,Rem), 
    (Rem =:= 1 -> append(A,[N|B],L), length(A,LA), length(B,LA));
    (append(A,[N|B],L), length(A,LA), length([N|B],LA)).
makeBST([],_).
makeBST(L, BST) :- sort(L,LS), split(LS,A,N,B), makeBST(A,LBST), makeBST(B,RBST), BST = node(N,LBST,RBST).

lookup(_, empty) :- false.
lookup(N, node(Root,L,R)) :- (N == Root); ((N < Root) -> lookup(N,L); lookup(N,R)).

pred(X,L,P) :- append(_,[P,X|_],L).
succ(X,L,P) :- append(_,[X,P|_],L).

insert(N, empty, BST2) :- BST2 = node(N,empty,empty).
insert(N, node(Root,L,R), BST2) :- 
    ((N < Root) -> insert(N, L, BST2L), BST2 = node(Root,BST2L,R)); 
    (N > Root) -> insert(N, R, BST2R), BST2 = node(Root,L,BST2R).

delete(N, node(N,empty,empty), BST2) :- BST2 = empty.
delete(N, node(Root,L,R), BST2) :- 
    ((N < Root) -> delete(N, L, BST2L), BST2 = node(Root,BST2L,R)); 
    ((N > Root) -> delete(N, R, BST2R), BST2 = node(Root,L,BST2R));
    (inorder(node(Root,L,R),T), pred(N,T,Pred), delete(Pred, L, BST2L), BST2 = node(Pred,BST2L,R)).