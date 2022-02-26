/*
integer binary tree
author: Aniruddha Deb
*/

ibt(empty).
ibt(node(N,L,R)) :- integer(N), ibt(L), ibt(R).

/* make a test binary tree and a test bst*/
leaf(T,N) :- T = node(N,empty,empty).

testibt(T) :- leaf(L5,5), leaf(L4,4), leaf(L7,7), T = node(1,node(2,empty,L4),node(3,L5,node(6,L7,empty))).
testibt_dup(T) :- leaf(L5,5), leaf(L4,4), leaf(L7,7), T = node(1,node(2,empty,L4),node(3,L5,node(3,L7,empty))).
testbst(T) :- leaf(L1,1), leaf(L4,4), leaf(L7,7), leaf(L9,9), leaf(L12,12), leaf(L14,14),
              T = node(6,node(3,node(2,L1,empty),node(5,L4,empty)),node(11,node(8,L7,L9),node(13,L12,L14))).

/* functions in assignment */

size(empty,0).
size(node(K,L,R),N) :- ibt(node(K,L,R)), size(L,T), size(R,U), N is (1 + T + U).

height(empty,0).
height(node(K,L,R),N) :- ibt(node(K,L,R)), height(L,T), height(R,U), N is (1 + max(T,U)).

preorder(empty,[]).
preorder(node(N,L,R), T) :- ibt(node(N,L,R)), preorder(L,PL), preorder(R,PR), append(PL, PR, K), append([N], K, T).

postorder(empty,[]).
postorder(node(N,L,R), T) :- ibt(node(N,L,R)), postorder(L,PL), postorder(R,PR), append(PL, PR, K), append(K, [N], T).

inorder(empty,[]).
inorder(node(N,L,R), T) :- ibt(node(N,L,R)), inorder(L,PL), inorder(R,PR), append([N], PR, K), append(PL, K, T).

/* tail recursion uses the concept of an accumulator. Preorder and postorder tail
recursion are pretty similar, but inorder uses two stacks for making the 
traversal */
trPreorder(empty,[]).
trPreorder(BT, T) :- ibt(BT), trPreorderAcc([BT],[],T).
trPreorderAcc([(node(N,L,R))|S], TP, T) :- append([L,R],S,S1), append(TP,[N],TPP), trPreorderAcc(S1, TPP, T).
trPreorderAcc([empty|S], TP, T) :- trPreorderAcc(S, TP, T).
trPreorderAcc([],TP,TP).

trPostorder(empty,[]).
trPostorder(BT, T) :- ibt(BT), trPostorderAcc([BT],[],T).
trPostorderAcc([(node(N,L,R))|S], TP, T) :- append([R,L],S,S1), append([N],TP,TPP), trPostorderAcc(S1, TPP, T).
trPostorderAcc([empty|S], TP, T) :- trPostorderAcc(S, TP, T).
trPostorderAcc([],TP,TP).

trInorder(empty,[]).
trInorder(BT, T) :- ibt(BT), trInorderAcc([BT],[],[],T).
trInorderAcc([node(N,L,R)|LS], PS, TP, T) :- trInorderAcc([L|LS], [node(N,L,R)|PS], TP, T).
trInorderAcc([empty|LS], PS, TP, T) :- trInorderAcc(LS, PS, TP, T).
trInorderAcc([],[node(N,_,R)|PS],TP,T) :- append(TP,[N],TPP), trInorderAcc([R], PS, TPP, T).
trInorderAcc([],[],TP,TP).

eulerTour(empty,[]).
eulerTour(node(N,L,R),T) :- ibt(node(N,L,R)), eulerTour(L,ETL), eulerTour(R,ETR), append([N],ETL,ETLL), append([N],ETR,ETRR), append(ETRR,[N],ETRF), append(ETLL, ETRF, T).

/* 
The Euler tour of a tree is a list of numbers describing a tree, whose context-free
grammar is given as 
T -> epsilon | dTdTd
d -> number 

Note that this grammar is not regular! hence, to parse a euler tour into a tree, 
we cannot use a regular expression or a finite-state automata. Hence, the only way
to obtain a tree from T is to use a NFA with backtracking, which is how we've 
implemented our preET, inET and postET methods: by converting the euler tour back
to a tree and using inorder traversal on the tree. 

The reasons behind this are that parsing the given grammar to preorder/postorder/inorder
would need a LL/LR parser, and I do not know how to implement one yet (I don't 
even know if my analysis is correct, just that no way I try, I can get this to work
linearly).

A failed implementation that works ONLY if the nodes of a tree are distinct is 
given below: To extract the traversals of the tree from the euler tour, mantain a count of 
how many times a given element appeared in the euler tour. For preorder, if it 
appeared once, add it to the traversal, for inorder twice, and for postorder thrice.

getCount([],_1,_2) :- false.
getCount([(X,Y)|L],N,C) :- ((X == N) -> C is Y) ; getCount(L,N,C).

incrCount([],N,LF) :- LF = [(N,1)].
incrCount([(N,Y)|L],N,LF) :- Yp1 is Y+1, LF = [(N,Yp1)|L].
incrCount([(X,Y)|L],N,LF) :- (incrCount(L,N,LP), LF = [(X,Y)|LP]).

extractFromET([],_1,_2,F,F).
extractFromET([N|L], Cnt, Num, E, F) :- 
    (incrCount(Cnt, N, CntNew), getCount(CntNew,N,C), (C == Num -> append(E,[N],E1); E1=E), extractFromET(L,CntNew,Num,E1,F)). 

preET(BT,L) :- ibt(BT), eulerTour(BT,T), extractFromET(T,[],1,[],L).
inET(BT,L) :- ibt(BT), eulerTour(BT,T), extractFromET(T,[],2,[],L).
postET(BT,L) :- ibt(BT), eulerTour(BT,T), extractFromET(T,[],3,[],L).

This fails for the tree testibt_dup 
(euler tour = [1, 2, 2, 4, 4, 4, 2, 1, 3, 5, 5, 5, 3, 3, 7, 7, 7, 3, 3, 3, 1]
as it needs to 'look ahead' to distinguish between two duplicate nodes.*/

etToTree([],empty).
etToTree(ET, N) :- append([X|L1],[X|LT],ET), append(L2,[X],LT), etToTree(L1,L), etToTree(L2,R), N = node(X,L,R).

preET(BT,L) :- ibt(BT), eulerTour(BT,ET), etToTree(ET,T), preorder(T,L).
inET(BT,L) :- ibt(BT), eulerTour(BT,ET), etToTree(ET,T), inorder(T,L).
postET(BT,L) :- ibt(BT), eulerTour(BT,ET), etToTree(ET,T), postorder(T,L).

/* simple string concatenation */
toString(empty,"()").
toString(node(N,L,R), S) :- ibt(node(N,L,R)), toString(L, Lstr), toString(R, Rstr), 
                            number_string(N, Nstr), 
                            string_concat("(", Nstr, S1),
                            string_concat(S1, ", ", S2),
                            string_concat(S2, Lstr, S3),
                            string_concat(S3, ", ", S4),
                            string_concat(S4, Rstr, S5),
                            string_concat(S5, ")", S).

/* the definition of balanced here is that |height(L) - height(R)| < 2 for all
nodes in the tree (the AVL definition of balanced)*/
isBalanced(empty).
isBalanced(node(N,L,R)) :- ibt(node(N,L,R)), isBalanced(L), isBalanced(R), height(L,Lh), height(R, Rh), abs(Lh-Rh) < 2.

allNodes(empty,[]).
allNodes(node(N,L,R), T) :- allNodes(L, Ln), allNodes(R, Rn), append(Ln,Rn,T1), T = [N|T1].

allLessThan([],_).
allLessThan([V|L],N) :- V < N, allLessThan(L,N).

allGreaterThan([],_).
allGreaterThan([V|L],N) :- V > N, allGreaterThan(L,N).

/* check if all nodes on the left are lesser than the node and all those on the
right are less than it for every node (if it's a BST) */
isBST(empty).
isBST(node(N,L,R)) :- ibt(node(N,L,R)), allNodes(L,Ln), allNodes(R,Rn), allLessThan(Ln,N), allGreaterThan(Rn,N).

/* MakeBST takes a list, sorts it and then creates a binary search tree by splitting
it into two at every step recursively and if the elements are singletons, making
them the leaves of the tree */
split([N],[],N,[]).
split(L,A,N,B) :- length(L,Len), divmod(Len,2,LA,Rem), 
    (Rem =:= 1 -> append(A,[N|B],L), length(A,LA), length(B,LA));
    (append(A,[N|B],L), length(A,LA), length([N|B],LA)).
makeBST([],_).
makeBST(L, BST) :- is_list(L), sort(L,LS), split(LS,A,N,B), makeBST(A,LBST), makeBST(B,RBST), BST = node(N,LBST,RBST).


lookup(_, empty) :- false.
lookup(N, node(Root,L,R)) :- isBST(node(Root,L,R)), ((N == Root); ((N < Root) -> lookup(N,L); lookup(N,R))).

/* returns the predecessor (successor) of the element X in a given list. Useful
for extracting the inorder predecessor (successor) of an element from a tree */
pred(X,L,P) :- append(_,[P,X|_],L).
succ(X,L,P) :- append(_,[X,P|_],L).

/* simple find and insert: the empty node is replaced with a new node */
insert(N, empty, BST2) :- integer(N), BST2 = node(N,empty,empty).
insert(N, node(Root,L,R), BST2) :- isBST(node(Root,L,R)), integer(N), 
    ((N < Root) -> insert(N, L, BST2L), BST2 = node(Root,BST2L,R)); 
    (N > Root) -> insert(N, R, BST2R), BST2 = node(Root,L,BST2R).

/* delete is a bit more involved: if the node is a leaf node, just delete it.
If not, then find it's inorder predecessor and delete it, and then make a new tree
with the inorder predecessor as the root and the tree made from deleting the inorder
predecessor from the left subtree as the left root 

This relies on two claims:
1. there exists an element which is the inorder predecessor of all elements, and
   this element is a leaf node of the tree. Hence, our algorithm terminates
2. The inorder predecessor of a node always lies in the left subtree of that node.

This algorithm can recursively thus delete a node from any given binary search 
tree (both balanced and unbalanced) */
delete(_N, empty, _BST2).
delete(N, node(N,empty,empty), BST2) :- integer(N), BST2 = empty.
delete(N, node(Root,L,R), BST2) :- isBST(node(Root,L,R)), integer(N),
    ((N < Root) -> delete(N, L, BST2L), BST2 = node(Root,BST2L,R)); 
    ((N > Root) -> delete(N, R, BST2R), BST2 = node(Root,L,BST2R));
    (inorder(node(Root,L,R),T), pred(N,T,Pred), delete(Pred, L, BST2L), BST2 = node(Pred,BST2L,R)).
