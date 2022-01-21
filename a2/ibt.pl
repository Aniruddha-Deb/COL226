/*
integer binary tree
author: Aniruddha Deb
*/

ibt(empty).
ibt(node(N,L,R)) :- integer(N), ibt(L), ibt(R).

/* make a test binary tree */
leaf(T,N) :- T = node(N,empty,empty).

testibt(T) :- leaf(L5,5), leaf(L4,4), leaf(L7,7), T = node(1,node(2,empty,L4),node(3,L5,node(6,L7,empty))).

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

/* TODO think about this
makeBST([],_).
makeBST([A|L],node(N,L,R)) :- 

make this work in conjunction with
insert(N, BST1, BST2) 
delete(N, BST1, BST2)
*/

lookup(_, empty) :- false.
lookup(N, node(Root,L,R)) :- (N == Root); ((N < Root) -> lookup(N,L); lookup(N,R)).