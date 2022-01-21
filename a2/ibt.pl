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