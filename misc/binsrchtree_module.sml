(* This is a module implementation of BINARY SEARCH TREES *)


(* A Binary Search Tree  or a BST is a binary tree with nodes labelled by elements
   TOTALLY ordered by an IRREFLEXIVE-TRANSITIVE relation "lt" such that
   for any node y in the tree, 

   o   lt (x, y) holds for all nodes x in the LEFT subtree of y,  and 
   o   lt (y, z) holds for all nodes z in the RIGHT subtree of y.

*)

(* can one use "bintree.sml" ? *)

(* How does one specialize a binary tree to a BST? *)

(* We assume that we are dealing with BSTs in the following examples. *)

(* Searching in a BST: checking for a node labelled x *)


signature TOTALORDER =
sig
    eqtype eltype
    val lt : eltype * eltype -> bool
    val toString : eltype -> string
end (* sig *);

structure INTLTLAB: TOTALORDER =
struct
    type eltype = int
    val  lt = op<
    val toString = Int.toString
end

structure INTGTLAB: TOTALORDER =
struct
    type eltype = int
    val  lt = op>
    val toString = Int.toString
end
structure STRINGLEXLTLAB: TOTALORDER =

struct
   
   type eltype = string;
   (* Lexicographic ordering '<' on strings *)

   fun lexlt (s, t) =
       let val Ls = explode (s);
   	val Lt = explode (t);
   	fun lstlexlt (_, []) = false
   	|   lstlexlt ([], (b:char)::M) = true
   	|   lstlexlt (a::L, b::M) = 
   		   if (a < b) then true 
   		   else if (a = b) then lstlexlt (L, M)
   			else false
           ;
        in	lstlexlt (Ls, Lt)
        end
   ;
  
   val lt = lexlt;
   val toString = String.toString

end (* struct *);

functor MakeBST (Lt: TOTALORDER):
   sig
	type 'a bintree;
	exception Empty_tree;
	val create: Lt.eltype bintree;
	val lookup: Lt.eltype * Lt.eltype bintree -> bool;
	val insert: Lt.eltype * Lt.eltype bintree -> Lt.eltype bintree;
	val deletemin: Lt.eltype bintree -> Lt.eltype * Lt.eltype bintree;
	val delete: Lt.eltype * Lt.eltype bintree -> Lt.eltype bintree;
	val drawTree: Lt.eltype bintree -> unit
	val fromList: Lt.eltype list -> Lt.eltype bintree 
   end
=
   struct
      open Lt;
      datatype 'a bintree = 
	       Leaf |
	       Node of 'a * 'a bintree * 'a bintree
      ;
      local fun drawTabs (n) = 
                if n>0 then 
		    (if n = 1 then print ("----")
		     else print("\t"); drawTabs (n-1)
		    )
                else ();
		
          fun drawTreeTabs (Leaf, n) = 
              (drawTabs (n); print(".\n"))
            |   drawTreeTabs (Node (a, left, right), n) =
                (drawTreeTabs (right, n+1);
                 drawTabs (n); print(toString (a)); 
                 print("\n");
                 drawTreeTabs (left, n+1)
                )
      in fun drawTree (t) = (print ("\n");
                             drawTreeTabs(t, 0); print ("\n"))
      end;

      val create = Leaf;

      fun lookup (x, Leaf) = false
	| lookup (x, Node (y, left, right)) =
      	  if x=y then true
      	  else if lt (x, y) then lookup (x, left)
      	  else lookup (x, right)
      ;
      
      (* Insert an element into a BST *)
      
      fun insert  (x, Leaf) = Node (x, Leaf, Leaf)
      |   insert (x, T as Node (y, left, right)) =
      	   if x=y then T				(* do nothing *)
      	   else if lt (x, y) then Node (y, insert (x, left), right)
      	   else Node (y, left, insert (x, right))
      ;
      
      fun fromList L = List.foldr insert Leaf (rev L)

      (* Delete (if it is there) from a BST *)
      
      (* Deletion requires the following function which can delete the  
         smallest element from a tree; Note that the smallest element in a 
         BST is the leftmost leaf-node in the tree (if it exists, otherwise 
         the root). The function deletemin should also return the value of the 
         smallest element to enable tree reordering for deletion.
      *)
      
      exception Empty_tree;
      
      fun deletemin (Leaf) = raise Empty_tree
      |   deletemin (Node (y, Leaf, right)) = (y, right)
      |   deletemin (Node (y, left, right)) =
      	      let val (z, L) = deletemin (left)
      	      in  (z, Node (y, L, right))
      	      end
      ;
      
      (* NOTE that deletemin does not require the comparison function lt as a 
         parameter.
      *)
      
      fun delete (x, Leaf) = Leaf
      |   delete (x, Node (y, left, right)) =
      	   if x=y then 
      	      if left = Leaf then right
      	      else if right = Leaf then left
      	      else (* extract the smallest element from the right subtree
      		      and make it the root of the new tree
      		   *)
      		   let val (z, R) = deletemin (right)
      		   in  Node (z, left, R)
      		   end
       	   else if lt (x, y) then Node (y, delete (x, left), right)
		else	Node (y, left, delete (x, right))
      ;
      
   end (* struct *);

(* Now we may apply the functor MakeBST to STRINGLAB to obtain a new structure
   StringBST which defines "binary search trees labelled by strings ordered by
   the lexicographic total ordering.
*)

structure StringLexltBST = MakeBST (STRINGLEXLTLAB);

structure IntLtbst = MakeBST (INTLTLAB)

structure IntGtbst = MakeBST (INTGTLAB)
