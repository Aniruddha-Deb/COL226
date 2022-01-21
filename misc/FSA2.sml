(* From Cormen, Leiserson and Rivest

 Assume an alphabet A with A* being the set of all finite strings from A. We use
 y ≽ z to denote that y is a suffix of z i.e. z = xy for some string x. 

 suffix function σ: Given a string pattern P[1..p],

 σ: A* -> 0..p. σ(x) is the length of the longest prefix of P which is a suffix of x
 i.e. σ(x) = max {k| P_k ≽ x}.

 There is a string matching determininistic automaton D(P) for every pattern P.

 The string matching automaton D(P) = (Q, A, 0, p, δ) corresponding to P[1..p] 
 states Q = {0, ..., p},
 alphabet A, 
 0 is the start state, 
 p is the lone accepting state and 
 δ: Q x A -> Q is the transition function defined  by 
 δ(q, a) = σ(P_q a) for each state q ∊ Q and a ∊ A. 
 
 The final state function φ: A* -> Q as  φ(ε) = 0 ∊ Q and φ(wa) = δ(φ(w), a)

 A string x ∊ A* is accepted by aut(P) iff φ(x) = p

 Lemma 1. (Suffix-function inequality) For any x ∊ A* and a ∊ A, σ(xa) ≤ σ(x)+1
 Proof: If σ(xa) = 0, then the lemma holds trivially. Otherwise σ(xa) > 0. Let
        r = σ(xa). By definition of σ, P_r = P[1..r] ≽ xa which implies P[r] = a.
	Hence P_(r-1)=P[1..(r-1)] ≽ x ∧  r-1 ≤ σ(x) ⇒ r = σ(xa) ≤ σ(x)+1. 

 Lemma 2. (Suffix-function recursion) For any x ∊ A* and a ∊ A, if q = σ(x) then
          σ(xa) = σ(P[1..q]a).
 Proof. By definition of σ, we have q = σ(x) ⇒ P_q = P[1..q] ≽ x ⇒ P[1..q]a ≽ xa.
        By lemma 1, we have r = σ(xa) ≤ σ(x)+1 = q+1. 
	Clearly P_r = P[1..r] ≽ xa and |P[1..r]| ≤ P[1..q]a  ⇒ P[1..r] = P[1..q]a.
	Hence r = σ(xa) ≤ σ(P[1..q]a)......(1).
        But we also have σ(P[1..q]a) ≤ σ(xa) ..........(2)
	since P[1..q]a ≽ xa. Hence σ(P[1..q]a) ≤ r = σ(xa). By (1) and (2) we have
	r = σ(xa) = σ(P[1..q]a).


 Theorem. For any text T[1..t], φ(T[1..i]) = σ(T[1..i]) for all 0<=i<=t.
 Proof. By induction on i. For i = 0, T[1..0] = ε and hence φ(T[1..0]) = 0 = σ(T[1..i]),
        where 0 is the start state. Assume the induction hypothesis (IH) that 
	for some i ≥ 0, φ(T[1..i]) = σ(T[1..i]) ................. (IH)
        and consider φ(T[1..(i+1)]) where T[i+1] = a. Let q = φ(T[1..i]).
        
                                                φ(T[1..(i+1)])
        =  {By definition of T[1..(i+1)] and a} φ(T[1..i]a)
        =  {By definition of φ}                 δ(φ(T[1..i]), a)
        =  {By definition of q}                 δ(q, a)
        =  {By definition of δ}                 σ(P[1..q]a)
        =  {By Lemma 2 and IH}                  σ(T[1..i]a)
        =  {By definition of T[1..(i+1)]}       σ(T[1..(i+1)])
*)

use "/home/self/sak/sml/programs/smlio/stdio/stdIO.sml";
open Stdio;

(*********************************************************

  Construct the DFA for the pattern.

  The time complexity of the construction is O(p^3|A|). Since the array of size 
  (p+1)*|A| needs to be filled with integers which requires the computation 
  of the longest prefix of the pattern which is also a suffix, it requires a 
  computation time of p^2.

  The space complexity is the size of the array i.e. O(p|A|).

**********************************************************)
open Array2;

val i2s = Int.toString
	      
val c2s = Char.toString

fun isSuffix (L, M) =
    let val l = length L
        and m = length M
    in ((l <= m) andalso (L = List.drop (M, m-l)))
    end

exception EmptyPattern

fun fsa () =  
    let (* Need to delay the input till after the prompt is output *) 
	val inps = fn x:string => readln () 
    in  (  writeln ("NOTE:");
	   writeln ("\t1. Only the 95 printable ASCII chars (from ord 32 to 126) are allowed;");
	   writeln ("\t2. Do not enclose in quotes;");
	   writeln ("\t3. Terminate pattern with [ENTER]");
           write ("Pattern = "); (* user prompt *)
	   let val str =   (inps "") (* read user input *)
	   in  str
	   end
	)
    end 
    
val pat = fsa ();

val p = size pat
val chlist = explode pat
exception prefixLength
fun prefix k = (* prefix of pat of length k *)
    if (k < 0) orelse (k > p) then raise prefixLength
    else List.take (chlist, k)
val space = chr(32) and tilde = chr(126) (* 95 characters *)
val rows = p+1 (* rows 0..p in automaton *)
val cols = 126-32 + 1; (* cols corresponds to number of different input chars *)
(* create an array of size p+1 x 95 *)

writeln ("*********************************************************\n\tAutomaton has "^(i2s rows)^ " * "^(i2s cols)^ " = "^(i2s (rows*cols))^" entries of the form <src>--<chr>--><tgt>\n\tOnly the interesting entries are shown below\n\tAll other entries go to state 0.\n********************************************************");

fun longestPrefixSuffix (r, c, k) =
    if isSuffix(prefix(k), prefix(r)@[chr(c +32)])
    then k
    else longestPrefixSuffix (r, c, k-1)
fun entry (r,c) =
    let val k = longestPrefixSuffix (r, c, Int.min (p, r+2)) 
    in (
        if k = 0 then write("")
        else writeln (i2s(r)^"--"^c2s(chr(c+32))^"-->"^i2s(k));
	k
       )
    end 
        
val aut = tabulate RowMajor (rows, cols, (fn (r,c) => entry(r,c)));

(***************** End of FSA construction ***********************)

exception emptyText
exception PatternLongerThanText

fun finalStates ([], q, F, s) =
    if q = p then rev ((s-q)::F) else  rev F
  | finalStates ((h::chListTail), q, F, s) =
    let val q' =  sub (aut, q, ord(h)-32)
    in 
	if q = p then finalStates (chListTail, q', ((s-q)::F), s+1)
	else finalStates (chListTail, q', F, s+1)
    end
	
fun fsaMatch (text) =
    let 
	val t = size text
	val chList = explode text
	(* assume initially nonempty text 
	   q is the current state of the automaton
	   F is the list indices of text which matched so far	
	   s is the count of characters from text consumed so far
	 *)
	    
    in  if  t = 0 then raise emptyText
	else if  p > t then raise  PatternLongerThanText
	else (* 0 < p <= t *)
	    ( writeln ("The list of starting indices where \""^pat^"\"  occurs is");
	      finalStates (chList, 0, [], 0)
	    )
    end

(************************************************* 
  Time and space complexity for finding all matches of the pattern is exactly Θ(|text|). 
**************************************************)
	
fun textInp () =
    let (* Need to delay the input till after the prompt is output *) 
	val inps = fn x:string => readln () 
    in  (  writeln ("NOTE: Follow the same NOTEs as for pattern string");
           write ("Text string (Empty string to EXIT) = "); (* user prompt *)
	   let val str =   (inps "") (* read user input *)
	   in
	      str
	   end
	)
    end; 
    
(* val text = textInp (); *)

use "/home/self/sak/sml/programs/List.toString.sml";

fun readEvalLoop () =  
    let val text = textInp () (* read user input *)
    in  (
	 if (text = "") then writeln ("End of conversation")
         else ( writeln (toString Int.toString (fsaMatch text));
		writeln ("");
		readEvalLoop ()
	      )
	)
    end;

writeln ("");
readEvalLoop () 
