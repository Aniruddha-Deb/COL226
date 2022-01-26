
auto rnd = Random(69);
const int iters = 2;
const int iters2 = 2;
const int iters3 = 2;
const int iters4 = 2;
import std.conv;
int abs(int a){
  if (a < 0) return -a;
  return a;
}
struct node {
  int data;
  node *left;
  node *right;
  this(int num){
    data = num;
    left = null;
    right = null;
  }
  void insert_random(int n){
    int i = uniform(0, 2, rnd);
    if (i == 0){
      if (left == null){
	left = new node(n);
      }
      else {
	(*left).insert_random(n);
      }
    } 
    else {
      if (right == null){
	right = new node(n);
      }
      else {
	(*right).insert_random(n);
      }
    }
  }
  void insert_balanced(int n){
    int h1 = 0, h2 = 0;
    if (left == null){
      left = new node(n);
      return;
    }
    if (right == null){
      right = new node(n);
      return;
    }
    h1 = (*left).getHeight();
    h2 = (*right).getHeight();
    if (h1 <= h2){
      (*left).insert_balanced(n);
    }
    else {
      (*right).insert_balanced(n);
    }
  }
  void insert_ordered(int n){
    if (data == n){
      return;
    }
    if (data < n){
      if (right == null){
	right = new node(n);
      }
      else {
	(*right).insert_ordered(n);
      }
    }
    else {
      if (left == null){
	left = new node(n);
      }
      else {
	(*left).insert_ordered(n);
      }
    }
  }
  string toString(){
    import std.conv;
    string s = "(";
    s ~= to!string(data);
    s ~= ", ";
    if (left == null){
      s ~= "(), ";
    }
    else {
      s ~= (*left).toString();
      s ~= ", ";
    }
    if (right == null){
      s ~= "())";
    }
    else {
      s ~= (*right).toString();
      s ~= ")";
    }
    return s;
  }
  string getTree(){
    import std.conv;
    string s = "node(";
    s ~= to!string(data);
    s ~= ", ";
    if (left == null){
      s ~= "empty, ";
    }
    else {
      s ~= (*left).getTree();
      s ~= ", ";
    }
    if (right == null){
      s ~= "empty)";
    }
    else {
      s ~= (*right).getTree();
      s ~= ")";
    }
    return s;
  }
  string getTourRec(){
    import std.conv;
    string s = "";
    s ~= to!string(data);
    s ~= ", ";
    if (left != null){
      s ~= (*left).getTourRec();
      s ~= ", ";
    }
    s ~= to!string(data);
    s ~= ", ";
    if (right != null){
      s ~= (*right).getTourRec();
      s ~= ", ";
    }
    s ~= to!string(data);
    return s;
  }
  string getTour(){
    string s = "[";
    s ~= getTourRec();
    s ~= "]";
    return s;
  }
  string getPreorderRec(){
    import std.conv;
    string s = "";
    s ~= to!string(data);
    s ~= ", ";
    if (left != null){
      s ~= (*left).getPreorderRec();
    }
    if (right != null){
      s ~= (*right).getPreorderRec();
    }
    return s;
  }
  string getPreorder(){
    string s = "[";
    s ~= getPreorderRec();
    s.length -= 2;
    s ~= "]";
    return s;
  }
  string getInorderRec(){
    import std.conv;
    string s = "";
    if (left != null){
      s ~= (*left).getInorderRec();
    }
    s ~= to!string(data);
    s ~= ", ";
    if (right != null){
      s ~= (*right).getInorderRec();
    }
    return s;
  }
  string getInorder(){
    string s = "[";
    s ~= getInorderRec();
    s.length -= 2;
    s ~= "]";
    return s;
  }
  string getPostorderRec(){
    import std.conv;
    string s = "";
    if (left != null){
      s ~= (*left).getPostorderRec();
    }
    if (right != null){
      s ~= (*right).getPostorderRec();
    }
    s ~= to!string(data);
    s ~= ", ";
    return s;
  }
  string getPostorder(){
    string s = "[";
    s ~= getPostorderRec();
    s.length -= 2;
    s ~= "]";
    return s;
  }
  int getSize(){
    int s = 1;
    if (left != null){
      s += (*left).getSize();
    }
    if (right != null){
      s += (*right).getSize();
    }
    return s;
  }
  int getHeight(){
    int h1 = 0, h2 = 0;
    if (left != null){
      h1 = (*left).getHeight();
    }
    if (right != null){
      h2 = (*right).getHeight();
    }
    if (h1 > h2) return h1+1;
    else return h2+1;
  }
  
  bool isBalanced(){
    bool b = true;
    int h1 = 0, h2 = 0;
    if (left != null){
      b &= (*left).isBalanced();
      if (!b) return false;
      h1 = (*left).getHeight();
    }
    if (right != null){
      b &= (*right).isBalanced();
      if (!b) return false;
      h2 = (*right).getHeight();
    }
    if (abs(h1-h2) > 1){
      return false;
    }
    return true;
  }
  string make_tree_tests(int test_number){
    import std.conv;
    string s = "build_tree"~to!string(test_number)~"(BT):- BT = ";
    s ~= getTree();
    s ~= ".\n";
    s ~= "get_size"~to!string(test_number)~"(N):- N = ";
    s ~= to!string(getSize());
    s ~= ".\n";
    s ~= "get_height"~to!string(test_number)~"(N):- N = ";
    s ~= to!string(getHeight());
    s ~= ".\n";
    s ~="get_inorder"~to!string(test_number)~"(L):- L = ";
    s ~= getInorder();
    s ~= ".\n";
    s ~="get_postorder"~to!string(test_number)~"(L):- L = ";
    s ~= getPostorder();
    s ~= ".\n";
    s ~="get_preorder"~to!string(test_number)~"(L):- L = ";
    s ~= getPreorder();
    s ~= ".\n";
    s ~="get_tour"~to!string(test_number)~"(L):- L = ";
    s ~= getTour();
    s ~= ".\n";
    if (isBalanced()){
      s ~= "is_balanced"~to!string(test_number)~".\n";
    }
    else {
      s ~= "is_balanced"~to!string(test_number)~":- fail.\n";
    }
    s ~= "get_string"~to!string(test_number)~"(S):- S = \"";
    s ~= toString();
    s ~= "\".\n";
    return s;
  }
}
string tests(int num){
  import std.conv;
  string n = to!string(num);
  string s = "run_test"~n~":-";
  s ~= "build_tree"~n~"(BT),\n";
  s ~= "preorder(BT, L1),\n";
  s ~= "postorder(BT, L2),\n";
  s ~= "inorder(BT, L3),\n";
  s ~= "trPreorder(BT, L4),\n";
  s ~= "trPostorder(BT, L5),\n";
  s ~= "trInorder(BT, L6),\n";
  s ~= "preET(BT, L7),\n";
  s ~= "postET(BT, L8),\n";
  s ~= "inET(BT, L9),\n";
  s ~= "eulerTour(BT, L10),\n";
  s ~= "size(BT, N1), \n";
  s ~= "height(BT, N2), \n";
  s ~= "toString(BT, S),\n";
  s ~= "get_size"~n~"(N1),\n";
  // s ~= "write(\"size pass\"),";
  s ~= "get_height"~n~"(N2),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_preorder"~n~"(L1),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_preorder"~n~"(L4),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_preorder"~n~"(L7),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_postorder"~n~"(L2),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_postorder"~n~"(L5),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_postorder"~n~"(L8),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_inorder"~n~"(L3),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_inorder"~n~"(L6),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_inorder"~n~"(L9),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_tour"~n~"(L10),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "get_string"~n~"(S),\n";
  //s ~= "write(\"size pass\"),";
  s ~= "checkSame(is_balanced"~n~", isBalanced(BT)).\n";
  //s ~= "write(\"size pass\").\n";
  return s;
}
bool isIn (int a, int [] arr){
  for (int i = 0; i < arr.length; i ++){
    if (arr[i] == a) return true;
  }
  return false;
}
string listToString(int [] arr){
  string s = "[";
  for (int i = 0; i < arr.length; i ++){
    s ~= to!string(arr[i]);
    if (i != arr.length-1){
      s ~= ", ";
    }
  }
  s ~= "]";
  return s;
}
string make_bst_tests(int num){
  string n = to!string(num);
  int [] arr;
  int m = uniform(40, 60, rnd);
  for (int i = 0; i < m; i ++){
    int u = uniform(1, 100, rnd);
    if (!isIn(u, arr)){
      arr ~= u;
    }
  }
  auto start = node(arr[0]);
  for (int i = 1; i < arr.length; i ++){
    start.insert_ordered(arr[i]);
  }
  int [] added;
  int [] lookup;
  int [] afterInsertion;
  int [] afterDeletion;
  for (int i = 0; i < 10; i ++){
    int u = uniform(1, 100, rnd);
    if (!isIn(u, arr)){
      if (!isIn(u, added))
	added ~= u;
    }
    else {
      lookup ~= u;
    }
  }
  for (int i = 0; i < arr.length; i ++){
    afterInsertion ~= arr[i];
    if (!isIn(arr[i], lookup)){
      afterDeletion ~= arr[i];
    }
  }
  for (int j = 0; j < added.length; j ++){
    afterInsertion ~= added[j];
    afterDeletion ~= added[j];
  }
  import std.algorithm;
  sort(afterInsertion);
  sort(afterDeletion);
  
  string s = "build_bst"~n~"(BT):- BT = " ~ start.getTree() ~".\n";
  s ~= "after_insertion_list"~n~"(L):- L = " ~ listToString(afterInsertion) ~ ".\n";
  s ~= "after_deletion_list"~n~"(L):- L = " ~ listToString(afterDeletion) ~ ".\n";
  s ~= "run_bst_test"~n~":- build_bst"~n~"(BT0),\n";
  for (int i = 0; i < added.length; i ++){
    s ~= "not(lookup("~to!string(added[i])~", BT0)),\n";
  }
  for (int i = 0; i < lookup.length; i ++){
    s ~= "lookup("~to!string(lookup[i])~", BT0),\n";
  }
  s ~= "isBST(BT0),\n";
  
  for (int i = 1; i <= added.length; i ++){
    s ~= "insert("~to!string(added[i-1])~", BT"~to!string(i-1)~", BT"~to!string(i)~"),\n";
  }
  string fBt = "BT"~to!string(added.length);
  s ~= "isBST("~fBt~"),\n";
  s ~= "inorder("~fBt~", L1), \n";
  s ~= "after_insertion_list"~n~"(L1), \n";
  for (ulong i = added.length+1; i <= added.length + lookup.length; i ++){
    s ~= "delete("~to!string(lookup[i-added.length-1])~", BT" ~to!string(i-1)~", BT"~ to!string(i) ~"), \n";
  }
  fBt = "BT"~to!string(added.length + lookup.length);
  s ~= "isBST("~fBt~"),\n";
  s ~= "inorder("~fBt~", L2), \n";
  s ~= "after_deletion_list"~n~"(L2). \n";
  return s;
}
string make_random_array(int num){
  int [] arr;
  int n = uniform(40, 60, rnd);
  for (int i = 0; i < n; i ++){
    int u = uniform(1, 100, rnd);
    if (!isIn(u, arr)){
      arr ~= u;
    }
  }
  string s = "random_array"~to!string(num)~"(L):- L = [";
  for (int i = 0; i < arr.length; i ++){
    s ~= to!string(arr[i]);
    if (i != arr.length - 1)
      s ~= ", ";
  }
  s ~= "].\n";
  import std.algorithm;
  sort(arr);
  s ~= "sorted_array"~to!string(num)~"(L):- L = [";
  for (int i = 0; i < arr.length; i ++){
    s ~= to!string(arr[i]);
    if (i != arr.length - 1)
      s ~= ", ";
  }
  s ~= "].\n";
  s ~= "run_make_bst_test"~to!string(num)~":- random_array"~to!string(num)~"(L), 
        makeBST(L, BT),
        isBST(BT), 
        isBalanced(BT),
        inorder(BT, Sorted),
        sorted_array"~to!string(num)~"(Sorted).\n";
  return s;
}

import std.random;
void main(){
  import std.stdio;
  string s = "checkSame(A, B):- (A, B);(not(A),not(B)).\n";
  for (int j = 0; j < iters; j ++){
    int size = uniform(20, 40, rnd);
    node start = node(uniform(1, 100, rnd));

    for (int i = 0; i < size; i ++){
      start.insert_random(uniform(1, 100, rnd));
    }
    s ~= (start.make_tree_tests(j));
  }
  for (int j = 0; j < iters; j ++){
    s ~= tests(j);
  }
  for (int j = iters; j < iters+iters3; j ++){
    int size = uniform(20, 40, rnd);
    node start = node(uniform(1, 100, rnd));

    for (int i = 0; i < size; i ++){
      start.insert_balanced(uniform(1, 100, rnd));
    }
    s ~= (start.make_tree_tests(j));
  }
  for (int j = iters; j < iters+iters3; j ++){
    s ~= tests(j);
  }
  s ~= "run_all_BT_tests:-";
  for (int j = 0; j < iters+iters3-1; j ++){
    s ~= "run_test"~to!string(j)~", \n";
  }
  s ~= "run_test"~to!string(iters+iters3-1)~". \n";
  import std.file;
  for (int i = 0; i < iters2; i ++){
    s ~= make_random_array(i);
  }
  s ~= "run_all_make_bst_tests:-";
  for (int j = 0; j < iters2-1; j ++){
    s ~= "run_make_bst_test"~to!string(j)~", \n";
  }
  s ~= "run_make_bst_test"~to!string(iters2-1)~". \n";
  for (int i = 0; i < iters4; i ++){
    s ~= make_bst_tests(i);
  }
  s ~= "run_all_bst_tests:-";
  for (int j = 0; j < iters4-1; j ++){
    s ~= "run_bst_test"~to!string(j)~", \n";
  }
  s ~= "run_bst_test"~to!string(iters4-1)~". \n";
  s ~= "run_tests:- run_all_BT_tests, 
                    write(\" size correct\n height correct\n preorder correct\n postorder correct\n inorder correct\n trPreorder correct\n trPostorder correct\n trInorder correct\n eulerTour correct\n preET correct\n inET correct\n postET correct\n toString correct\n isBalanced correct\n\"), 
                    run_all_make_bst_tests, 
                    write(\" makeBST correct \n\"),
                    run_all_bst_tests,
                    write(\" isBST correct \n lookup correct \n insert correct \n delete correct \n All correct \n\").\n";
  toFile(s, "bstTests.pl");
}
