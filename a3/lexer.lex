structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val eof = fn filename => T.TOK_EOF (!lin, !col);

fun inc a = a := !a + 1

%%
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName: string);
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");

%%
{ws}* => (continue ());
{eol} => (inc lin; eolpos:=yypos+size yytext; continue ());

"while" => (col:=yypos-(!eolpos); T.TOK_WHILE(!lin,!col));
"if" => (col:=yypos-(!eolpos); T.TOK_IF(!lin,!col));
"then" => (col:=yypos-(!eolpos); T.TOK_THEN(!lin,!col));
"else" => (col:=yypos-(!eolpos); T.TOK_ELSE(!lin,!col));
"endif" => (col:=yypos-(!eolpos); T.TOK_ENDIF(!lin,!col));
"endwh" => (col:=yypos-(!eolpos); T.TOK_ENDWH(!lin,!col));
"read" => (col:=yypos-(!eolpos); T.TOK_READ(!lin,!col));
"write" => (col:=yypos-(!eolpos); T.TOK_WRITE(!lin,!col));
"program" => (col:=yypos-(!eolpos); T.TOK_PROGRAM(!lin,!col));
"var" => (col:=yypos-(!eolpos); T.TOK_VAR(!lin,!col));
"int" => (col:=yypos-(!eolpos); T.TOK_INT(!lin,!col));
"bool" => (col:=yypos-(!eolpos); T.TOK_BOOL(!lin,!col));
"do" => (col:=yypos-(!eolpos); T.TOK_DO(!lin,!col));
"tt" => (col:=yypos-(!eolpos); T.TOK_TT(!lin,!col));
"ff" => (col:=yypos-(!eolpos); T.TOK_FF(!lin,!col));

"{" => (col:=yypos-(!eolpos); T.TOK_LBRACE(!lin,!col));
"}" => (col:=yypos-(!eolpos); T.TOK_RBRACE(!lin,!col));
"(" => (col:=yypos-(!eolpos); T.TOK_LPAREN(!lin,!col));
")" => (col:=yypos-(!eolpos); T.TOK_RPAREN(!lin,!col));
"," => (col:=yypos-(!eolpos); T.TOK_COMMA(!lin,!col));
";" => (col:=yypos-(!eolpos); T.TOK_SEMICOLON(!lin,!col));
":" => (col:=yypos-(!eolpos); T.TOK_COLON(!lin,!col));
"::" => (col:=yypos-(!eolpos); T.TOK_BLOCKSTART(!lin,!col));

"+" => (col:=yypos-(!eolpos); T.TOK_ADD(!lin,!col));
"~" => (col:=yypos-(!eolpos); T.TOK_UMINUS(!lin,!col));
"-" => (col:=yypos-(!eolpos); T.TOK_SUB(!lin,!col));
"*" => (col:=yypos-(!eolpos); T.TOK_MUL(!lin,!col));
"/" => (col:=yypos-(!eolpos); T.TOK_DIV(!lin,!col));
"%" => (col:=yypos-(!eolpos); T.TOK_MOD(!lin,!col));
"=" => (col:=yypos-(!eolpos); T.TOK_EQ(!lin,!col));
"<>" => (col:=yypos-(!eolpos); T.TOK_NE(!lin,!col));
">" => (col:=yypos-(!eolpos); T.TOK_GT(!lin,!col));
">=" => (col:=yypos-(!eolpos); T.TOK_GE(!lin,!col));
"<" => (col:=yypos-(!eolpos); T.TOK_LT(!lin,!col));
"<=" => (col:=yypos-(!eolpos); T.TOK_LE(!lin,!col));
"&&" => (col:=yypos-(!eolpos); T.TOK_AND(!lin,!col));
"||" => (col:=yypos-(!eolpos); T.TOK_OR(!lin,!col));
"!" => (col:=yypos-(!eolpos); T.TOK_NOT(!lin,!col));
":=" => (col:=yypos-(!eolpos); T.TOK_ASSIGN(!lin,!col));

{digit}+ => (col:=yypos-(!eolpos);
        T.TOK_NUM(foldl (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext), !lin, !col));
[A-Za-z][A-Za-z0-9]* => (col:=yypos-(!eolpos);T.TOK_ID(yytext,!lin,!col));

. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());
