open DataTypes

type lexresult = int * Token

fun inc a = a := !a + 1
val linenum = ref 1

val eof = fn () => (!linenum, EOF)
%%
%structure WhileLex
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%
\n => (inc linenum; lex());
{ws}+ => (lex());

"while" => ((!linenum,KEYWORD WHILE));
"if" => ((!linenum,KEYWORD IF));
"then" => ((!linenum,KEYWORD THEN));
"else" => ((!linenum,KEYWORD ELSE));
"endif" => ((!linenum,KEYWORD ENDIF));
"endwh" => ((!linenum,KEYWORD ENDWH));
"read" => ((!linenum,KEYWORD READ));
"write" => ((!linenum,KEYWORD WRITE));
"program" => ((!linenum,KEYWORD PROGRAM));
"var" => ((!linenum,KEYWORD VAR));
"int" => ((!linenum,KEYWORD INT));
"bool" => ((!linenum,KEYWORD BOOL));
"do" => ((!linenum,KEYWORD DO));
"tt" => ((!linenum,KEYWORD TRUE));
"ff" => ((!linenum,KEYWORD FALSE));

"{" => ((!linenum,SEPARATOR LBRACE));
"}" => ((!linenum,SEPARATOR RBRACE));
"(" => ((!linenum,SEPARATOR LPAREN));
")" => ((!linenum,SEPARATOR RPAREN));
"," => ((!linenum,SEPARATOR COMMA));
";" => ((!linenum,SEPARATOR SEMICOLON));
":" => ((!linenum,SEPARATOR COLON));
"::" => ((!linenum,SEPARATOR BLOCKSTART));

"+" => ((!linenum,OPERATOR ADD));
"~" => ((!linenum,OPERATOR UMINUS));
"-" => ((!linenum,OPERATOR SUB));
"*" => ((!linenum,OPERATOR MUL));
"/" => ((!linenum,OPERATOR DIV));
"%" => ((!linenum,OPERATOR MOD));
"=" => ((!linenum,OPERATOR EQ));
"<>" => ((!linenum,OPERATOR NE));
">" => ((!linenum,OPERATOR GT));
">=" => ((!linenum,OPERATOR GE));
"<" => ((!linenum,OPERATOR LT));
"<=" => ((!linenum,OPERATOR LE));
"&&" => ((!linenum,OPERATOR AND));
"||" => ((!linenum,OPERATOR OR));
"!" => ((!linenum,OPERATOR NOT));
":=" => ((!linenum,OPERATOR ASSIGN));

{digit}+ => ((!linenum,NUMERAL (foldl (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext))));
[A-Za-z][A-Za-z0-9]* => ((!linenum,IDENTIFIER yytext));

. => (print ("Unknown token found at " ^ (Int.toString (!linenum)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); lex());
