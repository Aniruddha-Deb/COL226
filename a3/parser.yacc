open DataTypes

%%
%nonterm program of AST | 
| procList of Proc list | parallel of Proc list
| pat of Pat | path of Path
| pi of Proc list | proc of Proc
| value of V

%pos int

%%
program: (l, KEYWORD PROGRAM) (m, IDENTIFIER s) (n, SEPARATOR BLOCKSTART) block 
    ((Node (s, block))
block: 

block
