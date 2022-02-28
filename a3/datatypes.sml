structure DataTypes = struct
    datatype Keyword = PROGRAM | VAR | INT | BOOL | READ | WRITE | IF | THEN | ELSE |
    ENDIF | WHILE | DO | ENDWH | TRUE | FALSE

    datatype Separator = BLOCKSTART | COLON | SEMICOLON | COMMA | LBRACE | RBRACE | 
    LPAREN | RPAREN

    datatype Operator = ADD | UMINUS | SUB | MUL | DIV | MOD | EQ | NE | GT | GE | 
    LT | LE | AND | OR | NOT | ASSIGN

    datatype Token = KEYWORD of Keyword | SEPARATOR of Separator | IDENTIFIER of string |
    OPERATOR of Operator | NUMERAL of int | EOF

    datatype VarType = Integer | Boolean

    datatype ASTNodeData = Program of string | DeclSeq | Declaration of string*VarType |
    CmdSeq | Assignment of string | While | If | Read | Write | Expression of Operator | 
    IntLiteral of int | BoolLiteral of bool

    datatype AST = empty | Node of int*ASTNodeData*(AST list)
end;
