structure DataTypes =
struct 
    datatype PROG  = PROG of (DEC list)*(CMD list)
    and      DEC   = INT of string | BOOL of string 
    and      CMD   = RD of string | WR of EXPR | WH of EXPR*(CMD list) | 
                     ITE of EXPR*(CMD list)*(CMD list) | SET of string*EXPR
    and      EXPR  = ADD of EXPR*EXPR | SUB of EXPR*EXPR | 
                     MUL of EXPR*EXPR | DIV of EXPR*EXPR | 
                     MOD of EXPR*EXPR | NUM of int | REF of string |
                     OR of EXPR*EXPR | AND of EXPR*EXPR | NOT of EXPR |
                     LT of EXPR*EXPR | GT of EXPR*EXPR | GE of EXPR*EXPR |
                     LE of EXPR*EXPR | EQ of EXPR*EXPR | NE of EXPR*EXPR |
                     TRUE | FALSE
end;
