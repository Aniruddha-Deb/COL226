signature VMC =
sig

    type VMCModel 
    type Value
    type Command

    val rules: Table.SymbolTable -> VMCModel -> VMCModel
    val toString: VMCModel -> string
    val postfix: DataTypes.CMD list -> Command FunStack.Stack
    val execute: Table.SymbolTable -> VMCModel -> VMCModel
    
end

structure Vmc : VMC =
struct

    datatype Value = Integer of int | Boolean of bool

    datatype Op = ADD | SUB | MUL | DIV | MOD | GT | GE | LT | LE | EQ | NE | AND | OR | NOT

    datatype Command = IntLiteral of int | Variable of string | BoolLiteral of bool 
                     | Block of Command list | ITE | WH | SET | Operator of Op
                     | RD | WR


    type VMCModel = Value FunStack.Stack * int array * Command FunStack.Stack

    fun rules table model = model

    fun toString model = "Not implemented yet"

    fun parse_expr (DataTypes.NUM n) = FunStack.push ((IntLiteral n),S)
      | parse_expr (DataTypes.IREF s) = FunStack.push ((Variable s),S)
      | parse_expr (DataTypes.BREF s) = FunStack.push ((Variable s),S)
      | parse_expr (DataTypes.TRUE) = FunStack.push ((BoolLiteral true), S)
      | parse_expr (DataTypes.FALSE) = FunStack.push ((BoolLiteral false), S)
      | parse_expr (DataTypes.NOT e) = 
    let
        val s1 = FunStack.push ((Operator NOT),S)
        val s2 = parse_expr S e1
    in s2 end
      | parse_expr S E =
    let
        val (s1,e1,e2) = case E of 
          | (DataTypes.ADD (e1,e2)) => ((FunStack.push ((Operator ADD),S)),e1,e2)
          | (DataTypes.SUB (e1,e2)) => ((FunStack.push ((Operator SUB),S)),e1,e2)
          | (DataTypes.MUL (e1,e2)) => ((FunStack.push ((Operator MUL),S)),e1,e2)
          | (DataTypes.DIV (e1,e2)) => ((FunStack.push ((Operator DIV),S)),e1,e2)
          | (DataTypes.MOD (e1,e2)) => ((FunStack.push ((Operator MOD),S)),e1,e2)
          | (DataTypes.OR (e1,e2)) => ((FunStack.push ((Operator OR),S)),e1,e2)
          | (DataTypes.AND (e1,e2)) => ((FunStack.push ((Operator AND),S)),e1,e2)
          | (DataTypes.LT (e1,e2)) => ((FunStack.push ((Operator LT),S)),e1,e2)
          | (DataTypes.GT (e1,e2)) => ((FunStack.push ((Operator GT),S)),e1,e2)
          | (DataTypes.GE (e1,e2)) => ((FunStack.push ((Operator GE),S)),e1,e2)
          | (DataTypes.LE (e1,e2)) => ((FunStack.push ((Operator LE),S)),e1,e2)
          | (DataTypes.EQ (e1,e2)) => ((FunStack.push ((Operator EQ),S)),e1,e2)
          | (DataTypes.NE (e1,e2)) => ((FunStack.push ((Operator NE),S)),e1,e2)
        val s2 = parse_expr s1 e1
        val s3 = parse_expr s2 e2
    in s3 end

    fun postfix_rec S ((DataTypes.RD s)::L) = 
            postfix_rec (FunStack.pushAll ([(Variable s), RD], S)) L
      | postfix_rec S ((DataTypes.WR e)::L) = 
    let
        val s1 = FunStack.push (WR,S)
        val s2 = parse_expr S e
    in
        s2
    end
      | postfix_rec S ((DataTypes.SETINT (s,e))::L) = 
        
            

    fun postfix L 

    fun execute table model = model
end
