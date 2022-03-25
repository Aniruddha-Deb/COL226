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

    exception TypeError
    (* Typing presents a problem here: too many checks, ugh! *)
    datatype Value = Integer of int | Boolean of bool

    datatype Op = ADD | SUB | MUL | DIV | MOD | GT | GE | LT | LE | EQ | NE | AND | OR | NOT

    datatype Command = Literal of (int*Table.Type) | Variable of string | 
                     | Block of Command list | ITE | WH | SET | Operator of Op
                     | RD | WR

    fun int2bool 0 = false | int2bool n = true;
    fun bool2int false = 0 | bool2int true = 1;
    fun int_not 0 = 1 | int_not 1 = 0 | int_not n = 0

    type VMCModel = Value FunStack.Stack * int array * Command FunStack.Stack

    fun op_int operator op1 op2 =
        case operator of
              ADD => op1 + op2
            | SUB => op1 - op2 
            | MUL => op1 * op2 
            | DIV => op1 / op2
            | MOD => op1 % op2
            | LT  => op1 < op2
            | GT  => op1 > op2
            | GE  => op1 >= op2
            | LE  => op1 <= op2
            | EQ  => op1 = op2
            | NE  => op1 <> op2
            | a => raise TypeError

    fun rules table (V,M,(Literal (m,T))::C) = 
            if T = Table.INT then ((Integer m)::V,M,C)
            else ((Boolean (int2bool m))::V,M,C)
      | rules table (V,M,(Variable x)::C) = 
        let
            val (pos,dtype) = Table.lookup x table
        in
            if dtype = Table.INT then ((Integer (Array.sub (M,pos)))::V,M,C)
            else ((Boolean (int2bool (Array.sub (M,pos))))::V,M,C)
        end
      | rules table ((Literal (op1,Table.BOOL))::V,M,(Operator NOT)::C) =
            ((Literal ((int_not op1),Table.BOOL))::V,M,C)
      | rules table ((Literal (op1,Table.BOOL))::(Literal (op2,Table.BOOL))::V,M,(Operator AND)::C) =
        let
            val op1_bool = int2bool op1
            val op2_bool = int2bool op2
        in
            ((Literal ((bool2int (op1_bool and op2_bool)),Table.BOOL))::V,M,C)
        end
      | rules table ((Literal (op1,Table.BOOL))::(Literal (op2,Table.BOOL))::V,M,(Operator OR)::C) =
        let
            val op1_bool = int2bool op1
            val op2_bool = int2bool op2
        in
            ((Literal ((bool2int (op1_bool or op2_bool)),Table.BOOL))::V,M,C)
        end
      | rules table ((Literal (op1,T))::(Literal (op2,T))::V,M,(Operator a)::C) =
        let
            val int_ans = op_int a op1 op2
        in
            if a = EQ or a = NE or a = GT or a = GE or a = LT or a = LE then
                ((Literal ((int2bool int_ans),Table.BOOL))::V,M,C)
            else
                ((Literal (int_ans,Table.INT))::V,M,C)
        end
      | rules table ((Literal (op1,T))::(Variable op2)::V,M,SET::C) =
        let 
            val (pos,dtype) = Table.lookup op2 table
        in
            if dtype <> T then raise TypeError
            else (Array.update (M,pos,op1);(V,M,C))
        end
      (* TODO ite and wh and then debug *)

    fun toString model = "Not implemented yet"

    fun parse_expr S (DataTypes.NUM n) = FunStack.push ((Literal (n,Table.INT)),S)
      | parse_expr S (DataTypes.IREF s) = FunStack.push ((Variable s),S)
      | parse_expr S (DataTypes.BREF s) = FunStack.push ((Variable s),S)
      | parse_expr S (DataTypes.TRUE) = FunStack.push ((Literal (1,Table.BOOL)), S)
      | parse_expr S (DataTypes.FALSE) = FunStack.push ((Literal (0,Table.BOOL)), S)
      | parse_expr S (DataTypes.NOT e) = 
    let
        val s1 = FunStack.push ((Operator NOT),S)
        val s2 = parse_expr s1 e
    in s2 end
      | parse_expr S E =
    let
        val (s1,e1,e2) = case E of 
            (DataTypes.ADD (e1,e2)) => ((FunStack.push ((Operator ADD),S)),e1,e2)
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

    fun parse_block S CL =
    let
        val S' = FunStack.stack2list (postfix CL)
        val B = Block S'
    in FunStack.push (B,S) end

    and postfix_rec S ((DataTypes.RD s)::L) = 
            postfix_rec (FunStack.pushAll ([(Variable s), RD], S)) L
      | postfix_rec S ((DataTypes.WR e)::L) = 
    let
        val s1 = FunStack.push (WR,S)
        val s2 = parse_expr s1 e
    in
        postfix_rec s2 L
    end
      | postfix_rec S ((DataTypes.SETINT (s,e))::L) = 
    let
        val s1 = FunStack.push (SET,S)
        val s2 = parse_expr s1 e
        val s3 = FunStack.push ((Variable s),s2)
    in
        postfix_rec s3 L
    end
      | postfix_rec S ((DataTypes.SETBOOL (s,e))::L) = 
    let
        val s1 = FunStack.push (SET,S)
        val s2 = parse_expr s1 e
        val s3 = FunStack.push ((Variable s),s2)
    in
        postfix_rec s3 L
    end
      | postfix_rec S ((DataTypes.WH (e,CL))::L) = 
    let
        val s1 = FunStack.push (WH,S)
        val s2 = parse_block s1 CL
        val s3 = parse_expr s2 e
    in
        postfix_rec s3 L
    end
      | postfix_rec S ((DataTypes.ITE (e,CL1,CL2))::L) =
    let
        val s1 = FunStack.push(ITE,S)
        val s2 = parse_block s1 CL2
        val s3 = parse_block s2 CL1
        val s4 = parse_expr s3 e
    in
        postfix_rec s4 L
    end
      | postfix_rec S [] = S (* yay *)
            
    and postfix L = postfix_rec (FunStack.create ()) L

    fun execute table model = model
end
