(* compiler.sml *)
signature WHILE =
sig
    val compile : string -> DataTypes.PROG
    val createSymbolTable : DataTypes.DEC list -> Table.SymbolTable
    val allocate : Table.SymbolTable -> int array
    val getTable : string -> Table.SymbolTable
    val createCommandStack : string -> Vmc.Command FunStack.Stack
end

structure While : WHILE =
struct
    exception WhileError;

    fun compile (fileName) =
    let 
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"
                      ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = WhileParser.parse
                    (15,
                    (WhileParser.makeLexer grab fileName),
                    printError,
                    fileName)
            handle WhileParser.ParseError => raise WhileError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in 
        tree
    end

    fun createSymbolTableRec table idx ((DataTypes.INT s)::L) = 
        createSymbolTableRec (Table.update table s (idx,Table.INT)) (idx+1) L
      | createSymbolTableRec table idx ((DataTypes.BOOL s)::L) = 
        createSymbolTableRec (Table.update table s (idx,Table.BOOL)) (idx+1) L
      | createSymbolTableRec table _ [] = table

    fun createSymbolTable ((DataTypes.INT s)::L) = 
        createSymbolTableRec (Table.update (Table.create ()) s (0,Table.INT)) (1) L
      | createSymbolTable ((DataTypes.BOOL s)::L) = 
        createSymbolTableRec (Table.update (Table.create ()) s (0,Table.BOOL)) (1) L

    fun allocate table = Array.array ((Table.size table),0)

    fun getTable filename = 
    let
        val (DataTypes.PROG (dcl,cml)) = compile filename
        val table = createSymbolTable dcl
    in
        table
    end

    fun createCommandStack filename = 
    let
        val (DataTypes.PROG (dcl, cml)) = compile filename
        val stack = Vmc.postfix (List.rev cml)
    in
        stack
    end

end;
