val instream = TextIO.openIn "test_prog.while";
val lexer = WhileLex.makeLexer(fn _ => TextIO.input instream);

fun toklist L = 
let
    val (lnum,tok) = lexer()
in
    if tok = WhileLex.UserDeclarations.EOF then (rev L) else toklist ((lnum,tok)::L)
end;
