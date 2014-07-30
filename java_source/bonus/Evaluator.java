import java.math.BigInteger;

public class Evaluator {

    private static final char EOF = 4711;

    public class ParseError extends RuntimeException {
	public ParseError(int pos, String problem) {
	    super("parse error at column " + pos + ": " + problem + " (Program: " + p + ")");
	}
    }

    /** The program. */
    private String p;
    private int pos;
    private int len;

    /**
     * Due to the "at most one fold" restriction, there can be
     * at most 3 diffent identifiers in a program.
     */
    private static final int MAX_VARS = 3;

    private final String[] vars = new String[MAX_VARS];
    private final long[] values = new long[MAX_VARS];
    int numVars = 0;

    public void setProgram(String p) {
        this.p = p;
        pos = 0;
        len = p.length();
    }

    public long eval(long input) {
	pos = 0;
	numVars = 0;

	accept('(');
	accept("lambda");
	accept('(');
	String x0 = acceptId();
	bind(x0, input);
	accept(')');
	long value = evalExpr();
	accept(')');
	skipSpace();
	if (pos < len) {
	    throw new ParseError(pos, "trailing input");
	}
	return value;
    }

    private long evalExpr() {
	skipSpace();
	switch(peekChar()) {
	case '0':
	    nextChar();
	    return 0L;
	case '1':
	    nextChar();
	    return 1L;
	case '(':
	    return evalParens();
	default:
	    // must be an identifier
	    return lookup(acceptId());
	}	
    }

    private long evalParens() {
	long value;
	accept('(');
	skipSpace();
	switch (peekChar()) {
	case 'a':
	case 'o':
	case 'p':
	case 'x':
	    value = binop();
	    break;
	case 'n':
	case 's':
	    value = unop();
	    break;
	case 'i':
	    value = if0();
	    break;
	case 'f':
	    value = fold();
	    break;
	default:
	    throw new ParseError(pos, "operator expected");
	}
	accept(')');
	return value;
    }

    private long unop() {
	String op = acceptId();
	long value = evalExpr();
	if (op.equals("not"))
	    return ~value;
	if (op.equals("shl1"))
	    return value << 1;
	if (op.equals("shr1"))
	    return value >>> 1;
	if (op.equals("shr4"))
	    return value >>> 4;
	if (op.equals("shr16"))
	    return value >>> 16;
	throw new ParseError(pos, "unknow unary operator: " + op);
    }

    private long binop() {
	String op = acceptId();
	long x = evalExpr();
	long y = evalExpr();
	if (op.equals("and"))
	    return x & y;
	if (op.equals("or"))
	    return x | y;
	if (op.equals("xor"))
	    return x ^ y;
	if (op.equals("plus"))
	    return x + y;
	throw new ParseError(pos, "unknown binary operator: " + op);
    }

    private long if0() {
	accept("if0");
	long e1 = evalExpr();
	long e2 = evalExpr();
	long e3 = evalExpr();
	return (e1 == 0) ? e2 : e3;
    }

    private long fold() {
	accept("fold");
	long word = evalExpr();
	long value = evalExpr();
	accept('(');
	accept("lambda");
	accept('(');
	String x = acceptId();
	String y = acceptId();
	accept(')');

	skipSpace();
	int accStart = pos;
	bind(x, 0);
	bind(y, 0);
	for (int b = 0; b < 64; b += 8) {
	    long byteVal = (word >>> b) & 255;
	    reBind(x, byteVal);
	    reBind(y, value);

	    pos = accStart;
	    value = evalExpr();
	}
	accept(')');
	popVars(2);

	return value;
    }

    private char peekChar() {
        if (pos >= len)
	    return EOF;
	return p.charAt(pos);
    }

    private char nextChar() {
        if (pos >= len)
	    return EOF;
	return p.charAt(pos++);
    }

    private char peekAt(int offs) {
	if (pos + offs >= len)
	    return EOF;
	return p.charAt(pos + offs);
    }

    private void skipSpace() {
        for (;;) {
	    char c = peekChar();
	    if (!(c == ' ' || c == '\t')) {
		break;
	    }
	    pos++;
	}
    }        

    private void accept(char tok) {
	skipSpace();
	if (nextChar() != tok) {
	    throw new ParseError(pos, tok + " expected");
	}
    }

    private void accept(String tok) {
	skipSpace();
	if (p.startsWith(tok, pos)) {
	    pos += tok.length();
        } else {
	    throw new ParseError(pos, tok + " expected");
	}
    }

    private boolean tryTok(String tok) {
	return p.startsWith(tok, pos) && !isIdChar(peekAt(tok.length()));
    }

    private String acceptId() {
	skipSpace();
	int start = pos;
	char c = nextChar();
	if (!isIdStart(c)) {
	    throw new ParseError(pos, "identifier expected");
	}
	while (isIdChar(peekChar())) {
	    nextChar();
	}
	return p.substring(start, pos);
    }

    private boolean isIdStart(char c) {
	return c >= 'a' && c <= 'z';
    }

    private boolean isIdChar(char c) {
	return (c >= '0' && c <= '9') || (c == '_') || (c >= 'a' && c <= 'z');
    }

    private void bind(String var, long value) {
	vars[numVars] = var;
	values[numVars] = value;
	numVars++;
    }

    private void popVars(int n) {
	if (numVars < n) {
	    throw new ParseError(pos, "too much popping");
	}
	numVars -= n;
    }

    private int varIndex(String var) {
	for (int k = numVars - 1; k >= 0; k--) {
	    if (vars[k].equals(var)) {
		return k;
	    }
	}
	return -1;
    }

    private long lookup(String var) {
	int k = varIndex(var);
	if (k >= 0) {
	    return values[k];
	}
	throw new ParseError(pos, "unbound variable: " + var);
    }

    private void reBind(String var, long value) {
	int k = varIndex(var);
	if (k < 0) {
	    throw new ParseError(pos, "unbound variable");
	}
	values[k] = value;
    }

    // Main fuer Tests

    public static void main(String[] args) {
	if (args.length != 2) {
	    System.out.println("usage: Evaluator program value");
	    return;
	}
	String program = args[0];
	long input = parseHex(args[1]);

	Evaluator ev = new Evaluator();
	ev.setProgram(program);
	long value = ev.eval(input);
	System.out.println(showHex(value));
    }

    private static long parseHex(String s) {
	if (s.startsWith("0x")) {
	    s = s.substring(2);
	}
	return new BigInteger(s, 16).longValue();
    }

    private static String showHex(long x) {
	return "0x" + Long.toHexString(x);
    }

}
