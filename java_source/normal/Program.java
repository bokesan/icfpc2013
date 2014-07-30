
public class Program {

    private final String prog;
    
    Program(String prog) {
        this.prog = prog;
    }
    
    @Override
    public String toString() {
        return "\"" + prog + "\"";
    }
    
    public String toStringWithoutQuotes() {
        return prog;
    }
}
