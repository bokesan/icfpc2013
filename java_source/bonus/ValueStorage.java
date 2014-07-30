import java.util.ArrayList;
import java.util.List;


public class ValueStorage {

    private final Integer VALUE_BLOCK_SIZE = 256;
    private List<Value> values = new ArrayList<Value>();
    
    public Value getNextValue() {
        if (values.size() == 0) {
            values = ServerConnector.getNewValues(VALUE_BLOCK_SIZE);
        }
        Value result = values.get(0);
        values.remove(0);
        return result;
    }
    
    public void setNextValue(Value val) {
        values.add(0, val);
        System.out.println("Neuer Testwert aus Mismatch gesetzt.");
    }
}
