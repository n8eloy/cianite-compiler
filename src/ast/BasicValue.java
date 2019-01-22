// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class BasicValue {
    private final Type type;
    private int intValue;
    private boolean boolValue;
    private String stringValue;

    public BasicValue(int intValue) {
        this.intValue = intValue;
        type = new TypeInt();
    }

    public BasicValue(boolean boolValue) {
        this.boolValue = boolValue;
        type = new TypeBoolean();
    }

    public BasicValue(String stringValue) {
        this.stringValue = stringValue;
        type = new TypeString();
    }

    public Type getType() {
        return type;
    }
    
    public String getStringValue() {
        return stringValue;
    }

    public int getIntValue() {
        return intValue;
    }

    public boolean isBoolValue() {
        return boolValue;
    }
}
