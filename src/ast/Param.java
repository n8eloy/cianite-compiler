// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class Param {
    private Type type;
    private String identifier;

    public Param(Type type, String identifier) {
        this.type = type;
        this.identifier = identifier;
    }

    public String getIdentifier() {
        return identifier;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }
    
    
}
