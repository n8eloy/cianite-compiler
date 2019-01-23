// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

abstract public class Member {
    private final Qualifier qualifier;
    private Type type;
    private final String identifier;
    
    public Member(Qualifier qualifier, Type type, String identifier) {
        this.qualifier = qualifier;
        this.type = type;
        this.identifier = identifier;
    }

    public Qualifier getQualifier() {
        return qualifier;
    }
    
    public Type getType() {
        return type;
    }
    
    public String getIdentifier() {
        return identifier;
    }
    
    public void setType(Type type) {
        this.type = type;
    }
}
