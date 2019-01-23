// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class Field extends Member {
    
    public Field(Qualifier qualifier, Type type, String identifier) {
        super(qualifier, type, identifier);
    }
}
