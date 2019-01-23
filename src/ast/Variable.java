// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class Variable extends Member {
    public Variable(Type type, String identifier) {
        super(null, type, identifier);
    }
}
