// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class Param extends Member {
    public Param(Type type, String identifier) {
        super(null, type, identifier);
    }
}
