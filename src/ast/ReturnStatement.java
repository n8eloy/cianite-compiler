// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class ReturnStatement extends Statement {
    private Type returnType;

    public ReturnStatement(Type returnType) {
        this.returnType = returnType;
    }
}
