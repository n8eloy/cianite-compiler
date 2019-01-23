// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

import java.util.ArrayList;

public class WhileStatement extends Statement {
    private ArrayList<Statement> statementList = new ArrayList<>();

    public void addStatement(Statement statement) {
        this.statementList.add(statement);
    }
}
