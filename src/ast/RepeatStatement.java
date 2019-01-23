// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

import java.util.ArrayList;

public class RepeatStatement extends Statement {
    private ArrayList<Statement> statementList = new ArrayList<>();

    public RepeatStatement(ArrayList<Statement> statement) {
        this.statementList = statement;
    }
}
