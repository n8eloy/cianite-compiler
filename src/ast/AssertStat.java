// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class AssertStat extends Statement {
    private Expr expression;
    private String stringValue;

    public AssertStat(Expr expression, String stringValue) {
        this.expression = expression;
        this.stringValue = stringValue;
    }
    
    public Expr getExpression() {
        return expression;
    }

    public void setExpression(Expr expression) {
        this.expression = expression;
    }
    
    public String getStringValue() {
        return stringValue;
    }
    
    public void setStringValue(String stringValue) {
        this.stringValue = stringValue;
    }
}
