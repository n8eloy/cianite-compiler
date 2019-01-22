// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class AssignExpr {
    private Expr leftExpression;
    private Expr rightExpression;

    public AssignExpr(Expr leftExpression) {
        this.leftExpression = leftExpression;
        this.rightExpression = null;
    }
    
    public AssignExpr(Expr leftExpression, Expr rightExpression) {
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
    }

    public Expr getRightExpression() {
        return rightExpression;
    }

    public void setRightExpression(Expr rightExpression) {
        this.rightExpression = rightExpression;
    }

    public Expr getLeftExpression() {
        return leftExpression;
    }

    public void setLeftExpression(Expr leftExpression) {
        this.leftExpression = leftExpression;
    }
    
    
}
