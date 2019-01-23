// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class IfStatement extends Statement {
    private Statement ifStat;
    private Statement elseStat;
    
    public Statement getIfStat() {
        return ifStat;
    }

    public void setIfStat(Statement ifStat) {
        this.ifStat = ifStat;
    }

    public Statement getElseStat() {
        return elseStat;
    }

    public void setElseStat(Statement elseStat) {
        this.elseStat = elseStat;
    }
    
    
}
