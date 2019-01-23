// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

import java.util.ArrayList;

public class Method extends Member {
    private ArrayList<Param> paramList;
    private ArrayList<Statement> statList;
    private boolean returnDefined = false;

    public Method(Qualifier qualifier, Type type, String identifier) {
        super(qualifier, type, identifier);
    }
    
    public ArrayList<Param> getParamList() {
        return paramList;
    }

    public void setParamList(ArrayList<Param> paramList) {
        this.paramList = paramList;
    }

    public ArrayList<Statement> getStatList() {
        return statList;
    }

    public void setStatList(ArrayList<Statement> statList) {
        this.statList = statList;
    }

    public boolean isReturnDefined() {
        return returnDefined;
    }

    public void setReturnDefined(boolean returnDefined) {
        this.returnDefined = returnDefined;
    }    
}
