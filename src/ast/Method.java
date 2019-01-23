// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

import java.util.ArrayList;

public class Method extends Member {
    private ArrayList<Param> paramList;
    private ArrayList<Statement> statList;

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
    
    
}
