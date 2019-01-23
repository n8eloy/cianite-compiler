// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public enum Qualifier {
    DE("default"), // Same as public, used for override control
    PR("private"),
    PU("public"),
    OV("override"),
    OVPU("override public"),
    FI("final"),
    FIPU("final public"),
    FIOV("final override"),
    FIOVPU("final override public");
    
    Qualifier(String name) {
        this.name = name;
    }
    
    private String name;
}
