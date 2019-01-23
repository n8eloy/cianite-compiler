/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ast;

/**
 *
 * @author natha
 */
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
