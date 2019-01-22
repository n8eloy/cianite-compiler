// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package comp;
import java.util.Hashtable;

public class SymbolTable {
    private Hashtable globalTable, localTable;
    
    public SymbolTable() {
        globalTable = new Hashtable();
        localTable = new Hashtable();
    }
}
