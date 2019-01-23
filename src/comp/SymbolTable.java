// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package comp;
import java.util.Hashtable;

public class SymbolTable {
    private Hashtable globalScopeTable, classScopeTable, localScopeTable;
    
    public SymbolTable() {
        globalScopeTable = new Hashtable(); // Classes
        classScopeTable = new Hashtable(); // Fields and methods
        localScopeTable = new Hashtable(); // Local declarations
    }
    
    public Object putInGlobal( String key, Object value ) {
       return globalScopeTable.put(key, value);
    }
    
    public Object putInClass( String key, Object value ) {
       return classScopeTable.put(key, value);
    }
    
    public Object putInLocal( String key, Object value ) {
       return localScopeTable.put(key, value);
    }
    
    public Object getInGlobal( String key ) {
       return globalScopeTable.get(key);
    }
    
    public Object getInClass( String key ) {
       return classScopeTable.get(key);
    }
    
    public Object getInLocal( String key ) {
       return localScopeTable.get(key);
    }
    
    public Object getDownTop( String key ) {
        Object result;
        if ( (result = localScopeTable.get(key)) != null ) {
            return result;
        } else if ( (result = classScopeTable.get(key)) != null ) {
            return result;
        } else {
            return globalScopeTable.get(key);
        }
    }
    
    public void eraseClass() {
        classScopeTable.clear();
    }

    public void eraseLocal() {
        localScopeTable.clear();
    }
}
