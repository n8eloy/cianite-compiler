// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

import java.util.ArrayList;

/*
 * Krakatoa Class
 */
public class CianetoClass extends Type {
    private final boolean open;
    private CianetoClass superclass = null;
    private ArrayList<Field> publicFieldList = new ArrayList<>();
    private ArrayList<Field> privateFieldList = new ArrayList<>();;
    private ArrayList<Method> publicMethodList = new ArrayList<>();;
    private ArrayList<Method> privateMethodList = new ArrayList<>();;

    public CianetoClass(String name, boolean open) {
        super(name);
        this.open = open;
    }
    
    public boolean isOpen() {
        return open;
    }
    
    /*
        Searches for a method in all hierarchy from inside class
    */
    public Method findInMethod(String id) {
        if(privateMethodList != null && !privateMethodList.isEmpty()) {
            for (Method m : privateMethodList) {
                if(m.getIdentifier().equals(id)) {
                    return m;
                }
            }
        }
        
        if(publicMethodList != null && !publicMethodList.isEmpty()) {
            for (Method m : publicMethodList) {
                if(m.getIdentifier().equals(id)) {
                    return m;
                }
            }
        }
        
        if(this.superclass != null) {
            return superclass.findOutMethod(id);
        }
        
        return null;
    }
    
    /*
        Searches for a method in all hierarchy from outside class
    */
    public Method findOutMethod(String id) {
        if(publicMethodList != null && !publicMethodList.isEmpty()) {
            for (Method m : publicMethodList) {
                if(m.getIdentifier().equals(id)) {
                    return m;
                }
            }
        }
        
        if(this.superclass != null) {
            return superclass.findOutMethod(id);
        }
        
        return null;
    }
    
    /*
        Return public method for a certain identifier or null if none is found
    */
    public Method findPublicMethod(String id) {
        if(publicMethodList != null && !publicMethodList.isEmpty()) {
            for (Method m : publicMethodList) {
                if(m.getIdentifier().equals(id)) {
                    return m;
                }
            }
        }
        return null;
    }
    
    /*
        Return public field for a certain identifier or null if none is found
    */
    public Field findPublicField(String id) {
        if(publicFieldList != null && !publicFieldList.isEmpty()) {
            for (Field f : publicFieldList) {
                if(f.getIdentifier().equals(id)) {
                    return f;
                }
            }
        }
        return null;
    }
    
    /*
        Return private field for a certain identifier or null if none is found
    */
    public Field findPrivateField(String id) {
        if(privateFieldList != null && !(privateFieldList.isEmpty())) {
            for (Field f : privateFieldList) {
                if(f.getIdentifier().equals(id)) {
                    return f;
                }
            }
        }
        
        if(publicFieldList != null && !(publicFieldList.isEmpty())) {
            for (Field f : publicFieldList) {
                if(f.getIdentifier().equals(id)) {
                    return f;
                }
            }
        }
        
        return null;
    }

    public CianetoClass getSuperclass() {
        return superclass;
    }
    
    public void setSuperclass(CianetoClass superclass) {
        this.superclass = superclass;
    }

    public ArrayList<Field> getPublicFieldList() {
        return publicFieldList;
    }   
    
    public ArrayList<Field> getPrivateFieldList() {
        return privateFieldList;
    }  

    public ArrayList<Method> getPublicMethodList() {
        return publicMethodList;
    }

    public ArrayList<Method> getPrivateMethodList() {
        return privateMethodList;
    }
        
    public void setPublicFieldList(ArrayList<Field> fieldList) {
        this.publicFieldList = fieldList;
    }
    
    public void setPrivateFieldList(ArrayList<Field> fieldList) {
        this.privateFieldList = fieldList;
    }
    
    public void setPublicMethodList(ArrayList<Method> publicMethodList) {
        this.publicMethodList = publicMethodList;
    }

    public void setPrivateMethodList(ArrayList<Method> privateMethodList) {
        this.privateMethodList = privateMethodList;
    }
    
    public void addPublicField(Field field) {
        this.publicFieldList.add(field);
    }
    
    public void addPrivateField(Field field) {
        this.privateFieldList.add(field);
    }
    
    public void addPublicMethod(Method method) {
        this.publicMethodList.add(method);
    }

    public void addPrivateMethod(Method method) {
        this.privateMethodList.add(method);
    }
}
