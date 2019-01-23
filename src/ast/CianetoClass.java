// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

import java.util.ArrayList;

/*
 * Krakatoa Class
 */
public class CianetoClass extends Type {
    private final boolean open;
    private CianetoClass superclass = null;
    private ArrayList<Field> publicFieldList, privateFieldList;
    private ArrayList<Method> publicMethodList, privateMethodList;

    public CianetoClass(String name, boolean open) {
        super(name);
        this.open = open;
    }
    
    public boolean isOpen() {
        return open;
    }
    
    /*
        Return public method for a certain identifier or null if none is found
    */
    public Method findPublicMethod(String id) {
        for (Method m : publicMethodList) {
            if(m.getIdentifier().equals(id)) {
                return m;
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
