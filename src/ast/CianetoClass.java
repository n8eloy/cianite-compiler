// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;
/*
 * Krakatoa Class
 */
public class CianetoClass extends Type {

   public CianetoClass( String name ) {
      super(name);
   }

   @Override
   public String getCname() {
      return getName();
   }

   private String name;
   private CianetoClass superclass;
   // private FieldList fieldList;
   // private MethodList publicMethodList, privateMethodList;
   // m�todos p�blicos get e set para obter e iniciar as vari�veis acima,
   // entre outros m�todos
}
