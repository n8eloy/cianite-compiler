// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class NullExpr extends Expr {
    
   public void genC( PW pw, boolean putParenthesis ) {
      pw.printIdent("NULL");
   }
   
}