// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package ast;

public class TypeNull extends Type {

	public TypeNull() {
		super("NullType");
	}

	@Override
	public String getCname() {
		return "NULL";
	}

}
