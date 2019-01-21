// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package comp;

import java.io.PrintWriter;
import java.util.ArrayList;
import ast.CianetoClass;
import ast.LiteralInt;
import ast.MetaobjectAnnotation;
import ast.Program;
import ast.Statement;
import lexer.Lexer;
import lexer.Token;

public class Compiler {

	// compile must receive an input with an character less than
	// p_input.lenght
	public Program compile(char[] input, PrintWriter outError) {

		ArrayList<CompilationError> compilationErrorList = new ArrayList<>();
		signalError = new ErrorSignaller(outError, compilationErrorList);
		symbolTable = new SymbolTable();
		lexer = new Lexer(input, signalError);
		signalError.setLexer(lexer);

		Program program = null;
		lexer.nextToken();
		program = program(compilationErrorList);
		return program;
	}

	private Program program(ArrayList<CompilationError> compilationErrorList) {
		// Program ::= CianetoClass { CianetoClass }
		ArrayList<MetaobjectAnnotation> metaobjectCallList = new ArrayList<>();
		ArrayList<CianetoClass> CianetoClassList = new ArrayList<>();
		Program program = new Program(CianetoClassList, metaobjectCallList, compilationErrorList);
		boolean thereWasAnError = false;
		while ( lexer.token == Token.CLASS ||
				(lexer.token == Token.ID && lexer.getStringValue().equals("open") ) ||
				lexer.token == Token.ANNOT ) {
			try {
				while ( lexer.token == Token.ANNOT ) {
					metaobjectAnnotation(metaobjectCallList);
				}
				classDec();
			}
			catch( CompilerError e) {
				// if there was an exception, there is a compilation error
				thereWasAnError = true;
				while ( lexer.token != Token.CLASS && lexer.token != Token.EOF ) {
					try {
						next();
					}
					catch ( RuntimeException ee ) {
						e.printStackTrace();
						return program;
					}
				}
			}
			catch ( RuntimeException e ) {
				e.printStackTrace();
				thereWasAnError = true;
			}

		}
		if ( !thereWasAnError && lexer.token != Token.EOF ) {
			try {
				error("End of file expected");
			}
			catch( CompilerError e) {
			}
		}
		return program;
	}

	/**  parses a metaobject annotation as <code>{@literal @}cep(...)</code> in <br>
     * <code>
     * @cep(5, "'class' expected") <br>
     * class Program <br>
     *     func run { } <br>
     * end <br>
     * </code>
     *

	 */
	@SuppressWarnings("incomplete-switch")
	private void metaobjectAnnotation(ArrayList<MetaobjectAnnotation> metaobjectAnnotationList) {
		String name = lexer.getMetaobjectName();
		int lineNumber = lexer.getLineNumber();
		lexer.nextToken();
		ArrayList<Object> metaobjectParamList = new ArrayList<>();
		boolean getNextToken = false;
		if ( lexer.token == Token.LEFTPAR ) {
			// metaobject call with parameters
			lexer.nextToken();
			while ( lexer.token == Token.LITERALINT || lexer.token == Token.LITERALSTRING ||
					lexer.token == Token.ID ) {
				switch ( lexer.token ) {
				case LITERALINT:
					metaobjectParamList.add(lexer.getNumberValue());
					break;
				case LITERALSTRING:
					metaobjectParamList.add(lexer.getLiteralStringValue());
					break;
				case ID:
					metaobjectParamList.add(lexer.getStringValue());
				}
				lexer.nextToken();
				if ( lexer.token == Token.COMMA )
					lexer.nextToken();
				else
					break;
			}
			if ( lexer.token != Token.RIGHTPAR )
				error("')' expected after metaobject call with parameters");
			else {
				getNextToken = true;
			}
		}
		if ( name.equals("nce") ) {
			if ( metaobjectParamList.size() != 0 )
				error("Metaobject 'nce' does not take parameters");
		}
		else if ( name.equals("cep") ) {
			if ( metaobjectParamList.size() != 3 && metaobjectParamList.size() != 4 )
				error("Metaobject 'cep' take three or four parameters");
			if ( !( metaobjectParamList.get(0) instanceof Integer)  ) {
				error("The first parameter of metaobject 'cep' should be an integer number");
			}
			else {
				int ln = (Integer ) metaobjectParamList.get(0);
				metaobjectParamList.set(0, ln + lineNumber);
			}
			if ( !( metaobjectParamList.get(1) instanceof String) ||  !( metaobjectParamList.get(2) instanceof String) )
				error("The second and third parameters of metaobject 'cep' should be literal strings");
			if ( metaobjectParamList.size() >= 4 && !( metaobjectParamList.get(3) instanceof String) )
				error("The fourth parameter of metaobject 'cep' should be a literal string");

		}
		metaobjectAnnotationList.add(new MetaobjectAnnotation(name, metaobjectParamList));
		if ( getNextToken ) lexer.nextToken();
	}

	private void classDec() {
		if ( lexer.token == Token.ID && lexer.getStringValue().equals("open") ) {
                    next();
			// open class
		}
		if ( lexer.token != Token.CLASS ) error("'class' expected");
		lexer.nextToken();
		if ( lexer.token != Token.ID )
			error("Identifier expected");
		String className = lexer.getStringValue();
		lexer.nextToken();
		if ( lexer.token == Token.EXTENDS ) {
			lexer.nextToken();
			if ( lexer.token != Token.ID )
				error("Identifier expected");
			String superclassName = lexer.getStringValue();

			lexer.nextToken();
		}

		memberList();
		if ( lexer.token != Token.END)
			error("'end' expected");
		lexer.nextToken();

	}

	private void memberList() {
		while ( true ) {
			qualifier();
			if ( lexer.token == Token.VAR ) {
				member();
			}
			else if ( lexer.token == Token.FUNC ) {
				member();
			}
			else {
				break;
			}
		}
	}

	private void error(String msg) {
		this.signalError.showError(msg);
	}
        
        private void error(String msg, boolean last) {
		this.signalError.showError(msg, last);
	}

	private void next() {
		lexer.nextToken();
	}

	private void check(Token shouldBe, String msg) {
		if ( lexer.token != shouldBe ) {
			error(msg);
		}
	}
        
	private void check(Token shouldBe, String msg, boolean last) {
		if ( lexer.token != shouldBe ) {
                    if (last) {
                        error(msg, true);
                    } else {
			error(msg);
                    }
		}
	}

	private void methodDec() {
		lexer.nextToken();
		if ( lexer.token == Token.ID ) {
			// unary method
			lexer.nextToken();

		}
		else if ( lexer.token == Token.IDCOLON ) {
			// keyword method. It has parameters
                        next();
                        formalParamDec();
		}
		else {
			error("An identifier or identifer: was expected after 'func'");
		}
		if ( lexer.token == Token.MINUS_GT ) {
			// method declared a return type
			lexer.nextToken();
			type();
		}
		if ( lexer.token != Token.LEFTCURBRACKET ) {
			error("'{' expected");
		}
		next();
		statementList();
		if ( lexer.token != Token.RIGHTCURBRACKET ) {
			error("'}' expected");
		}
		next();
	}

	private void statementList() {
		  // only '}', end and 'until' is necessary in this test
		while ( !checkPass(Token.RIGHTCURBRACKET, Token.END, Token.UNTIL)) {
			statement();
		}
	}

	private void statement() {
		boolean checkSemiColon = true;
		switch ( lexer.token ) {
		case IF:
			ifStat();
			checkSemiColon = false;
			break;
		case WHILE:
			whileStat();
			checkSemiColon = false;
			break;
		case RETURN:
			returnStat();
			break;
		case BREAK:
			breakStat();
			break;
                case SEMICOLON:
                        break;
		case REPEAT:
			repeatStat();
			break;
		case VAR:
			localDec();
			break;
		case ASSERT:
			assertStat();
			break;
		default:
			if ( lexer.token == Token.ID && lexer.getStringValue().equals("Out") ) {
				writeStat();
			}
                        else if (checkPass(Token.PLUS, Token.MINUS, Token.INT, 
                                Token.BOOLEAN, Token.STRING, Token.LEFTPAR, 
                                Token.NOT, Token.NULL, Token.ID, Token.SUPER,
                                Token.SELF)) {
				assignExpr();
			} else {
                            error("Statement expected");
                        }

		}
		if ( checkSemiColon ) {
			check(Token.SEMICOLON, "';' expected", true);
                        next();
		}
	}

	private void localDec() {
		next();
		type();
		
                idList();
                
		if ( lexer.token == Token.ASSIGN ) {
			next();
			// check if there is just one variable
			expr();
		}

	}

	private void repeatStat() {
		next();
                //while ( lexer.token != Token.UNTIL && lexer.token != Token.RIGHTCURBRACKET && lexer.token != Token.END ) {
                //        statement();
                //}
                statementList();
		check(Token.UNTIL, "'until' was expected");
                next();
                expr();
	}

	private void breakStat() {
		next();

	}

	private void returnStat() {
		next();
		expr();
	}

	private void whileStat() {
		next();
		expr();
		check(Token.LEFTCURBRACKET, "'{' expected after the 'while' expression");
		next();
		while ( lexer.token != Token.RIGHTCURBRACKET && lexer.token != Token.END ) {
			statement();
		}
		check(Token.RIGHTCURBRACKET, "'}' expected");
                next();
	}

	private void ifStat() {
		next();
		expr();
		check(Token.LEFTCURBRACKET, "'{' expected after the 'if' expression");
		next();
		while ( lexer.token != Token.RIGHTCURBRACKET && lexer.token != Token.END && lexer.token != Token.ELSE ) {
			statement();
		}
		check(Token.RIGHTCURBRACKET, "'}' expected");
                next();
		if ( lexer.token == Token.ELSE ) {
			next();
			check(Token.LEFTCURBRACKET, "'{' expected after 'else'");
			next();
			while ( lexer.token != Token.RIGHTCURBRACKET ) {
				statement();
			}
			check(Token.RIGHTCURBRACKET, "'}' expected");
                        next();
		}
	}

	/**

	 */
	private void writeStat() {
		next();
		check(Token.DOT, "'.' expected after 'Out'");
		next();
		check(Token.IDCOLON, "'print:' or 'println:' expected after 'Out.'");
		String printName = lexer.getStringValue();
                next();
		expr();
	}

	private void fieldDec() {
		lexer.nextToken();
		type();
                
                idList();
                                
                check(Token.SEMICOLON, "';' expected", true);
                next();
	}

	private void type() {
		if ( lexer.token == Token.INT || lexer.token == Token.BOOLEAN || lexer.token == Token.STRING ) {
			next();
		}
		else if ( lexer.token == Token.ID ) {
			next();
		}
		else {
			this.error("A type was expected");
		}

	}

	private void qualifier() {
		if ( lexer.token == Token.PRIVATE ) {
			next();
		}
		else if ( lexer.token == Token.PUBLIC ) {
			next();
		}
		else if ( lexer.token == Token.OVERRIDE ) {
			next();
			if ( lexer.token == Token.PUBLIC ) {
				next();
			}
		}
		else if ( lexer.token == Token.FINAL ) {
			next();
			if ( lexer.token == Token.PUBLIC ) {
				next();
			}
			else if ( lexer.token == Token.OVERRIDE ) {
				next();
				if ( lexer.token == Token.PUBLIC ) {
					next();
				}
			}
		}
	}
	/**
	 * change this method to 'private'.
	 * uncomment it
	 * implement the methods it calls
	 */
	private Statement assertStat() {

		lexer.nextToken();
		//int lineNumber = lexer.getLineNumber();
		expr();
		if ( lexer.token != Token.COMMA ) {
			this.error("',' expected after the expression of the 'assert' statement");
		}
		lexer.nextToken();
		if ( lexer.token != Token.LITERALSTRING ) {
			this.error("A literal string expected after the ',' of the 'assert' statement");
		}
		//String message = lexer.getLiteralStringValue();
		lexer.nextToken();
		//if ( lexer.token == Token.SEMICOLON )
		//	lexer.nextToken();

		return null;
	}

        //private LiteralInt literalInt() {
        //
        //        LiteralInt e = null;
        //
        //        // the number value is stored in lexer.getToken().value as an object of
        //        // Integer.
        //        // Method intValue returns that value as an value of type int.
        //        int value = lexer.getNumberValue();
        //        lexer.nextToken();
        //        return new LiteralInt(value);
        //}

        //private static boolean startExpr(Token token) {
        //
        //        return token == Token.FALSE || token == Token.TRUE
        //                        || token == Token.NOT || token == Token.SELF
        //                        || token == Token.LITERALINT || token == Token.SUPER
        //                        || token == Token.LEFTPAR || token == Token.NULL
        //                        || token == Token.ID || token == Token.LITERALSTRING;
        //
        //}
        
        // --------------------------------------------------- //
        
        // Returns true if token == any from shouldBeArray
        private boolean checkPass(Token ... shouldBeArray) {
            for (Token shouldBe:shouldBeArray) {
                if (lexer.token == shouldBe)
                    return true;
            }
            return false;
        }
        
        // AssignExpr ::= Expression [ “=” Expression ]
        private void assignExpr() {
            expr();
            
            if(checkPass(Token.ASSIGN)) {
                next();
                expr();
            }
        }
        
        // CompStatement ::= “{” { Statement } “}”
        //private void compStatement() {
        //    check(Token.LEFTCURBRACKET,"'{' expected");
        //    next();
        //
        //    while(checkPass(Token.IF, Token.WHILE, Token.RETURN, Token.BREAK, 
        //            Token.SEMICOLON, Token.REPEAT, Token.VAR, Token.ASSERT) ||
        //            (checkPass(Token.ID) && lexer.getStringValue().equals("Out"))){
        //        statement();
        //    }
        //
        //    check(Token.RIGHTCURBRACKET,"'}' expected");
        //    next();
        //}
        
        // Expression ::= SimpleExpression [ Relation SimpleExpression ]
        private void expr() {
            simpleExpr();
            
            if(checkPass(Token.EQ, Token.LT, Token.GT, Token.LE, Token.GE, Token.NEQ)) {
                next();
                simpleExpr();
            }
        }
        
        // ExpressionList ::= Expression { “,” Expression }
        private void exprList() {
            expr();
            
            while(checkPass(Token.COMMA)) {
                next();
                expr();
            }
        }
        
        // Factor ::= BasicValue | “(” Expression “)” | “!” Factor | “nil” 
        // | ObjectCreation | PrimaryExpr
        // ObjectCreation syntatic analysis in primaryExpr() to solve ambiguity
        private void factor() {
            if(checkPass(Token.LEFTPAR)) {
                next();
                expr();
                check(Token.RIGHTPAR, "')' expected");
                next();
            } else if(checkPass(Token.NOT)) {
                next();
                factor();
            } else if(checkPass(Token.NULL)) {
                next();
            } else if(checkPass(Token.ID, Token.SELF, Token.SUPER)){
                primaryExpr();
            } else if(checkPass(Token.LITERALINT, Token.TRUE, Token.FALSE, Token.LITERALSTRING)) {
                next();
            } else {
                error("Expression expected");
                //error("Factor (Literal int, boolean, literal string, identifier, '(', '!', 'nil', 'self' or 'super') expected");
            }
        }
        
        // FormalParamDec ::= ParamDec { “,” ParamDec }
        private void formalParamDec() {
            paramDec();
            
            while(checkPass(Token.COMMA)) {
                next();
                paramDec();
            }
        }
        
        // IdList ::= Id { “,” Id }
        private void idList() {
            check(Token.ID, "Identifier expected");
            next();
            
            while(checkPass(Token.COMMA)) {
                next();
                check(Token.ID, "Identifier expected");
                next();
            }
        }
        
        // Member ::= FieldDec | MethodDec
        private void member() {
            if(checkPass(Token.VAR)) {
                fieldDec();
            } else if(checkPass(Token.FUNC)) {
                methodDec();
            } else {
                error("'var' or 'func' expected");
            }
        }
        
        // ObjectCreation ::= Id “.” “new”
        // ObjectCreation syntatic analysis in primaryExpr() to solve ambiguity
        private void objectCreation() {
            //check(Token.ID, "Identifier expected");
            //next();
            //check(Token.DOT, "'.' expected");
            //next();
            check(Token.NEW, "'new' expected");
            next();
        }
        
        // ParamDec ::= Type Id
        private void paramDec() {
            type();
            check(Token.ID, "Identifier expected");
            next();
        }
        
        // ReadExpr ::= “In” “.” ( “readInt” | “readString” )
        // ReadExpr syntatic analysis in primaryExpr() to solve ambiguity
        private void readExpr() {
            //check(Token.ID, "Identifier expected");
            //if(lexer.getStringValue().equals("In")) {
            //    next();
                
            //    check(Token.DOT, "'.' expected");
            //    next();
                
                check(Token.ID, "Identifier expected");
                if(lexer.getStringValue().equals("readInt") || lexer.getStringValue().equals("readString")) {
                    next();
                } else {
                    error("'readInt' or 'readString' expected");
                }
            //} else {
            //    error("'In' expected");
            //}
        }
        
        // PrimaryExpr ::= “super” “.” IdColon ExpressionList |
        // “super” “.” Id | Id | Id “.” Id | Id “.” IdColon ExpressionList |
        // “self” | “self” “.” Id | “self” ”.” IdColon ExpressionList |
        // “self” ”.” Id “.” IdColon ExpressionList | “self” ”.” Id “.” Id |
        // ReadExpr
        // ObjectCreation syntatic analysis in primaryExpr() to solve ambiguity
        // ReadExpr syntatic analysis in primaryExpr() to solve ambiguity
        private void primaryExpr() {
            switch(lexer.token) {
                case ID:
                    next();
                    
                    if(checkPass(Token.DOT)) {
                        next();
                        
                        if(checkPass(Token.NEW)) {
                            objectCreation();
                        } else if(checkPass(Token.ID)) {
                            if(lexer.getStringValue().equals("readInt") || lexer.getStringValue().equals("readString")) {
                                readExpr();
                            } else {
                                next();
                            }
                        } else {
                            check(Token.IDCOLON, "Identifier, identifer: or 'new' expected");
                            next();
                            exprList();
                        }
                    }
                    break;
                case SELF:
                    next();
                    
                    if(checkPass(Token.DOT)) {
                        next();
                        
                        if(checkPass(Token.ID)) {
                            next();
                            
                            if(checkPass(Token.DOT)) {
                                next();
                                
                                if(checkPass(Token.ID)) {
                                    next();
                                } else {
                                    check(Token.IDCOLON, "Identifier or identifer: expected");
                                    next();
                                    exprList();
                                }
                            }
                        } else {
                            check(Token.IDCOLON, "Identifier or identifer: expected");
                            next();
                            exprList();
                        }
                    }
                    break;
                case SUPER:
                    next();
                    
                    check(Token.DOT, "'.' expected");
                    next();
                    
                    if (checkPass(Token.ID)) {
                        next();
                    } else {
                        check(Token.IDCOLON, "Identifier or identifer: expected");
                        next();
                        exprList();
                    }
                    break;
                default:
                    error("Identifier, 'self' or 'super' expected");
                    break;
            }
        }
        
        // SignalFactor ::= [ Signal ] Factor
        private void signalFactor() {
            if (checkPass(Token.PLUS, Token.MINUS)) {
                next();
            }
            factor();
        }
        
        // SimpleExpression ::= SumSubExpression { “++” SumSubExpression }
        private void simpleExpr() {
            sumSubExpr();
            
            while(checkPass(Token.COMCAT)) {
                next();
                sumSubExpr();
            }
        }
        
        // SumSubExpression ::= Term { LowOperator Term }
        private void sumSubExpr() {
            term();
            
            while(checkPass(Token.PLUS, Token.MINUS, Token.OR)) {
                next();
                term();
            }
        }
        
        // Term ::= SignalFactor { HighOperator SignalFactor }
        private void term() {
            signalFactor();
            
            while(checkPass(Token.MULT, Token.DIV, Token.AND)) {
                next();
                signalFactor();
            }
        }
        
        // --------------------------------------------------- //
        
	private SymbolTable     symbolTable;
	private Lexer           lexer;
	private ErrorSignaller  signalError;

}
