// Laboratório de Compiladores - Nathan Eloy, Victor Watanabe

package comp;

import java.io.PrintWriter;
import java.util.ArrayList;
import ast.*;
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
				CianetoClassList.add(classDec());
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

	private CianetoClass classDec() {
                boolean openClass = false;
		if ( lexer.token == Token.ID && lexer.getStringValue().equals("open") ) {
                        next();
			openClass = true;
		}
		if ( lexer.token != Token.CLASS ) error("'class' expected");
		lexer.nextToken();
		if ( lexer.token != Token.ID )
			error("Identifier expected");
		String className = lexer.getStringValue();
		lexer.nextToken();
                
                // Verifies if class exists
                if (symbolTable.getInGlobal(className) != null) {
                    error("Class '"+className+"' already defined");
                }
                
                CianetoClass currClass = new CianetoClass(className, openClass);
                // Puts class in global hashtable
                symbolTable.putInGlobal(className, currClass);
                
		if ( lexer.token == Token.EXTENDS ) {
			lexer.nextToken();
			if ( lexer.token != Token.ID )
				error("Identifier expected");
                        String superclassName = lexer.getStringValue();
                        lexer.nextToken();
                        
                        // Verifies if superclass exists
                        CianetoClass superClass = (CianetoClass) symbolTable.getInClass(superclassName);
                        if (superClass == null) {
                            error("Class '"+superclassName+"' not defined");
                        } else if (!superClass.isOpen()) {
                            error("Class '"+superclassName+"' not open");
                        } else {
                            currClass.setSuperclass(superClass);
                        }
		}

		ArrayList<Member> memberList = memberList(currClass);
                
                for(Member m : memberList) {
                    if (m.getClass() == Field.class) {
                        if (m.getQualifier() == Qualifier.PR) {
                            currClass.addPrivateField((Field) m);
                        } else {
                            currClass.addPublicField((Field) m);
                        }
                    } else {
                        if (m.getQualifier() == Qualifier.PR) {
                            currClass.addPrivateMethod((Method) m);
                        } else {
                            currClass.addPublicMethod((Method) m);
                        }
                    }
                }
                
		if ( lexer.token != Token.END)
                    error("'end' expected");
		lexer.nextToken();
                symbolTable.eraseClass();
                return(currClass);
	}

	private ArrayList<Member> memberList(CianetoClass currClass) {
            ArrayList<Member> memberList = new ArrayList<>();
            while ( true ) {
                Qualifier qualifier = qualifier();
                if (checkPass(Token.VAR, Token.FUNC)) {
                        memberList.addAll(member(currClass, qualifier));
                } else {
                        break;
                }
            }
            return(memberList);
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

	private Member methodDec(CianetoClass currClass, Qualifier qualifier) {                
            lexer.nextToken();

            if (!(lexer.token == Token.ID || lexer.token == Token.IDCOLON) ){
                error("An identifier or identifer: was expected after 'func'");
            }
            String identifier = lexer.getStringValue();
            
            // Verifying qualifier
            if(!currClass.isOpen()) {
                if(qualifier == Qualifier.FI || qualifier == Qualifier.FIPU ||
                    qualifier == Qualifier.FIOV || qualifier == Qualifier.FIOVPU) {
                    error("Final classes cannot declare final methods");
                }
            }
            
            // Overriding
            if(qualifier == Qualifier.OV || qualifier == Qualifier.OVPU ||
                qualifier == Qualifier.FIOV || qualifier == Qualifier.FIOVPU ) {
                CianetoClass parent;
                if((parent = currClass.getSuperclass()) != null) {
                    boolean found = false;
                    
                    // Searches all parents
                    while(parent != null && found == false) {
                        Method parentMethod;
                        
                        if((parentMethod = parent.findPublicMethod(identifier)) != null) {
                            // Method found in parent class
                            found = true;
                            Qualifier parentQualifier = parentMethod.getQualifier();
                            
                            if (parentQualifier == Qualifier.PU) {
                                if (!(qualifier == Qualifier.OVPU || qualifier == Qualifier.FIOVPU)) {
                                    error("Override impossible, superclass class method has 'public', overriding method shall have 'override public'");
                                }
                            } else if (parentQualifier == Qualifier.FI ||
                                    parentQualifier == Qualifier.FIPU ||
                                    parentQualifier == Qualifier.FIOV ||
                                    parentQualifier == Qualifier.FIOVPU) {
                                error("Override impossible, superclass method is final");
                            }
                        }
                        parent = parent.getSuperclass();
                    }
                    if(!found) {
                        error("Override impossible, no public method was found in preceding superclasses");
                    }
                } else {
                    error("Override impossible, class has no superclass");
                }
            }
            
            Method method = new Method(qualifier, new TypeNull(), identifier);
            symbolTable.putInClass(identifier, method);

            if ( lexer.token == Token.IDCOLON ) {
                next();
                method.setParamList(formalParamDec());
            } else {
                next();
            }

            if ( lexer.token == Token.MINUS_GT ) {
                // method declared a return type
                lexer.nextToken();
                method.setType(type());
            }

            if ( lexer.token != Token.LEFTCURBRACKET ) {
                error("'{' expected");
            }
            next();

            method.setStatList(statementList());

            if ( lexer.token != Token.RIGHTCURBRACKET ) {
                    error("'}' expected");
            }

            // At end of func, all locals must be erased
            symbolTable.eraseLocal();
            next();                

            return(method);
	}

	private ArrayList<Statement> statementList() {
            ArrayList<Statement> statementList = new ArrayList<>();
            // only '}', end and 'until' is necessary in this test
            while ( !checkPass(Token.RIGHTCURBRACKET, Token.END, Token.UNTIL)) {
                    statementList.add(statement());
            }
            return(statementList);
	}

	private Statement statement() {
            Statement statement;
            boolean checkSemiColon = true;
            switch ( lexer.token ) {
            case IF:
                statement = ifStat();
                checkSemiColon = false;
                break;
            case WHILE:
                statement = whileStat();
                checkSemiColon = false;
                break;
            case RETURN:
                statement = returnStat();
                break;
            case BREAK:
                statement = breakStat();
                break;
            case SEMICOLON:
                break;
            case REPEAT:
                statement = repeatStat();
                break;
            case VAR:
                statement = localDec();
                break;
            case ASSERT:
                statement = assertStat();
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

	private ArrayList<Member> fieldDec(Qualifier qualifier) {
                // Verifies qualifier
                if(!(qualifier == Qualifier.DE || qualifier == Qualifier.PU || qualifier == Qualifier.PR)) {
                    error("Qualifier '"+qualifier+"' makes no sense on a field");
                }
		lexer.nextToken();
		Type type = type();
                
                ArrayList<String> idList = idList();
                
                if(checkPass(Token.SEMICOLON)) {
                    next();
                }
                
                ArrayList<Member> fieldList = new ArrayList<>();
                for (String id : idList) {
                    if(symbolTable.getDownTop(id) != null) {
                        error("'"+id+"' already defined");
                    } else {
                        Field newField = new Field(qualifier, type, id);
                        fieldList.add(newField);
                        symbolTable.putInClass(id, newField);
                    }
                }
                //check(Token.SEMICOLON, "';' expected", true);
                //next();
                
                return(fieldList);
	}

	private Type type() {
            switch(lexer.token) {
                case INT:
                    next();
                    return(new TypeInt());
                case BOOLEAN:
                    next();
                    return(new TypeBoolean());
                case STRING:
                    next();
                    return(new TypeString());
                case ID:
                    String className = lexer.getStringValue();
                    next();
                    
                    // Verifies if type/class exists
                    CianetoClass currClass = (CianetoClass) symbolTable.getInGlobal(className);
                    
                    if (currClass == null) {
                        this.error("Class '"+className+"' not defined");
                    }
                    
                    return(currClass);
                default:
                    this.error("A type was expected");
                    return(null);
            }
	}

	private Qualifier qualifier() {
            Qualifier qualifier = Qualifier.PU;
            
            if ( lexer.token == Token.PRIVATE ) {
                next();
                qualifier = Qualifier.PR;
            }
            else if ( lexer.token == Token.PUBLIC ) {
                next();
                qualifier = Qualifier.PU;
            }
            else if ( lexer.token == Token.OVERRIDE ) {
                next();
                qualifier = Qualifier.OV;
                if ( lexer.token == Token.PUBLIC ) {
                    next();
                    qualifier = Qualifier.OVPU;
                }
            }
            else if ( lexer.token == Token.FINAL ) {
                next();
                qualifier = Qualifier.FI;
                if ( lexer.token == Token.PUBLIC ) {
                    next();
                    qualifier = Qualifier.FIPU;
                }
                else if ( lexer.token == Token.OVERRIDE ) {
                    next();
                    qualifier = Qualifier.FIOV;
                    if ( lexer.token == Token.PUBLIC ) {
                            next();
                            qualifier = Qualifier.FIOVPU;
                    }
                }
            }

            return(qualifier);
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
        private ArrayList<Param> formalParamDec() {
            ArrayList<Param> paramList = new ArrayList<>();
            paramList.add(paramDec());
            
            while(checkPass(Token.COMMA)) {
                next();
                paramList.add(paramDec());
            }
            
            return(paramList);
        }
        
        // IdList ::= Id { “,” Id }
        private ArrayList<String> idList() {
            ArrayList<String> idList = new ArrayList<>();
            
            check(Token.ID, "Identifier expected");
            idList.add(lexer.getStringValue());
            next();
            
            while(checkPass(Token.COMMA)) {
                next();
                check(Token.ID, "Identifier expected");
                idList.add(lexer.getStringValue());
                next();
            }
            
            return(idList);
        }
        
        // Member ::= FieldDec | MethodDec
        private ArrayList<Member> member(CianetoClass currClass, Qualifier qualifier) {
            ArrayList<Member> memberList = new ArrayList<>();
            if(checkPass(Token.VAR)) {
                memberList.addAll(fieldDec(qualifier));
            } else if(checkPass(Token.FUNC)) {
                memberList.add(methodDec(currClass, qualifier));
            } else {
                error("'var' or 'func' expected");
            }
            return(memberList);
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
        private Param paramDec() {
            Type type = type();
            
            check(Token.ID, "Identifier expected");
            String identifier = lexer.getStringValue();
            next();
            
            Param param = new Param(type, identifier);
            
            // Puts parameters in local hashtable
            symbolTable.putInLocal(identifier, param);
                    
            return(param);
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
