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
                
                CianetoClass progClass = (CianetoClass) symbolTable.getInGlobal("Program");
                if(progClass != null) {
                    Method runMethod = progClass.findPublicMethod("run");
                    if(runMethod == null) {
                        error("No method 'run' was declared in 'Program'");
                    } else if(runMethod.getType() != Type.nullType) {
                        error("Method 'run' must have no return");
                    }
                } else {
                    error("No class 'Program' was declared");
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
                    error("Class '"+className+"' already declared");
                }
                CianetoClass currClass = new CianetoClass(className, openClass);
                // Puts class in global hashtable
                symbolTable.putInGlobal(className, currClass);
                currentClass = currClass;
                
		if ( lexer.token == Token.EXTENDS ) {
			lexer.nextToken();
			if ( lexer.token != Token.ID )
				error("Identifier expected");
                        String superclassName = lexer.getStringValue();
                                                
                        // Verifies if superclass exists
                        CianetoClass superClass = (CianetoClass) symbolTable.getInGlobal(superclassName);
                        if (superClass == null) {
                            error("Class '"+superclassName+"' not declared", true);
                        } else if (!superClass.isOpen()) {
                            error("Class '"+superclassName+"' not open", true);
                        } else {
                            currClass.setSuperclass(superClass);
                        }
                        
                        lexer.nextToken();
		}
		ArrayList<Member> memberList = memberList(currClass);
//                if(memberList != null && !memberList.isEmpty()) {
//                    for(Member m : memberList) {
//                        if (m.getClass() == Field.class) {
//                            if (m.getQualifier() == Qualifier.PR) {
//                                currClass.addPrivateField((Field) m);
//                            } else {
//                                currClass.addPublicField((Field) m);
//                            }
//                        } else {
//                            if (m.getQualifier() == Qualifier.PR) {
//                                currClass.addPrivateMethod((Method) m);
//                            } else {
//                                currClass.addPublicMethod((Method) m);
//                            }
//                        }
//                    }
//                }
		if ( lexer.token != Token.END)
                    error("'end' expected");
		lexer.nextToken();
                symbolTable.eraseClass();
                currentClass = null;
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
                error("An identifier or identifier: was expected after 'func'");
            }
            String identifier = lexer.getStringValue();
            
            if(symbolTable.getInClass(identifier) != null) {
                error("'"+identifier+"' already declared");
            }
            
            CianetoClass superClass = currClass.getSuperclass();
            if(superClass != null) {
                if(superClass.findPublicMethod(identifier) != null) {
                    if(qualifier != Qualifier.OV && qualifier != Qualifier.OVPU) {
                        error("Overriding superclass '"+superClass.getName()+"' method without 'override' qualifier before method");
                    }
                }
            }
            
            // Verifying qualifier
            if(!currClass.isOpen()) {
                if(qualifier == Qualifier.FI || qualifier == Qualifier.FIPU ||
                    qualifier == Qualifier.FIOV || qualifier == Qualifier.FIOVPU) {
                    error("Final class '"+currClass.getName()+"' is not allowed to declare final methods");
                }
            }
                        
            Method method = new Method(qualifier, Type.nullType, identifier);
            symbolTable.putInClass(identifier, method);
            if(qualifier == Qualifier.PR) {
                currentClass.addPrivateMethod(method);
            } else {
                currentClass.addPublicMethod(method);
            }

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
                            
                            if(method.getType() != parentMethod.getType()) {
                                error("Overriding '"+parent.getName()+"' method '"+parentMethod.getIdentifier()+"' is impossible, methods have different return types");
                            } else {
                                if((method.getParamList() != null && parentMethod.getParamList() != null) &&(!method.getParamList().isEmpty() && !parentMethod.getParamList().isEmpty())){
                                    if(method.getParamList().size() == parentMethod.getParamList().size()) {
                                        for(int i=1; i < method.getParamList().size(); i++) {
                                            if(method.getParamList().get(i).getType() != parentMethod.getParamList().get(i).getType()) {
                                                error("Overriding '"+parent.getName()+"' method '"+parentMethod.getIdentifier()+"' is impossible, methods have different signatures");
                                            }
                                        }
                                    } else {
                                        error("Overriding '"+parent.getName()+"' method '"+parentMethod.getIdentifier()+"' is impossible, methods have different signature sizes");
                                    }
                                }
                            }
                            
                            if (parentQualifier == Qualifier.PU) {
                                if (!(qualifier == Qualifier.OVPU || qualifier == Qualifier.FIOVPU)) {
                                    //error("Overriding '"+parent.getName()+"' method '"+parentMethod.getIdentifier()+"' is impossible, superclass class method has explicit qualifier 'public', overriding method should have explicit qualifier 'override public'");
                                }
                            } else if (parentQualifier == Qualifier.FI ||
                                    parentQualifier == Qualifier.FIPU ||
                                    parentQualifier == Qualifier.FIOV ||
                                    parentQualifier == Qualifier.FIOVPU) {
                                error("Overriding '"+parent.getName()+"' method '"+parentMethod.getIdentifier()+"' is impossible, superclass method is final");
                            }
                            
                            
                        }
                        parent = parent.getSuperclass();
                    }
                    if(!found) {
                        error("Overriding a method '"+identifier+"' is impossible, no public method was found in preceding superclasses");
                    }
                } else {
                    error("Overriding a method '"+identifier+"' is impossible, class has no superclass");
                }
            }

            if ( lexer.token != Token.LEFTCURBRACKET ) {
                error("'{' expected");
            }
            next();
            
            currentMethod = method;
            method.setStatList(statementList());

            if ( lexer.token != Token.RIGHTCURBRACKET ) {
                    error("'}' expected");
            }
            
            // Verifies if return is necessary and present
            if(currentMethod.getType() != Type.nullType && !currentMethod.isReturnDefined()) {
                error("Return expected");
            }
                    
            // At end of func, all locals must be erased
            symbolTable.eraseLocal();
            currentMethod = null;
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
            Statement statement = null;
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
                    statement = writeStat();
                }
                else if (checkPass(Token.PLUS, Token.MINUS, Token.INT, 
                    Token.BOOLEAN, Token.STRING, Token.LEFTPAR, 
                    Token.NOT, Token.NULL, Token.ID, Token.SUPER,
                    Token.SELF)) {
                    statement = assignExpr();
                } else {
                    error("Statement expected");
                }

            }
            if ( checkSemiColon ) {
                check(Token.SEMICOLON, "';' expected", true);
                next();
            }
            return(statement);
	}

	private Statement localDec() {
		next();
		Type type = type();
		
                ArrayList<String> idList = idList();
                
		if ( lexer.token == Token.ASSIGN ) {
			next();
			// check if there is just one variable
			Type exprType = expr();
                        
                        if(exprType != type) {
                            error("Invalid type: '"+type.getName()+"' expected");
                        }
		}

                for (String id : idList) {
                    if(symbolTable.getInLocal(id) != null) {
                        error("'"+id+"' already declared");
                    } else {
                        Variable newVar = new Variable(type, id);
                        symbolTable.putInLocal(id, newVar);
                    }
                }
                
                return(new LocalDecStatement());
	}

	private RepeatStatement repeatStat() {
		next();
                //while ( lexer.token != Token.UNTIL && lexer.token != Token.RIGHTCURBRACKET && lexer.token != Token.END ) {
                //        statement();
                //}
                RepeatStatement repeatStat = new RepeatStatement(statementList());
                if(highestAboveStatement == null) {
                    highestAboveStatement = repeatStat;
                }
		check(Token.UNTIL, "'until' was expected");
                next();
                Type type = expr();
                
                if(type != Type.booleanType) {
                    error("Boolean type expected");
                }
                
                if(highestAboveStatement == repeatStat) {
                    highestAboveStatement = null;
                }
                return(repeatStat);
	}

	private Statement breakStat() {
            next();
            if(highestAboveStatement == null) {
                error("'break' outside repetition statement");
            }
            return(new BreakStatement());
	}

	private Statement returnStat() {
		next();
                Type returnType = expr();
                
                if (currentMethod.getType() != returnType) {
                    error("Invalid return type: "+currentMethod.getType()+" expected");
                }
                currentMethod.setReturnDefined(true);
                return(new ReturnStatement(returnType));
	}

	private Statement whileStat() {
		next();
                WhileStatement whileStat = new WhileStatement();
                if(highestAboveStatement == null) {
                    highestAboveStatement = whileStat;
                }
		Type type = expr();
                
                if(type != Type.booleanType) {
                    error("Boolean type expected");
                }
                
		check(Token.LEFTCURBRACKET, "'{' expected after the 'while' expression");
		next();
		while ( lexer.token != Token.RIGHTCURBRACKET && lexer.token != Token.END ) {
			whileStat.addStatement(statement());
		}
		check(Token.RIGHTCURBRACKET, "'}' expected");
                next();
                
                if(highestAboveStatement == whileStat) {
                    highestAboveStatement = null;
                }
                return(whileStat);
	}

	private Statement ifStat() {
		next();
		IfStatement ifStat = new IfStatement();
                Type type = expr();
                
                if(type != Type.booleanType) {
                    error("Boolean type expected");
                }
                                
		check(Token.LEFTCURBRACKET, "'{' expected after the 'if' expression");
		next();
		while ( lexer.token != Token.RIGHTCURBRACKET && lexer.token != Token.END && lexer.token != Token.ELSE ) {
			ifStat.setIfStat(statement());
		}
		check(Token.RIGHTCURBRACKET, "'}' expected");
                next();
		if ( lexer.token == Token.ELSE ) {
			next();
			check(Token.LEFTCURBRACKET, "'{' expected after 'else'");
			next();
			while ( lexer.token != Token.RIGHTCURBRACKET ) {
				ifStat.setElseStat(statement());
			}
			check(Token.RIGHTCURBRACKET, "'}' expected");
                        next();
		}
                return(ifStat);
	}

	/**

	 */
	private Statement writeStat() {
		next();
		check(Token.DOT, "'.' expected after 'Out'");
		next();
		check(Token.IDCOLON, "'print:' or 'println:' expected after 'Out.'");
		String printName = lexer.getStringValue();
                next();
                
                if(!(printName.equals("print:") || printName.equals("println:"))) {
                    error("'print:' or 'println:' expected after 'Out.'");
                }
                
		Type type = expr();
                if((type != Type.stringType) && (type != Type.intType)) {
                    error("Invalid type: Int or String expected");
                }
                return (new WriteStatement());
	}

	private ArrayList<Member> fieldDec(Qualifier qualifier) {
                // Verifies qualifier
                if(!(qualifier == Qualifier.DE || qualifier == Qualifier.PU || qualifier == Qualifier.PR)) {
                    error("Qualifier '"+qualifier+"' makes no sense on a field");
                }
		lexer.nextToken();
		Type type = type();
                
                ArrayList<String> idList = idList();
                                
                ArrayList<Member> fieldList = new ArrayList<>();
                for (String id : idList) {
                    if(symbolTable.getDownTop(id) != null) {
                        error("'"+id+"' already declared");
                    } else {
                        Field newField = new Field(qualifier, type, id);
                        fieldList.add(newField);
                        symbolTable.putInClass(id, newField);
                        
                        if(qualifier == Qualifier.PR) {
                            currentClass.addPrivateField(newField);
                        } else {
                            currentClass.addPublicField(newField);
                        }
                    }
                }
                
                if(checkPass(Token.SEMICOLON)) {
                    next();
                }
                //check(Token.SEMICOLON, "';' expected", true);
                //next();
                
                return(fieldList);
	}

	private Type type() {
            switch(lexer.token) {
                case INT:
                    next();
                    return(Type.intType);
                case BOOLEAN:
                    next();
                    return(Type.booleanType);
                case STRING:
                    next();
                    return(Type.stringType);
                case ID:
                    String className = lexer.getStringValue();
                    next();
                    
                    // Verifies if type/class exists
                    CianetoClass currClass = (CianetoClass) symbolTable.getInGlobal(className);
                    
                    if (currClass == null) {
                        this.error("Class '"+className+"' not declared");
                    }
                    
                    return(currClass);
                default:
                    this.error("A type was expected");
                    return(Type.undefinedType);
            }
	}

	private Qualifier qualifier() {
            Qualifier qualifier = Qualifier.DE;
            
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
        private AssignStatement assignExpr() {
            Type ltype = expr();
            
            if(checkPass(Token.ASSIGN)) {
                next();
                Type rtype = expr();
                boolean A;
                if((ltype.getClass() != CianetoClass.class && ltype != rtype) || (ltype == Type.nullType)) {
                    error("Invalid assignment between types '"+ltype.getName()+"' and '"+rtype.getName()+"'");
                } else if (ltype.getClass() == CianetoClass.class ){
                    if (rtype.getClass() == CianetoClass.class) {
                        CianetoClass lclass = (CianetoClass) ltype;
                        CianetoClass rclass = (CianetoClass) rtype;
                        boolean found = false;

                        while(rclass != null && found == false) {
                            if(lclass.getName().equals(rclass.getName())) {
                                found = true;
                            }
                            rclass = rclass.getSuperclass();
                        }
                        rclass = (CianetoClass) rtype;
                        if(!found) {
                            error("Invalid assignment between unrelated classes '"+lclass.getName()+"' and '"+rclass.getName()+"'");
                        }
                    }
                    else if (rtype != Type.nullType){
                        error("Invalid assignment between Class '"+ltype.getName()+"' and type'"+rtype.getName()+"'");
                    }
                    
                } 
            }
            return new AssignStatement();
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
        private Type expr() {            
            Type ltype = simpleExpr();
            
            if(checkPass(Token.EQ, Token.LT, Token.GT, Token.LE, Token.GE, Token.NEQ)) {
                Token relation = lexer.token;
                next();
                Type rtype = simpleExpr();
                
                if (relation == Token.LT || relation == Token.GT || 
                    relation == Token.LE || relation == Token.GE) {
                    if (ltype != Type.intType || rtype != Type.intType) {
                        error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for comparison: Int expected");
                    } else {
                        return(Type.booleanType);
                    }
                } else if (relation == Token.EQ || relation == Token.NEQ) {
                    if ((ltype == Type.stringType && rtype == Type.stringType) ||
                        (ltype == Type.intType && rtype == Type.intType) ||
                        (ltype == Type.booleanType && rtype == Type.booleanType) ||
                        ((ltype == Type.stringType || rtype == Type.stringType) && (ltype == Type.nullType || rtype == Type.nullType))) {
                        return(Type.booleanType);
                    } else if ((ltype.getClass() == CianetoClass.class) && (rtype.getClass() == CianetoClass.class)) {
                        CianetoClass lclass = (CianetoClass) ltype;
                        CianetoClass rclass = (CianetoClass) rtype;
                        boolean found = false;
                        
                        while(rclass != null && found == false) {
                            if(lclass.getName().equals(rclass.getName())) {
                                found = true;
                            }
                            rclass = rclass.getSuperclass();
                        }
                        rclass = (CianetoClass) rtype;
                        while(lclass != null && found == false) {
                            if(lclass.getName().equals(rclass.getName())) {
                                found = true;
                            }
                            lclass = lclass.getSuperclass();
                        }
                        
                        
                        if(found) {
                            return(Type.booleanType);
                        } else {
                            error("Incompatible types cannot be compared with '==' because the results will always be 'false'");
                            //error("Invalid classes '"+lclass.getName()+"' and '"+rclass.getName()+"' for comparison");
                        }
                    } else {
                        error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for comparison");
                    }
                }
            }
            
            return (ltype);
        }
        
        // ExpressionList ::= Expression { “,” Expression }
        private void exprList(Method m) {
            int paramIndex = 0;
            Type type = expr();
            ArrayList<Param> paramList = m.getParamList();
            
            if(paramList.get(paramIndex).getType() != type) {
                error("Incompatible passed parameter "+paramIndex+" type: '"+paramList.get(paramIndex).getType()+"' expected");
            }
            
            while(checkPass(Token.COMMA)) {
                paramIndex++;
                next();
                type = expr();
                if(paramList.get(paramIndex).getType() != type) {
                    error("Incompatible passed parameter "+paramIndex+" type: '"+paramList.get(paramIndex).getType()+"' expected");
                }
            }
        }
        
        // Factor ::= BasicValue | “(” Expression “)” | “!” Factor | “nil” 
        // | ObjectCreation | PrimaryExpr
        // ObjectCreation syntatic analysis in primaryExpr() to solve ambiguity
        private Type factor() {
            System.out.print("   <2>   ");
            Type type = Type.undefinedType;
            if(checkPass(Token.LEFTPAR)) {
                System.out.print("   <3>   ");
                next();
                type = expr();
                System.out.print("   <4>   ");
                check(Token.RIGHTPAR, "')' expected");
                next();
            } else if(checkPass(Token.NOT)) {
                next();
                type = factor();
                
                if(type != Type.booleanType) {
                    error("Invalid type: Boolean expected");
                }
            } else if(checkPass(Token.NULL)) {
                next();
                type = Type.nullType;
            } else if(checkPass(Token.ID, Token.SELF, Token.SUPER)){
                System.out.print("   <4>   ");
                return(primaryExpr());
            } else if(checkPass(Token.LITERALINT, Token.TRUE, Token.FALSE, Token.LITERALSTRING)) {
                switch(lexer.token) {
                    case LITERALINT:
                        type = Type.intType;
                        break;
                    case TRUE:
                        type = Type.booleanType;
                        break;
                    case FALSE:
                        type = Type.booleanType;
                        break;
                    case LITERALSTRING:
                        type = Type.stringType;
                        break;
                }
                next();
            } else {
                error("Expression expected");
                //error("Factor (Literal int, boolean, literal string, identifier, '(', '!', 'nil', 'self' or 'super') expected");
            }
            System.out.print("   <5>   "+lexer.token);
            
            return(type);
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
                if(qualifier == Qualifier.PU){
                    error("Attempt to declare public instance variable 'i'");
                }
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
        private Type objectCreation(String classId) {
            CianetoClass foundClass = (CianetoClass) symbolTable.getInGlobal(classId);
            if(foundClass == null) {
                error("Class '"+classId+"' not declared");
            }
            //check(Token.ID, "Identifier expected");
            //next();
            //check(Token.DOT, "'.' expected");
            //next();
            check(Token.NEW, "'new' expected");
            next();
            
            return(foundClass);
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
        private Type readExpr() {
            Type type = Type.undefinedType;
            //check(Token.ID, "Identifier expected");
            //if(lexer.getStringValue().equals("In")) {
            //    next();
                
            //    check(Token.DOT, "'.' expected");
            //    next();
                
                check(Token.ID, "Identifier expected");
                if(lexer.getStringValue().equals("readInt") || lexer.getStringValue().equals("readString")) {
                    type = lexer.getStringValue().equals("readInt") ? Type.intType : Type.stringType ;
                    next();
                } else {
                    error("'readInt' or 'readString' expected");
                }
            //} else {
            //    error("'In' expected");
            //}
            return(type);
        }
        
        // PrimaryExpr ::= “super” “.” IdColon ExpressionList |
        // “super” “.” Id | Id | Id “.” Id | Id “.” IdColon ExpressionList |
        // “self” | “self” “.” Id | “self” ”.” IdColon ExpressionList |
        // “self” ”.” Id “.” IdColon ExpressionList | “self” ”.” Id “.” Id |
        // ReadExpr
        // ObjectCreation syntatic analysis in primaryExpr() to solve ambiguity
        // ReadExpr syntatic analysis in primaryExpr() to solve ambiguity
        private Type primaryExpr() {
            Type type = Type.undefinedType;
            switch(lexer.token) {
                case ID:
                    String localId = lexer.getStringValue();
                    next();
                    
                    if(checkPass(Token.DOT)) {
                        String instanceId = localId;
                        next();
                        
                        if(checkPass(Token.NEW)) {
                            type = objectCreation(localId);
                        } else if(checkPass(Token.ID)) {
                            if(lexer.getStringValue().equals("readInt") || lexer.getStringValue().equals("readString")) {
                                type = readExpr();
                            } else {
                                String memberId = lexer.getStringValue();
                                next();
                                
                                Member fieldFound = (Member) symbolTable.getInLocal(instanceId);
                                if(fieldFound == null) {
                                    error("'"+instanceId+"' not declared");
                                }
                                Type classType = fieldFound.getType();
                                CianetoClass classFound = (CianetoClass) symbolTable.getInGlobal(classType.getName());
                                
                                if(classFound == null) {
                                    error("'"+instanceId+"' not declared");
                                } else {
                                    Member memberFound = classFound.findPublicField(memberId);
                                    
                                    if(memberFound == null && (memberFound = (Member) classFound.findOutMethod(memberId)) == null) {
                                        error("Class '"+instanceId+"' has no public field or method '"+memberId+"'");
                                    } else {
                                        type = memberFound.getType();
                                    }
                                }
                            }
                        } else {
                            check(Token.IDCOLON, "Identifier, identifier: or 'new' expected");
                            String methodId = lexer.getStringValue();
                            next();
                            
                            Member fieldFound = (Member) symbolTable.getInLocal(instanceId);
                            if(fieldFound == null) {
                                error("'"+instanceId+"' not declared");
                            }
                            Type classType = fieldFound.getType();
                            CianetoClass classFound = (CianetoClass) symbolTable.getInGlobal(classType.getName());
                            
                            Method methodFound = null;
                            if(classFound == null) {
                                error("'"+instanceId+"' not declared");
                            } else {
                                methodFound = classFound.findOutMethod(methodId);

                                if(methodFound == null) {
                                    error("Class '"+classFound.getName()+"' or superclass has no public method '"+methodId+"'");
                                } else {
                                    type = methodFound.getType();
                                }
                            }
                            
                            exprList(methodFound);
                        }
                    } else {
                        Member localFound = (Member) symbolTable.getInLocal(localId);
                        if(localFound == null) {
                            error("'"+localId+"' not declared");
                        } else {
                            type = localFound.getType();
                        }
                    }
                    break;
                case SELF:
                    next();
                    
                    if(checkPass(Token.DOT)) {
                        next();
                        
                        if(checkPass(Token.ID)) {
                            String memberId = lexer.getStringValue();
                            next();
                            
                            if(checkPass(Token.DOT)) {
                                Field fieldFound = currentClass.findPrivateField(memberId);
                                next();
                                
                                if(fieldFound == null) {
                                                        error("Class '"+currentClass.getName()+"' or superclass has no field '"+memberId+"'");
                                } else {
                                    Type classType = fieldFound.getType();
                                    CianetoClass classFound = (CianetoClass) symbolTable.getInGlobal(classType.getName());
                                
                                    if(checkPass(Token.ID)) {
                                        String methodId = lexer.getStringValue();

                                        Method method = classFound.findOutMethod(methodId);
                                        if(method == null) {
                                            error("Method '"+methodId+"' not declared on class '"+type.getName()+"'");
                                        }
                                        type = method.getType();
                                        next();
                                    } else {
                                        check(Token.IDCOLON, "Identifier or identifier: expected");
                                        String methodId = lexer.getStringValue();
                                        
                                        Method method = classFound.findOutMethod(methodId);
                                        
                                        if(method == null) {
                                            error("Method '"+methodId+"' not declared on class '"+type.getName()+"'");
                                        }
                                        type = method.getType();
                                        next();
                                        exprList(method);
                                    }
                                }
                            } else {
                                Field selfField = currentClass.findPrivateField(memberId);
                                
                                if(selfField != null) {
                                    type = selfField.getType();
                                } else {
                                    Method selfMethod = currentClass.findInMethod(memberId);
                                    if (selfMethod != null) {
                                        type = selfMethod.getType();
                                    } else {
                                        error("Class '"+currentClass.getName()+"' has no method or field '"+memberId+"'");
                                    }
                                }
                            }
                        } else {
                            check(Token.IDCOLON, "Identifier or identifier: expected");
                            String methodId = lexer.getStringValue();
                            next();
                            
                            Method methodFound = currentClass.findInMethod(methodId);
                            if(methodFound == null) {
                                error("Class '"+currentClass.getName()+"' or superclass has no public method '"+methodId+"'");
                            } else {
                                type = methodFound.getType();
                            }
                            
                            exprList(methodFound);
                        }
                    } else {
                        return(currentClass);
                    }
                    break;
                case SUPER:
                    CianetoClass superClass = currentClass.getSuperclass();
                    if (superClass == null) {
                        error("Class '"+currentClass.getName()+"' has no superclass");
                    }
                    next();
                    
                    check(Token.DOT, "'.' expected");
                    next();
                    
                    if (checkPass(Token.ID)) {
                        String superMemberId = lexer.getStringValue();
                        next();
                        
                        Field superField = superClass.findPublicField(superMemberId);
                        Method superMethod = superClass.findOutMethod(superMemberId);
                        if(superField != null) {
                            type = superField.getType();
                        } else if (superMethod != null) {
                            type = superMethod.getType();
                        } else {
                            error("'"+currentClass.getName()+"' superclass has no method or field '"+superMemberId+"'");
                        }
                    } else {
                        check(Token.IDCOLON, "Identifier or identifier: expected");
                        String superMemberId = lexer.getStringValue();
                        next();
                        
                        Method superMethod = superClass.findOutMethod(superMemberId);
                        if (superMethod != null) {
                            type = superMethod.getType();
                        } else {
                            error("'"+currentClass.getName()+"' superclass has no method or field '"+superMemberId+"'");
                        }
                        exprList(superMethod);
                    }
                    break;
                default:
                    error("Identifier, 'self' or 'super' expected");
                    break;
            }
            return(type);
        }
        
        // SignalFactor ::= [ Signal ] Factor
        private Type signalFactor() {
            if (checkPass(Token.PLUS, Token.MINUS)) {
                Token signal = lexer.token;
                next();
                
                Type type = factor();
                
                if (type != Type.intType) {
                    error("Invalid type for '"+signal+"' signal");
                }
                return(type);
            } else {
                
                System.out.print("   <1>   ");
                Type f = factor();
                System.out.print("   <1.2>   ");
                return(f);
            }
        }
        
        // SimpleExpression ::= SumSubExpression { “++” SumSubExpression }
        private Type simpleExpr() {
            Type ltype = sumSubExpr();
            
            while(checkPass(Token.COMCAT)) {
                next();
                Type rtype = sumSubExpr();
                if((ltype == Type.intType || ltype == Type.stringType) && (rtype == Type.intType || rtype == Type.stringType)) {
                    ltype = Type.stringType;
                } else {
                    error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for concatenation: Int or String expected");
                }
            }
            return(ltype);
        }
        
        // SumSubExpression ::= Term { LowOperator Term }
        private Type sumSubExpr() {
            Type ltype = term();
            while(checkPass(Token.PLUS, Token.MINUS, Token.OR)) {
                Token relation =  lexer.token;
                next();
                Type rtype = term();
                
                if (relation == Token.PLUS || relation == Token.MINUS) {
                    if(ltype == Type.intType && rtype == Type.intType) {
                        return(Type.intType);
                    } else {
                        error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for operation: Int expected");
                    }
                } else {
                    if(ltype == Type.booleanType && rtype == Type.booleanType) {
                        return(Type.booleanType);
                    } else {
                        error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for operation: Boolean expected");
                    }
                }
            }
            return(ltype);
        }
        
        // Term ::= SignalFactor { HighOperator SignalFactor }
        private Type term() {
            Type ltype = signalFactor();
            System.out.print("   <5.5>   ");
            while(checkPass(Token.MULT, Token.DIV, Token.AND)) {
                System.out.print("   <6>   ");
                Token relation = lexer.token;
                next();
                Type rtype = signalFactor();
                if (relation == Token.MULT || relation == Token.DIV) {
                    if(ltype == Type.intType && rtype == Type.intType) {
                        System.out.print("RETURN1");
                        return(Type.intType);
                    } else {
                        error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for operation: Int expected");
                    }
                } else {
                    if(ltype != Type.booleanType || rtype != Type.booleanType) {
                        error("Invalid types '"+ltype.getName()+"' and '"+rtype.getName()+"' for operation: Boolean expected");
                    } else {
                        //System.out.print("RETURN2");
                        //return(Type.booleanType);
                    }
                }
            }
            return(ltype);
        }
        
        // --------------------------------------------------- //
        
	private SymbolTable     symbolTable;
	private Lexer           lexer;
	private ErrorSignaller  signalError;
        private CianetoClass    currentClass;
        private Method          currentMethod;
        private Statement       highestAboveStatement = null;

}
