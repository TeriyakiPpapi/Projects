package IgniV;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;


/**
 * Recursive descent parser for the igniv language.
 */
class IgniVParser implements IgniVTokens
{
    private IgniVLexer lexer;
    private IgniVProgram igniv;


    public IgniVParser(IgniVLexer lexer) {
        this.lexer = lexer;
    }


    /**
     * Parse, return only if the program successfully parses.
     */
    public ParseTree parse() {
        // create the program
        igniv = new IgniVProgram();

        // call the start symbol
        program();

        return igniv;
    }


    /**
     * Parse a program.
     */
    private ParseTree program() {
        lexer.next();
        
        while(has(FUNCTION)) {
            igniv.add(funDef());
        }
        
        mustBe(MAIN);
        lexer.next();
        mustBe(LPAREN);
        lexer.next();
        mustBe(RPAREN);
        lexer.next();
        mustBe(LCURLY);
        lexer.next();
        
        igniv.add(stmntList());
        
        mustBe(RCURLY);
        lexer.next();

        mustBe(EOF);
        return igniv;
    }


    /** 
     * Parse a stmnt-list.
     */
    private ParseTree stmntList() {
        IgniVProgram parent = igniv;
        IgniVProgram result = new IgniVProgram(parent);

        //enter the new scope
        igniv = result;

        // parse this list iteratively
        while(has(REF) || has(PRINT) || has(PRINTLN) || has(READ) || has(RETURN)
                || has(WHILE) || has(IF) || has(LPAREN) || isLiteral()) {
            result.add(stmnt());
            
        }

        //Exclude null programs.
        if(result.size() == 0) {
            System.out.println("Error! Null programs are not allowed.");
            System.exit(-2);
        }

        //restore scope
        igniv = parent;

        return result;
    }


    /**
     * Parse a stmnt.
     */
    private ParseTree stmnt() {
        // check for refOrExpr
        if(has(REF) || has(LPAREN) || has(INTEGER) || has(FLOAT)) {
            ParseTree result = refOrExpr();
            mustBe(SEMICOLON);
            lexer.next();
            return result;
        }
        else if(has(WHILE)) {
            return whiles();
        }
        else if(has(IF)) {
            return ifElse();
        }
        else if (has(RETURN)) {
            ParseTree result = returns();
            mustBe(SEMICOLON);
            lexer.next();
            return result;
        }
        else if (has(READ)) {
            ParseTree result = read();
            mustBe(SEMICOLON);
            lexer.next();
            return result;
        }
        else if(has(PRINTLN)){
            ParseTree result = println();
            mustBe(SEMICOLON);
            lexer.next();
            return result;
        }
        else {
            mustBe(PRINT);
            ParseTree result = print();
            mustBe(SEMICOLON);
            lexer.next();
            return result;
        }
        
    }
    
    
    /**
     * Parse a while block.
     */
    private ParseTree whiles() {
        mustBe(WHILE);
        lexer.next();
        mustBe(LPAREN);
        lexer.next();
        
        ParseTree left = expr();
        int operator;
        if (has(NOTEQUAL) || has(LESSTHAN) || has(LESSTHANEQ)
                || has(GREATTHAN) || has(GREATTHANEQ)) {
            operator = lexer.token();
            lexer.next();
        }
        else {
            mustBe(ISEQUAL);
            operator = lexer.token();
            lexer.next();
        }
        
        ParseTree right = expr();
        mustBe(RPAREN);
        lexer.next();
        
        IgniVProgram.While loop = igniv.new While(left, right, operator);
        
        mustBe(LCURLY);
        lexer.next();
        
        while(!has(RCURLY)) {
            loop.add(stmnt());
        }
        
        mustBe(RCURLY);
        lexer.next();
        
        return loop;
    }
    
    
    /**
     * Parse an If-Else block.
     */
    private ParseTree ifElse() {
        mustBe(IF);
        lexer.next();
        mustBe(LPAREN);
        lexer.next();
        
        ParseTree left = expr();
        int operator;
        if (has(NOTEQUAL) || has(LESSTHAN) || has(LESSTHANEQ)
                || has(GREATTHAN) || has(GREATTHANEQ)) {
            operator = lexer.token();
            lexer.next();
        }
        else {
            mustBe(ISEQUAL);
            operator = lexer.token();
            lexer.next();
        }
        
        ParseTree right = expr();
        mustBe(RPAREN);
        lexer.next();
        
        IgniVProgram.IfElse block = igniv.new IfElse(left, right, operator);
        
        mustBe(LCURLY);
        lexer.next();
        
        while(!has(RCURLY)) {
            block.addI(stmnt());
        }
        
        mustBe(RCURLY);
        lexer.next();
        
        if(has(ELSE)) {
            lexer.next();

            mustBe(LCURLY);
            lexer.next();

            while(!has(RCURLY)) {
                block.addE(stmnt());
            }

            mustBe(RCURLY);
            lexer.next();
        }
        
        return block;
    }
    
    
    /**
     * Parse a return statement.
     */
    private ParseTree returns() {
        mustBe(RETURN);
        lexer.next();
        
        return igniv.new Return(expr());
    }


    /** 
     * Parse fun-def
     */
    private ParseTree funDef() {
        //TODO: Actually implement this thing

        mustBe(FUNCTION);
        lexer.next();

        mustBe(REF);
        IgniVProgram.FunDef f = igniv.new FunDef(lexer.string());
        lexer.next();

        mustBe(LPAREN);
        lexer.next();

        // parameter list
        while(has(REF)) {
            //consume the reference
            f.addParameter(lexer.string());
            lexer.next();

            //consume the comma if it is there
            if(has(COMMA)) {
                lexer.next();
            } else {
                mustBe(RPAREN); // be careful!
            }
        }

        // consume RPAREN
        mustBe(RPAREN);
        lexer.next();
        mustBe(LCURLY);
        lexer.next();

        // get the statement list
        f.setBody((IgniVProgram)stmntList());

        // consume the end
        mustBe(RCURLY);
        lexer.next();

        return f;
    }


    /** 
     * Parse ref-or-expr
     */
    private ParseTree refOrExpr()
    {
        if(has(REF)) {
            //consume the reference
            ParseTree ref = igniv.new VarRef(lexer.string());
            lexer.next();
            return refOrExpr2(ref);
        } else {
            return expr();
        }
    }


    /**
     * Parse ref-or-expr'
     */
    private ParseTree refOrExpr2(ParseTree left)
    {
        if (has(LBRACKET)) {
            lexer.next();
            ParseTree index = expr();
            mustBe(RBRACKET);
            lexer.next();
            
            if (has(ASSIGN)) {
                //consume the assign
                lexer.next();
                return igniv.new Assign((IgniVProgram.VarRef)left, index, (IgniVProgram.Expr) expr());
            }
            else {
                return left;
            }
        }
        else if(has(ASSIGN)) {
            //consume the assign
            lexer.next();
            return igniv.new Assign((IgniVProgram.VarRef)left, (IgniVProgram.Expr) expr());
        } else if(has(LPAREN)) {
            lexer.next();
            return funCallRHS(((IgniVProgram.VarRef)left).name); 
        } else {
            return expr2(left);
        }
    }


    /**
     * Parse expr
     */
    private ParseTree expr()
    {
        ParseTree left = term();
        return expr2(left);
    }


    /**
     * Parse expr'
     */
    private ParseTree expr2(ParseTree left)
    {
        if(has(PLUS)) {
            //addition
            lexer.next();
            return igniv.new Add(left, expr());
        } else if(has(MINUS)){
            //subtraction
            lexer.next();
            return igniv.new Subtract(left, expr());
        }

        //empty (no mustBe())
        return left;
    }


    /**
     * Parse term
     */
    private ParseTree term()
    {
        return term2(factor());
    }


    /**
     * Parse term'
     */
    private ParseTree term2(ParseTree left)
    {
        if(has(TIMES)) {
            //multiplication
            lexer.next();
            return igniv.new Multiply(left, term());
        } else if(has(DIVIDE)){
            //division
            lexer.next();
            return igniv.new Divide(left, term());
        }

        return left;
    }


    /**
     * Parse factor
     */
    private ParseTree factor()
    {
        return factor2(exponent());
    }


    /** 
     * Parse factor2
     */
    private ParseTree factor2(ParseTree left) 
    {
        if(has(EXPONENT)) {
            //exponent
            lexer.next();
            return igniv.new Exponent(left, factor());
        }

        return left;
    }


    /** 
     * Parse Exponent 
     */
    private ParseTree exponent()
    {
        if(has(LPAREN)) {
            //consume (
            lexer.next();

            //consume expr
            ParseTree result = expr();

            //consume )
            mustBe(RPAREN);
            lexer.next();

            return result;
        }
        else if(has(LCURLY)) {
            return igniv.new Literal(EvalType.ARRAY, list());
        }
        else if(has(REF)) {
            return refOrCall();
        }
        else if(has(MINUS)) {
            lexer.next();
            if (has(INTEGER)) {
                ParseTree result = igniv.new Literal(EvalType.INTEGER, (int)lexer.value() * -1);
                lexer.next();
                return result;
            }
            else {
                mustBe(FLOAT);
                ParseTree result = igniv.new Literal(EvalType.FLOAT, (double)lexer.value() * -1);
                lexer.next();
                return result;
            }
        }
        else if(has(STRING)) {
            ParseTree result = igniv.new Literal(EvalType.STRING, lexer.value());
            lexer.next();
            return result;
        }
        else if(has(CHARLIT)) {
            ParseTree result = igniv.new Literal(EvalType.CHARLIT, lexer.value());
            lexer.next();
            return result;
        }
        else if(has(INTEGER)) {
            // integer literal
            ParseTree result = igniv.new Literal(EvalType.INTEGER, lexer.value());
            lexer.next();
            return result;
        }
        else {
            // float literal is all that is left
            mustBe(FLOAT);
            ParseTree result = igniv.new Literal(EvalType.FLOAT, lexer.value());
            lexer.next();
            return result;
        }
    }
    
    private ArrayList<EvalResult> list() {
        mustBe(LCURLY);
        lexer.next();
        
        //argument list
        boolean done = false;
        ArrayList<EvalResult> ls = new ArrayList<>();
        
        if (!has(RCURLY)) {
            while(!done) {
                ls.add(expr().eval());
                if(has(COMMA)) {
                    lexer.next();
                } else {
                    done = true;
                    mustBe(RCURLY);
                }
            }
        }

        // consume RCURLY
        lexer.next();
        
        return ls;
    }

    /**
     * Parse a Ref or Call
     */
    private ParseTree refOrCall() {
        //capture the reference
        String name = lexer.string();

        //consume the reference
        lexer.next();
        
        if (has(LBRACKET)) {
            lexer.next();
            
            ParseTree index = expr();
            mustBe(RBRACKET);
            lexer.next();
            
            return igniv.new VarRef(name, index);
        }
        else if(has(LPAREN)) {
            lexer.next();
            return funCallRHS(name);
        } else {
            return igniv.new VarRef(name);
        }
    }


    /**
     * Parse a function call RHS
     */
    private ParseTree funCallRHS(String ref) {
        //argument list
        boolean done = false;

        IgniVProgram.FunCall fc = igniv.new FunCall(ref);
        
        if (!has(RPAREN)) {
            while(!done) {
                fc.addVal(expr());
                if(has(COMMA)) {
                    lexer.next();
                } else {
                    done = true;
                    mustBe(RPAREN);
                }
            }
        }

        // consume RPAREN
        lexer.next();

        return fc;
    }

    /** 
     * Parse print.
     */
    private ParseTree print() {
        IgniVProgram.Print p = igniv.new Print(false);

        // consume print
        mustBe(PRINT);
        lexer.next();
        
        mustBe(LPAREN);
        lexer.next();

        argList(p);
        
        mustBe(RPAREN);
        lexer.next();

        return p;
    }
    
    /**
     * Parse a println.
     */
    private ParseTree println() {
        IgniVProgram.Print p = igniv.new Print(true);

        // consume print
        mustBe(PRINTLN);
        lexer.next();
        
        mustBe(LPAREN);
        lexer.next();

        argList(p);
        
        mustBe(RPAREN);
        lexer.next();

        return p;
    }
    
    /**
     * Parse read.
     */
    private ParseTree read() {
        IgniVProgram.Read r = igniv.new Read();
        
        // consume read
        mustBe(READ);
        lexer.next();
        
        mustBe(LPAREN);
        lexer.next();
        
        refList(r);
        
        mustBe(RPAREN);
        lexer.next();
        
        return r;
    }


    /** 
     * Parse arg-list.
     */
    private void argList(IgniVProgram.Print p) {
        p.add(arg());
        argList2(p);
    }

   
    /**
     * Parse arg-list'
     */
    private void argList2(IgniVProgram.Print p) {
        if(has(COMMA)) {
            lexer.next();
            p.add(arg());
            argList2(p);
        }
    }

    /** 
     * Parse arg
     */
    private ParseTree arg() {
        return expr();
    }
    
    /** 
     * Parse ref-list.
     */
    private void refList(IgniVProgram.Read r) {
        r.add(ref());
        refList2(r);
    }

   
    /**
     * Parse ref-list'
     */
    private void refList2(IgniVProgram.Read r) {
        if(has(COMMA)) {
            lexer.next();
            r.add(ref());
            refList2(r);
        }
    }
    
    /** 
     * Parse ref
     */
    private String ref() {
        //capture the reference
        String name = lexer.string();

        //consume the reference
        lexer.next();
        
        return name;
    }


    /**
     * Only returns if the current token is the specified token.
     * Terminates the program otherwise.
     */
    private void mustBe(int token) {
        if(!has(token)) {
            if (lexer.tokenLabel() == "INVALID") {
                System.out.printf("Lexer Error on Line: %d, Column: %d\n", lexer.line(), lexer.column());
                
                lexer.skipLine();
                System.out.print(lexer.fullLine());
                System.out.printf("%" + (lexer.column()) + "s", "^\n");
                System.out.println("Invalid token!");
                
                System.exit(-1);
            }
            else {
                System.out.printf("Parse Error on Line: %d, Column: %d\n", lexer.line(), lexer.column());
                
                lexer.skipLine();
                System.out.print(lexer.fullLine());
                System.out.printf("%" + (lexer.column()) + "s", "^\n");
                
                System.out.printf("Expected: %s\n", lexer.tokenLabel(token));
                System.out.printf("Got: %s\n", lexer.tokenLabel());
                System.exit(-1);
            }
        }
    }
    
    /// Return true of the current token is a literal
    private boolean isLiteral() {
        if (has(INTEGER) || has(CHARLIT) || has(STRING)|| has(FLOAT)) {
            return true;
        }
        else {
            return false;
        }
    }


    /**
     * Return true if the current token is the specified token.
     */
    private boolean has(int token) {
        return lexer.token() == token;
    }


    public static void main(String [] args) throws FileNotFoundException {
//        InputStream testin = new FileInputStream("hello.igv");
//        
//        IgniVParser parser = new IgniVParser(new IgniVLexer(testin));
        IgniVParser parser = new IgniVParser(new IgniVLexer(System.in));
        parser.parse();
        System.out.println("Success!");
    }
}
