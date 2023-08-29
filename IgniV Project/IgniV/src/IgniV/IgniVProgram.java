package IgniV;
/**
 * A parse-tree representation of a IgniV program.
 */
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Scanner;

class IgniVProgram implements ParseTree
{
    /** symbol table of the program */
    private Map<String, EvalResult> symtab;     

    /** enclosing program */
    private IgniVProgram parent;

    /** the statements of the program */
    private ArrayList<ParseTree> stmntList; 

    /** a handy variable for return void responses */
    private EvalResult voidResult;

    /** construct the program */
    public IgniVProgram(IgniVProgram parent)
    {
        symtab = new HashMap<String, EvalResult>();
        stmntList = new ArrayList<ParseTree>();
        voidResult = new EvalResult();
        voidResult.type = EvalType.VOID;
        voidResult.value = null;
        this.parent = parent;
    }


    /** no-arg constructor */
    public IgniVProgram()
    {
        this(null);
    }


    /** Add a statement to the program. */
    public void add(ParseTree stmnt) {
        stmntList.add(stmnt);
    }


    /** Search my program scope for a variable */
    public EvalResult scopeSearch(String name) {
        // search our table 
        EvalResult result = symtab.get(name);
    
        // search parents if not local
        if(result == null && parent != null) {
            result = parent.scopeSearch(name);
        }

        return result;
    }


    /** 
     * Get the number size of the program.
     * @return The number of statements in the program.
     */
    public int size() {
        return stmntList.size();
    }


    /** 
     * Evaluate the program.
     * @return A the result of the final expression.
     */
    @Override
    public EvalResult eval()
    {
        EvalResult result = voidResult;

        // evaluate each statement
        for(int i=0; i<size(); i++) {
            result = stmntList.get(i).eval();
            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                break;
            }
        }

        return result;
    }
    
    /**
     * Returns PROGRAM operator int.
     */
    @Override
    public int token() {
        return IgniVTokens.PROGRAM;
    }



    /**
     * This inner class represents a variable reference.
     */
    public class VarRef extends Expr {
        public String name;
        public ParseTree index;

        /** 
         * Construct the variable reference from its name.
         */
        public VarRef(String name) {
            this.name = name;
        }
        
        public VarRef(String name, ParseTree index) {
            this.name = name;
            this.index = index;
        }


        /**
         * Assign a value to the variable.
         */
        public void assign(EvalResult value) {
            // see if the variable exsts
            EvalResult v = scopeSearch(name);

            // add the variable to the symbol table if it is new
            if(v == null) {
                symtab.put(name, value);
            } 
            else {
                if(v.type == value.type) {
                    EvalResult clone = (EvalResult) value.clone();
                    
                    v.value = clone.value;
                }
                else {
                    System.out.println("Error! Type assignment is incorrect.");
                    System.exit(-1);
                }
            }
        }

        
        public void assign(EvalResult value, ParseTree index) {
            // see if the variable exsts
            EvalResult v = scopeSearch(name);
            int i = (int) index.eval().value;

            // add the variable to the symbol table if it is new
            if(v == null) {
                symtab.put(name, value);
            } 
            else {
                if ( ((ArrayList<EvalResult>)v.value).size() <= i) {
                    if( ((ArrayList<EvalResult>)v.value).get(0).type == value.type) {
                        EvalResult clone = (EvalResult) value.clone();
                        ((ArrayList<EvalResult>)v.value).add(clone);
                    }
                    else {
                        System.out.println("Error! Type assignment is incorrect.");
                        System.exit(-1);
                    }
                }
                else if( ((ArrayList<EvalResult>)v.value).get(i).type == value.type) {
                    ((ArrayList<EvalResult>)v.value).remove(i);
                    EvalResult clone = (EvalResult) value.clone();
                    ((ArrayList<EvalResult>)v.value).add(i, clone);
                }
                else {
                    System.out.println("Error! Type assignment is incorrect.");
                    System.exit(-1);
                }
            }
        }


        /**
         * Evaluate a variable reference.
         * @return The value stored in the variable.
         */
        @Override
        public EvalResult eval() {
            if (index != null) {
                EvalResult array = scopeSearch(name);
                return ((ArrayList<EvalResult>)array.value).get((int) index.eval().value);
            }
            else {
                return scopeSearch(name);
            }
        }
        
        /**
         * Returns REF operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.REF;
        }
    }
    

    /**
     * This inner class represents an assignment statement.
     */
    public class Assign implements ParseTree {
        private VarRef variable;
        private Expr expr;
        private ParseTree index;

        /**
         * Construct the assignment from its parts 
         */
        public Assign(VarRef variable, Expr expr) {
            this.variable = variable;
            this.expr = expr;
        }
        
        public Assign(VarRef variable, ParseTree index, Expr expr) {
            this.variable = variable;
            this.expr = expr;
            this.index = index;
        }


        /**
         * Evaluate the assignment, and return a void response. 
         * @return A void result.
         */
        @Override
        public EvalResult eval() {
            if (index != null) {
                variable.assign((EvalResult)expr.eval().clone(), index);
            }
            else {
                variable.assign((EvalResult)expr.eval().clone());
            }

            return voidResult;
        }
        
        /**
         * Returns ASSIGN operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.ASSIGN;
        }
    }



    /**
     * This is an empty class to begin the hierarchy of expression types.
     */
    public abstract class Expr implements ParseTree {
        /* this space left intentionally blank */
    }



    /**
     * This is the next step in the expression tree, mainly contributing a type
     * system for binary operations.
     */
    public abstract class BinOp extends Expr {
        // left hand side of the binary operation
        protected EvalResult lhs; 

        // right hand side of the binary operation
        protected EvalResult rhs;

        // computes the return type of this binary operation
        protected EvalType returnType() {
            // we do the standard type widening
            if(lhs.type == EvalType.INTEGER && rhs.type == EvalType.INTEGER) {
                return EvalType.INTEGER;
            } else if(lhs.type == EvalType.FLOAT && rhs.type == EvalType.INTEGER) {
                return EvalType.FLOAT;
            } else if(lhs.type == EvalType.INTEGER && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            } else if(lhs.type == EvalType.FLOAT && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }

            // if we reach here, we have an invalid operand.
            return EvalType.VOID;
        }
    }



    /**
     * Peform an addition operation.
     */
    public class Add extends BinOp {
        ParseTree lop;
        ParseTree rop;

        /**
         * Construct an addition between the left and right operands.
         * @param lop Left operand
         * @param rop Right operand
         */
        public Add(ParseTree lop, ParseTree rop) {
            this.lop = lop;
            this.rop = rop;
        }
        
        // computes the return type of this binary operation
        @Override
        protected EvalType returnType() {
            // we do the standard type widening
            if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.INTEGER) {
                return EvalType.INTEGER;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.INTEGER) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.FLOAT && rhs.type == EvalType.INTEGER) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.FLOAT && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.STRING) {
                return EvalType.STRING;
            }
            else if (lhs.type == EvalType.STRING && rhs.type == EvalType.INTEGER) {
                return EvalType.STRING;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.STRING) {
                return EvalType.STRING;
            }
            else if (lhs.type == EvalType.STRING && rhs.type == EvalType.CHARLIT) {
                return EvalType.STRING;
            }
            else if (lhs.type == EvalType.STRING && rhs.type == EvalType.STRING) {
                return EvalType.STRING;
            }

            // if we reach here, we have an invalid operand.
            return EvalType.VOID;
        }

        /**
         * Evaluate the addition, returning the result.
         * @return The result of the addition operation.
         */
        @Override
        public EvalResult eval() {
            EvalResult result = new EvalResult();

            //compute the lhs and rhs of the operation
            lhs = lop.eval();
            rhs = rop.eval();

            // compute the result type
            result.type = returnType();

            // carry out a valid operation
            if(result.type == EvalType.INTEGER) {
                // get the operand values
                int x = ((Number)lhs.value).intValue();
                int y = ((Number)rhs.value).intValue();

                result.value = x + y;
            }
            else if(result.type == EvalType.FLOAT) {
                // get the operand values
                double x;
                double y;
                if (lhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)lhs.value);
                    x = i.doubleValue();
                }
                else {
                    x = (double) lhs.value;
                }
                
                if (rhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)rhs.value);
                    y = i.doubleValue();
                }
                else {
                    y = (double) rhs.value;
                }

                result.value = x + y;
            }
            else if(result.type == EvalType.CHARLIT) {
                // get the operand values
                char x;
                char y;
                if (lhs.value.getClass() == Integer.class) {
                    x = (char)(Integer.parseInt(lhs.value+""));
                }
                else {
                    x = (char) lhs.value;
                }
                
                if (rhs.value.getClass() == Integer.class) {
                    y = (char)(Integer.parseInt(rhs.value+""));
                }
                else {
                    y = (char) rhs.value;
                }

                result.value = (char)(x + y);
            }
            else if(result.type == EvalType.STRING) {
                // get the operand values
                String x = lhs.value.toString();
                String y = rhs.value.toString();
                
                result.value = x + y;
            }

            return result;
        }
        
        /**
         * Returns plus operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.PLUS;
        }
    }



    /**
     * Peform a subtraction operation.
     */
    public class Subtract extends BinOp {
        ParseTree lop;
        ParseTree rop;

        /**
         * Construct a subtraction between the left and right operands.
         * @param lop Left operand
         * @param rop Right operand
         */
        public Subtract (ParseTree lop, ParseTree rop) {
            this.lop = lop;
            this.rop = rop;
        }
        
        // computes the return type of this binary operation
        @Override
        protected EvalType returnType() {
            // we do the standard type widening
            if(lhs.type == EvalType.INTEGER && rhs.type == EvalType.INTEGER) {
                return EvalType.INTEGER;
            } else if(lhs.type == EvalType.FLOAT && rhs.type == EvalType.INTEGER) {
                return EvalType.FLOAT;
            } else if(lhs.type == EvalType.INTEGER && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            } else if(lhs.type == EvalType.FLOAT && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            } else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            } else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.INTEGER) {
                return EvalType.CHARLIT;
            } else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }

            // if we reach here, we have an invalid operand.
            return EvalType.VOID;
        }


        /**
         * Evaluate the subtraction, returning the result.
         * @return The result of the subtraction operation.
         */
        @Override
        public EvalResult eval() {
            EvalResult result = new EvalResult();

            //compute the lhs and rhs of the operation
            lhs = lop.eval();
            rhs = rop.eval();

            // compute the result type
            result.type = returnType();

            // carry out a valid operation
            if(result.type == EvalType.INTEGER) {
                //get the operand values
                int x = ((Number)lhs.value).intValue();
                int y = ((Number)rhs.value).intValue();

                result.value = x - y;
            }
            else if(result.type == EvalType.FLOAT) {
                // get the operand values
                double x;
                double y;
                if (lhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)lhs.value);
                    x = i.doubleValue();
                }
                else {
                    x = (double) lhs.value;
                }
                
                if (rhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)rhs.value);
                    y = i.doubleValue();
                }
                else {
                    y = (double) rhs.value;
                }

                result.value = x - y;
            }
            else if(result.type == EvalType.CHARLIT) {
                // get the operand values
                char x;
                char y;
                if (lhs.value.getClass() == Integer.class) {
                    x = (char)(Integer.parseInt(lhs.value+""));
                }
                else {
                    x = (char) lhs.value;
                }
                
                if (rhs.value.getClass() == Integer.class) {
                    y = (char)(Integer.parseInt(rhs.value+""));
                }
                else {
                    y = (char) rhs.value;
                }

                result.value = (char)(x - y);
            }

            return result;
        }
        
        /**
         * Returns minus operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.MINUS;
        }
    }



    /**
     * Peform a multiplication operation.
     */
    public class Multiply extends BinOp {
        ParseTree lop;
        ParseTree rop;

        /**
         * Construct a multiplication between the left and right operands.
         * @param lop Left operand
         * @param rop Right operand
         */
        public Multiply(ParseTree lop, ParseTree rop) {
            this.lop = lop;
            this.rop = rop;
        }
        
        // computes the return type of this binary operation
        @Override
        protected EvalType returnType() {
            // we do the standard type widening
            if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.INTEGER) {
                return EvalType.INTEGER;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.CHARLIT) {
                return EvalType.STRING;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.INTEGER) {
                return EvalType.STRING;
            }
            else if(lhs.type == EvalType.FLOAT && rhs.type == EvalType.INTEGER) {
                return EvalType.FLOAT;
            }
            else if(lhs.type == EvalType.INTEGER && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if(lhs.type == EvalType.FLOAT && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.STRING) {
                return EvalType.STRING;
            }
            else if (lhs.type == EvalType.STRING && rhs.type == EvalType.INTEGER) {
                return EvalType.STRING;
            }
            
            // if we reach here, we have an invalid operand.
            return EvalType.VOID;
        }

        /**
         * Evaluate the multiplication, returning the result.
         * @return The result of the multiplication operation.
         */
        @Override
        public EvalResult eval() {
            EvalResult result = new EvalResult();

            //compute the lhs and rhs of the operation
            lhs = lop.eval();
            rhs = rop.eval();

            // compute the result type
            result.type = returnType();

            // carry out a valid operation
            if (result.type == EvalType.INTEGER) {
                // get the operand values
                int x = (int) lhs.value;
                int y = (int) rhs.value;

                result.value = x * y;
            }
            else if(result.type == EvalType.FLOAT) {
                // get the operand values
                double x;
                double y;
                if (lhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)lhs.value);
                    x = i.doubleValue();
                }
                else {
                    x = (double) lhs.value;
                }
                
                if (rhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)rhs.value);
                    y = i.doubleValue();
                }
                else {
                    y = (double) rhs.value;
                }

                result.value = x * y;
            }
            else if (result.type == EvalType.STRING) {
                if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.CHARLIT) {
                    // get the operand values
                    int x = (int) lhs.value;
                    char y = (char) rhs.value;
                    
                    StringBuilder output = new StringBuilder();
                    for (int i = 0; i < x; i++) {
                        output.append(y);
                    }
                    
                    result.value = output.toString();
                }
                else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.INTEGER) {
                    // get the operand values
                    char x = (char) lhs.value;
                    int y = (int) rhs.value;
                    
                    StringBuilder output = new StringBuilder();
                    for (int i = 0; i < y; i++) {
                        output.append(x);
                    }
                    
                    result.value = output.toString();
                }
                else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.STRING) {
                    // get the operand values
                    int x = (int) lhs.value;
                    String y = rhs.value.toString();
                    
                    StringBuilder output = new StringBuilder();
                    for (int i = 0; i < x; i++) {
                        output.append(y);
                    }
                    
                    result.value = output.toString();
                }
                else if (lhs.type == EvalType.STRING && rhs.type == EvalType.INTEGER) {
                    // get the operand values
                    String x = lhs.value.toString();
                    int y = (int) rhs.value;
                    
                    StringBuilder output = new StringBuilder();
                    for (int i = 0; i < y; i++) {
                        output.append(x);
                    }
                    
                    result.value = output.toString();
                }
            }

            return result;
        }
        
        /**
         * Returns TIMES operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.TIMES;
        }
    }



    /**
     * Peform a division operation.
     */
    public class Divide extends BinOp {
        ParseTree lop;
        ParseTree rop;

        /**
         * Construct a division between the left and right operands.
         * @param lop Left operand
         * @param rop Right operand
         */
        public Divide(ParseTree lop, ParseTree rop) {
            this.lop = lop;
            this.rop = rop;
        }


        /**
         * Evaluate the division, returning the result.
         * @return The result of the division operation.
         */
        @Override
        public EvalResult eval() {
            EvalResult result = new EvalResult();

            //compute the lhs and rhs of the operation
            lhs = lop.eval();
            rhs = rop.eval();

            // compute the result type
            result.type = returnType();

            // carry out a valid operation
            if(result.type == EvalType.INTEGER) {
                //get the operand values
                int x = (int) lhs.value;
                int y = (int) rhs.value;

                result.value = x / y;
            }
            else if(result.type == EvalType.FLOAT) {
                // get the operand values
                double x;
                double y;
                if (lhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)lhs.value);
                    x = i.doubleValue();
                }
                else {
                    x = (double) lhs.value;
                }
                
                if (rhs.type == EvalType.INTEGER) {
                    Integer i = new Integer((int)rhs.value);
                    y = i.doubleValue();
                }
                else {
                    y = (double) rhs.value;
                }

                result.value = x / y;
            }

            return result;
        }
        
        /**
         * Returns DIVIDE operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.DIVIDE;
        }
    }



    /**
     * Peform an exponent operation.
     */
    public class Exponent extends BinOp {
        ParseTree lop;
        ParseTree rop;

        /**
         * Construct an exponent between the left and right operands.
         * @param lop Left operand
         * @param rop Right operand
         */
        public Exponent(ParseTree lop, ParseTree rop) {
            this.lop = lop;
            this.rop = rop;
        }


        /**
         * Evaluate the exponent, returning the result.
         * @return The result of the exponent operation.
         */
        @Override
        public EvalResult eval() {
            EvalResult result = new EvalResult();

            //compute the lhs and rhs of the operation
            lhs = lop.eval();
            rhs = rop.eval();

            // compute the result type
            result.type = returnType();

            // carry out a valid operation
            if(result.type == EvalType.INTEGER) {
                //get the operand values
                int x = (int) lhs.value;
                int y = (int) rhs.value;

                result.value = (int) Math.pow(x,y);
            } else if(result.type == EvalType.FLOAT) {
                //get the operand values
                double x = ((Number)lhs.value).doubleValue();
                double y = ((Number)rhs.value).doubleValue();

                result.value = Math.pow(x,y);
            }

            return result;
        }
        
        /**
         * Returns EXPONENT operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.EXPONENT;
        }
    } 



    /**
     * Represent litetal values on the parse tree.
     */
    public class Literal extends Expr
    {
        private EvalResult value;

        /**
         * Construct the literal node.
         */
        public Literal(EvalType type, Object value) {
            this.value = new EvalResult();
            this.value.type=type;
            this.value.value=value;
        }


        /**
         * Return the literal value.
         * @return The literal value.
         */
        @Override
        public EvalResult eval() {
            return value;
        }
        
        /**
         * Returns LITERAL operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.LITERAL;
        }
    }



    /**
     * Execute a print statement.
     */
    public class Print implements ParseTree
    {
        ArrayList<ParseTree> args;
        boolean ln;

        /**
         * Construct the Print statement.
         */
        public Print(boolean ln)
        {
            args = new ArrayList<ParseTree>();
            this.ln = ln;
        }


        /**
         * Add an argument to the print's arg list.
         */
        public void add(ParseTree arg) {
            args.add(arg);
        }


        /**
         * Print the arglist.
         * @return VOID result.
         */
        @Override
        public EvalResult eval() {
            // print the argument list
            for(int i=0; i<args.size(); i++) {
                if (args.get(i).eval().type == EvalType.ARRAY) {
                    for (int j = 0; j < ((ArrayList)args.get(i).eval().value).size(); j++) {
                        System.out.print(((ArrayList<EvalResult>)args.get(i).eval().value).get(j).value + " ");
                    }
                }
                else {
                    System.out.print(args.get(i).eval().value);
                }
            }
            if (ln) {
                System.out.println();
            }

            return voidResult;
        }
        
        /**
         * Returns PRINT operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.PRINT;
        }
    }
    
    /**
     * Execute a read statement.
     */
    public class Read implements ParseTree {
        ArrayList<String> refs;
        
        /**
         * Construct the Print statement.
         */
        public Read() {
            refs = new ArrayList<String>();
        }
        
        /**
         * Add a reference to the read's ref list.
         */
        public void add(String ref) {
            refs.add(ref);
        }
        
        /**
         * Read the reflist.
         * @return VOID result.
         */
        @Override
        public EvalResult eval() {
            
            // read the reference list
            Scanner input = new Scanner(System.in);
            
            for(int i=0; i<refs.size(); i++) {
                // Create a ref for the variable
                IgniVProgram.VarRef var = new VarRef(refs.get(i));
                
                String line = input.nextLine();
                InputStream targetStream = new ByteArrayInputStream(line.getBytes());
                IgniVLexer in = new IgniVLexer(targetStream);
                int token = in.next();
                if (token == IgniVTokens.MINUS) {
                    token = in.next();
                    if (token == IgniVTokens.INTEGER) {
                        var.assign(new EvalResult(EvalType.INTEGER, Integer.parseInt(line)));
                    }
                    else if (token == IgniVTokens.FLOAT) {
                        var.assign(new EvalResult(EvalType.FLOAT, Double.parseDouble(line)));
                    }
                    else {
                        var.assign(new EvalResult(EvalType.STRING, (String) line));
                    }
                }
                else if (token == IgniVTokens.INTEGER) {
                    var.assign(new EvalResult(EvalType.INTEGER, Integer.parseInt(line)));
                }
                else if (token == IgniVTokens.FLOAT) {
                    var.assign(new EvalResult(EvalType.FLOAT, Double.parseDouble(line)));
                }
                else if (token == IgniVTokens.CHARLIT) {
                    var.assign(new EvalResult(EvalType.CHARLIT, (char) in.value()));
                }
                else {
                    var.assign(new EvalResult(EvalType.STRING, (String) line));
                }
            }
            
            return voidResult;
        }
        
        /**
         * Returns READtoken int.
         */
        @Override
        public int token() {
            return IgniVTokens.READ;
        }
    }


    /** 
     * Evaluate a function definition.
     */
    public class FunDef implements ParseTree
    {
        String name;
        ArrayList<String> parameters;
        IgniVProgram body;

        /** construct a function **/
        public FunDef(String name) {
            this.name = name;
            parameters = new ArrayList<String>();
        }


        /** Add a parameter */
        public void addParameter(String name) {
            // take a lax attitude toward duplicate names in the
            // interest of time.
            parameters.add(name);
        }


        /** Set the body */
        public void setBody(IgniVProgram body) {
            this.body = body;
        }


        /** evaluate */
        @Override
        public EvalResult eval() {
            EvalResult result = new EvalResult();
            result.type = EvalType.FUNCTION;
            result.value = this;
            symtab.put(name, result);

            return voidResult;
        }
        
        /**
         * Returns FUNCTION operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.FUNCTION;
        }


        /** Call the function */
        public EvalResult call(ArrayList<EvalResult> args) {
            // check the parameter size
            if(args.size() != parameters.size()) {
                System.out.println("Error: Invalid parameter list for " + name);
                return voidResult;
            }

            // handle the symbol table in a way that would
            // not work with recursion!
            for(int i=0; i<args.size(); i++) {
                body.symtab.put(parameters.get(i), (EvalResult) args.get(i).clone());
            }

            // run the function
            return body.eval();
        }
    }


    /** 
     * Evaluate a function call.
     */
    public class FunCall extends Expr
    {
        ArrayList<ParseTree> args;
        String name;

        public FunCall(String name) {
            this.name = name;
            args = new ArrayList<ParseTree>();
        }

        public void addVal(ParseTree e) {
            args.add(e);
        }

        @Override
        public EvalResult eval() {
            // find the function
            FunDef f = (FunDef) scopeSearch(name).value;

            if(f == null) {
                System.out.println("Undefined Function: " + name);
            }

            // evaluate the argument expressions
            ArrayList<EvalResult> val = new ArrayList<EvalResult>();
            for(int i=0; i<args.size(); i++) {
                val.add((EvalResult)args.get(i).eval().clone());
            }

            // call the function
            return f.call(val);
        }
        
        /**
         * Returns FUNCTION operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.FUNCTION;
        }
    }
    
    /**
     * Returns the value from a return statement.
     */
    public class Return implements ParseTree {
        ParseTree eval;
        public Return(ParseTree eval) {
            this.eval = eval;
        }
        
        @Override
        public EvalResult eval() {
            return eval.eval();
        }
        
        /**
         * Returns RETURN operator int.
         */
        @Override
        public int token() {
            return IgniVTokens.RETURN;
        }
    }
    
    /**
     * Executes a while loop.
     */
    public class While extends BinOp {
        private ArrayList<ParseTree> stmntList;
        ParseTree lcon;
        ParseTree rcon;
        int operator;
        int token;
        
        public While(ParseTree lcon, ParseTree rcon, int operator) {
            stmntList = new ArrayList<>();
            this.lcon = lcon;
            this.rcon = rcon;
            this.operator = operator;
            this.token = IgniVTokens.WHILE;
        }
        
        /** Add a statement to the program. */
        public void add(ParseTree stmnt) {
            stmntList.add(stmnt);
        }
        
        /** 
        * Get the number size of the program.
        * @return The number of statements in the program.
        */
        public int size() {
            return stmntList.size();
        }
        
        // computes the return type of this binary operation
        @Override
        protected EvalType returnType() {
            // we do the standard type widening
            if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.INTEGER) {
                return EvalType.INTEGER;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.INTEGER) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.FLOAT && rhs.type == EvalType.INTEGER) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.FLOAT && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }

            // if we reach here, we have an invalid operand.
            return EvalType.VOID;
        }
        
        @Override
        public EvalResult eval() {
            boolean flag = true;
            
            lhs = lcon.eval();
            rhs = rcon.eval();
            
            if (returnType() == EvalType.INTEGER || returnType() == EvalType.FLOAT) {
                if (operator == IgniVTokens.ISEQUAL) {
                    while (( ((Number) lhs.value).doubleValue() == ((Number) rhs.value).doubleValue() ) && flag) {
                        for(int i=0; i<size(); i++) {
                            stmntList.get(i).eval();
                            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                flag = false;
                                break;
                            }
                        }
                        lhs = lcon.eval();
                        rhs = rcon.eval();
                    }
                }
                else if (operator == IgniVTokens.NOTEQUAL) {
                    while (( ((Number) lhs.value).doubleValue() != ((Number) rhs.value).doubleValue() ) && flag) {
                        for(int i=0; i<size(); i++) {
                            stmntList.get(i).eval();
                            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                flag = false;
                                break;
                            }
                        }
                        lhs = lcon.eval();
                        rhs = rcon.eval();
                    }
                }
                else if (operator == IgniVTokens.LESSTHAN) {
                    while (( ((Number) lhs.value).doubleValue() < ((Number) rhs.value).doubleValue() ) && flag) {
                        for(int i=0; i<size(); i++) {
                            stmntList.get(i).eval();
                            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                flag = false;
                                break;
                            }
                        }
                        lhs = lcon.eval();
                        rhs = rcon.eval();
                    }
                }
                else if (operator == IgniVTokens.LESSTHANEQ) {
                    while (( ((Number) lhs.value).doubleValue() <= ((Number) rhs.value).doubleValue() ) && flag) {
                        for(int i=0; i<size(); i++) {
                            stmntList.get(i).eval();
                            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                flag = false;
                                break;
                            }
                        }
                        lhs = lcon.eval();
                        rhs = rcon.eval();
                    }
                }
                else if (operator == IgniVTokens.GREATTHAN) {
                    while (( ((Number) lhs.value).doubleValue() > ((Number) rhs.value).doubleValue() ) && flag) {
                        for(int i=0; i<size(); i++) {
                            stmntList.get(i).eval();
                            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                flag = false;
                                break;
                            }
                        }
                        lhs = lcon.eval();
                        rhs = rcon.eval();
                    }
                }
                else if (operator == IgniVTokens.GREATTHANEQ) {
                    while (( ((Number) lhs.value).doubleValue() >= ((Number) rhs.value).doubleValue() ) && flag) {
                        for(int i=0; i<size(); i++) {
                            stmntList.get(i).eval();
                            if (stmntList.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                flag = false;
                                break;
                            }
                        }
                        lhs = lcon.eval();
                        rhs = rcon.eval();
                    }
                }
            }
            else {
                System.out.println("Error: type comparison not supported");
                System.exit(-8);
            }

            return voidResult;
        }
        
        /**
         * Returns WHILE or RETURN operator int.
         */
        @Override
        public int token() {
            return token;
        }
        
    }
    
    /**
     * Executes a If-Else block.
     */
    public class IfElse extends BinOp {
        private ArrayList<ParseTree> stmntListI;
        private ArrayList<ParseTree> stmntListE;
        ParseTree lcon;
        ParseTree rcon;
        int operator;
        int token;
        boolean hasElse;
        
        public IfElse(ParseTree lcon, ParseTree rcon, int operator) {
            stmntListI = new ArrayList<>();
            stmntListE = new ArrayList<>();
            this.lcon = lcon;
            this.rcon = rcon;
            this.operator = operator;
            this.token = IgniVTokens.IF;
            hasElse = false;
        }
        
        /** Add a statement to the if. */
        public void addI(ParseTree stmnt) {
            stmntListI.add(stmnt);
        }
        
        /** Add a statement to the if. */
        public void addE(ParseTree stmnt) {
            hasElse = true;
            stmntListE.add(stmnt);
        }
        
        /** 
        * Get the number size of the program.
        * @return The number of statements in the program.
        */
        public int sizeI() {
            return stmntListI.size();
        }
        
        /** 
        * Get the number size of the program.
        * @return The number of statements in the program.
        */
        public int sizeE() {
            return stmntListE.size();
        }
        
        // computes the return type of this binary operation
        @Override
        protected EvalType returnType() {
            // we do the standard type widening
            if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.INTEGER) {
                return EvalType.INTEGER;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.INTEGER) {
                return EvalType.CHARLIT;
            }
            else if (lhs.type == EvalType.INTEGER && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.FLOAT && rhs.type == EvalType.INTEGER) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.FLOAT && rhs.type == EvalType.FLOAT) {
                return EvalType.FLOAT;
            }
            else if (lhs.type == EvalType.CHARLIT && rhs.type == EvalType.CHARLIT) {
                return EvalType.CHARLIT;
            }

            // if we reach here, we have an invalid operand.
            return EvalType.VOID;
        }
        
        @Override
        public EvalResult eval() {
            
            lhs = lcon.eval();
            rhs = rcon.eval();
            
            if (returnType() == EvalType.INTEGER || returnType() == EvalType.FLOAT) {
                if (operator == IgniVTokens.ISEQUAL) {
                    if ( ((Number) lhs.value).doubleValue() == ((Number) rhs.value).doubleValue() ) {
                        for(int i=0; i<sizeI(); i++) {
                            stmntListI.get(i).eval();
                            if (stmntListI.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                    else if(hasElse) {
                        for(int i=0; i<sizeE(); i++) {
                            stmntListE.get(i).eval();
                            if (stmntListE.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                }
                else if (operator == IgniVTokens.NOTEQUAL) {
                    if ( ((Number) lhs.value).doubleValue() != ((Number) rhs.value).doubleValue() ) {
                        for(int i=0; i<sizeI(); i++) {
                            stmntListI.get(i).eval();
                            if (stmntListI.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                    else if(hasElse) {
                        for(int i=0; i<sizeE(); i++) {
                            stmntListE.get(i).eval();
                            if (stmntListE.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                }
                else if (operator == IgniVTokens.LESSTHAN) {
                    if ( ((Number) lhs.value).doubleValue() < ((Number) rhs.value).doubleValue() ) {
                        for(int i=0; i<sizeI(); i++) {
                            stmntListI.get(i).eval();
                            if (stmntListI.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                    else if(hasElse) {
                        for(int i=0; i<sizeE(); i++) {
                            stmntListE.get(i).eval();
                            if (stmntListE.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                }
                else if (operator == IgniVTokens.LESSTHANEQ) {
                    if ( ((Number) lhs.value).doubleValue() <= ((Number) rhs.value).doubleValue() ) {
                        for(int i=0; i<sizeI(); i++) {
                            stmntListI.get(i).eval();
                            if (stmntListI.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                    else if(hasElse) {
                        for(int i=0; i<sizeE(); i++) {
                            stmntListE.get(i).eval();
                            if (stmntListE.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                }
                else if (operator == IgniVTokens.GREATTHAN) {
                    if ( ((Number) lhs.value).doubleValue() > ((Number) rhs.value).doubleValue() ) {
                        for(int i=0; i<sizeI(); i++) {
                            stmntListI.get(i).eval();
                            if (stmntListI.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                    else if(hasElse) {
                        for(int i=0; i<sizeE(); i++) {
                            stmntListE.get(i).eval();
                            if (stmntListE.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                }
                else if (operator == IgniVTokens.GREATTHANEQ) {
                    if ( ((Number) lhs.value).doubleValue() >= ((Number) rhs.value).doubleValue() ) {
                        for(int i=0; i<sizeI(); i++) {
                            stmntListI.get(i).eval();
                            if (stmntListI.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                    else if(hasElse) {
                        for(int i=0; i<sizeE(); i++) {
                            stmntListE.get(i).eval();
                            if (stmntListE.get(i).token() == IgniVTokens.RETURN) {
                                token = IgniVTokens.RETURN;
                                break;
                            }
                        }
                    }
                }
            }
            else {
                System.out.println("Error: type comparison not supported");
                System.exit(-8);
            }

            return voidResult;
        }
        
        /**
         * Returns IF or RETURN operator int.
         */
        @Override
        public int token() {
            return token;
        }
        
    }

}
