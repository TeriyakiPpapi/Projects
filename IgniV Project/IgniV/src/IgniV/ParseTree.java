package IgniV;


public interface ParseTree
{
    /**
     * Execute the parse tree.
     */
    public EvalResult eval();
    
    /**
     * Return statement's signifying token.
     */
    public int token();
}
