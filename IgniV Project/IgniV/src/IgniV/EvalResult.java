package IgniV;

import java.util.ArrayList;

/**
 * A simple object containing a type and value.
 */
public class EvalResult
{
    public EvalType type;
    public Object value;
    
    public EvalResult() {
    }

    public EvalResult(EvalType type, Object value) {
        this.type = type;
        this.value = value;
    }
    
    @Override
    public Object clone() {
        if (this.value.getClass() == Integer.class) {
            return new EvalResult(this.type, new Integer((Integer)this.value));
        }
        else if (this.value.getClass() == Double.class) {
            return new EvalResult(this.type, new Double((Double)this.value));
        }
        else if (this.value.getClass() == Character.class) {
                return new EvalResult(this.type, new Character((Character)this.value));
        }
        else if (this.value.getClass() == String.class) {
            return new EvalResult(this.type, new String((String)this.value));
        }
        else { // is array
            return new EvalResult(this.type, this.value);
        }
        
        
    }
//    @Override
//    public Object clone(){
//        try{
//            return (EvalResult) super.clone();
//        }catch (CloneNotSupportedException e){
//            return new EvalResult(this.type, this.value);
//        }
//    }
}
