package ohd.hseb.monitor;

import java.util.HashMap;
import java.util.Map;

public class MonitorOperator
{
    
    // static stuff -----------------------------------------------------------------
    private static final int GT = 0;
    private static final int GTE = 1;
    private static final int EQ = 2;
    private static final int LT = 3;
    private static final int LTE = 4;
  
    public static final MonitorOperator GREATER_THAN = new MonitorOperator(GT);
    public static final MonitorOperator GREATER_THAN_OR_EQUAL = new MonitorOperator(GTE);
    public static final MonitorOperator EQUAL = new MonitorOperator(EQ);
    public static final MonitorOperator LESS_THAN = new MonitorOperator(LT);
    public static final MonitorOperator LESS_THAN_OR_EQUAL = new MonitorOperator(LTE);
    
    private static final Map<Integer, String> _operatorSymbolMap = new HashMap<Integer, String>() ;
    
    static
    {
        _operatorSymbolMap.put(GT, ">");
        _operatorSymbolMap.put(GTE, ">=");
        _operatorSymbolMap.put(EQ, "==");
        _operatorSymbolMap.put(LT, "<");
        _operatorSymbolMap.put(LTE, "<=");
    }
  
    // ----------------------------------------------------------------------------
    
    private String _name = null;
    private int _typeCode;
 
    // ----------------------------------------------------------------------------
    private MonitorOperator(int typeCode)
    {
        _typeCode = typeCode;
    }
    
    // ----------------------------------------------------------------------------
    public String getSymbol()
    {
        return _operatorSymbolMap.get(_typeCode);
    }
    
    // ----------------------------------------------------------------------------

    public boolean evaluate(long value1, long value2)
    {
        boolean result = false;
        
        switch(_typeCode)
        {
            case GT:
            {
                result = (value1 > value2);
                break;
            }
            
            case GTE:
            {
                result = (value1 >= value2);
                break;
            }
            
            case EQ:
            {
                result = (value1 == value2);
                break;
            }
            
            case LT:
            {
                result = (value1 < value2);
                break;
            }
            
            case LTE:
            {
                result = (value1 <= value2);
                break;
            }
            default:
            {
                throw new Error("Invalid type code");
            }
            
        }
        
        return result;
    }
    
    // ----------------------------------------------------------------------------

   
    // ----------------------------------------------------------------------------
    
    
}
