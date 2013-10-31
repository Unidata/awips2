/*
 * Created on Sep 3, 2004
 *
 * 
 */
package ohd.hseb.model;

/**
 * @author GobsC
 *
 * This follows the java enum pattern with private constructors and public static final instances.
 */
public class ForecastInterpolationMethod
{
    private String _methodName = null;
    
    public static final ForecastInterpolationMethod DIFFERENCE = new ForecastInterpolationMethod("DIFFERENCE");
    public static final ForecastInterpolationMethod RATIO = new ForecastInterpolationMethod("RATIO");
    
    
    private static String[] _methodNameArray = { "DIFF", "DIFFERENCE", "RATIO" }; 

    private static ForecastInterpolationMethod[] _methodArray = { DIFFERENCE, DIFFERENCE, RATIO }; 
    
    // -------------------------------------------------------------------------------------
    
    private ForecastInterpolationMethod(String methodName)
    {
        _methodName = methodName;
        
        return;
    }
    
    // -------------------------------------------------------------------------------------
    public static ForecastInterpolationMethod getMethodByName(String methodName)
    {
        ForecastInterpolationMethod method = DIFFERENCE; //this is the default
        
        for (int i = 0; i < _methodNameArray.length; i++ )
        {
            if (_methodNameArray[i].equalsIgnoreCase(methodName))
            {
                method = _methodArray[i];
                break;
            }
        }
     
        return method;
    }
    // -------------------------------------------------------------------------------------
    
    public String getName()
    {
        return _methodName;
        
    }
    // -------------------------------------------------------------------------------------
    public String toString()
    {
        String outString = null;
        
        outString = "methodName = " + getName();
        
        return outString;
        
    }
}
