/*
 * Created on Nov 5, 2003
 *
 */
 
package ohd.hseb.model;

/**
 * @author Chip Gobs
 */
public class RainfallRunoffModelType
{
    
    public static RainfallRunoffModelType SAC_SMA = new RainfallRunoffModelType("SAC-SMA");
    public static RainfallRunoffModelType API_MKC = new RainfallRunoffModelType("API-MKC");
   
    private static final RainfallRunoffModelType[] _modelTypeArray = { API_MKC, SAC_SMA }; 
    
    private String _name;
    
    private RainfallRunoffModelType(String name)
    {
        _name = name;   
    }
    
    /**
     * This method returns a RainfallRunoffModelType, based on a name lookup.  If no match is found, then the passed in
     * default type is returned.  The default type can be null if desired.
     * @param name The name of the RainfallRunoffModelType
     * @return
     */
    public static RainfallRunoffModelType getRainfallRunoffModelTypeByName(String name, RainfallRunoffModelType defaultType)
    {
        RainfallRunoffModelType returnType = defaultType;
        
        for (RainfallRunoffModelType type : _modelTypeArray)
        {
            if (name.equalsIgnoreCase(type.getName()))
            {
                returnType = type;
            }
        }
        
        return returnType;
    }
    
    /**
     * This method returns a RainfallRunoffModelType, based on a name lookup.  If no match is found, then the
     * default type of API_MKC is returned.
     * @param name The name of the RainfallRunoffModelType
     * @return
     */
    public static RainfallRunoffModelType getRainfallRunoffModelTypeByName(String name)
    {
        return  getRainfallRunoffModelTypeByName(name, RainfallRunoffModelType.API_MKC);
    }
    
    
    
    public static  RainfallRunoffModelType[] getModelTypeArray()
    {
        return _modelTypeArray;
    }
    
    public String getName()
    {
        return _name;   
    }
    
    public String toString()
    {
        return _name;   
    }
    
}
