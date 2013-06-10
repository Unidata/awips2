package ohd.hseb.timeserieslite;


import java.util.HashMap;
import java.util.Map;

public class PDCDataType
{
    private static Map _dataTypeMap = new HashMap();
    
    
    public final static PDCDataType HEIGHT =  new PDCDataType("Height"); 
    public final static PDCDataType DISCHARGE = new PDCDataType("Discharge"); 
    public final static PDCDataType LAKE_STORAGE = new PDCDataType("Lake Storage"); 
    
    public final static PDCDataType PRECIP_TOTAL = new PDCDataType("Precip Totals"); 
    public final static PDCDataType PRECIP_PC = new PDCDataType("Precip PC"); 
    
    public final static PDCDataType SNOW_DEPTH =  new PDCDataType("Snow Depth"); 
    public final static PDCDataType SNOW_WATER_EQUIVALENT =  new PDCDataType("Snow Water Equivalent"); 
    
    public final static PDCDataType TEMPERATURE = new PDCDataType("Temperature"); 
    public final static PDCDataType DEWPOINT = new PDCDataType("Dewpoint");
    
    public final static PDCDataType RELATIVE_HUMIDITY = new PDCDataType("Relative Humidity"); 
    
    public final static PDCDataType WIND_SPEED = new PDCDataType("Wind Speed"); 
    public final static PDCDataType WIND_DIRECTION = new PDCDataType("Wind Direction"); 
    
   
    //----------------------------------------------------------------------------
    
    private String _name;
          
    //----------------------------------------------------------------------------
    private PDCDataType()
    {
        
    }
  
    //----------------------------------------------------------------------------
    private PDCDataType(String name)
    {
       _name = name;  
       
       if (_dataTypeMap == null)
       {
          _dataTypeMap = new HashMap();    
       }
       
       _dataTypeMap.put(name.toLowerCase(), this);
       
       return;
    } 
    //----------------------------------------------------------------------------
    
    public String getName()
    {     
        return _name;    
    }
  
    //----------------------------------------------------------------------------
    
    
    public static PDCDataType getDataTypeFromString(String name)
    {
        PDCDataType dataType = (PDCDataType) _dataTypeMap.get(name.toLowerCase());
     
        return dataType;
    }
    
    //  ----------------------------------------------------------------------------
    
    
    public String toString()
    {
          return _name;
    }

}
