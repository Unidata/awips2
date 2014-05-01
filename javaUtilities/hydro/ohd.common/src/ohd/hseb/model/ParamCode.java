package ohd.hseb.model;

import java.util.HashMap;
import java.util.Map;

public class ParamCode
{
    private String _paramCodeString = null;
    
    private static final Map _shefDurCodeToIhfsDurCodeMap = new HashMap();
    private static final Map _ihfsDurCodeToShefDurCodeMap = new HashMap();
    
    private String _pe = null;
    private String _typeSource = null;
    private int _ihfsDur = 0;
    private String _shefDur = null;
    private String _extremum = null;
    
    static
    {
        initDurationCodeMaps();
    }
    
    // -------------------------------------------------------------------------------
    public ParamCode(String paramCodeString)
    {
        _paramCodeString = paramCodeString;
        parse(paramCodeString);
        
        return;
    }
    // -------------------------------------------------------------------------------
    
    public ParamCode(ParamCode paramCode)
    {
        _pe = paramCode.getPe();
        _typeSource = paramCode.getTypeSource();
        _ihfsDur = paramCode.getIhfsDur();
       _shefDur = paramCode.getShefDur();
        _extremum = paramCode.getExtremum();
        _paramCodeString = paramCode._paramCodeString;
     
        
        return;
    }
    //  -------------------------------------------------------------------------------
    //  -------------------------------------------------------------------------------
    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append(getPe() + 
                      getShefDur() +
                      getTypeSource() +
                      getExtremum() );
        
        Runtime rt = Runtime.getRuntime();
        
        return buffer.toString();
      
        
    }

    //  -------------------------------------------------------------------------------
    
    public String toStringVerbose()
    {
        StringBuffer buffer = new StringBuffer();
        
        
        buffer.append("pe = " + getPe() +
                      " dur = " + getIhfsDur() +
                      " ts = " + getTypeSource() +      
                      " extremum = " + getExtremum() );
        
        return buffer.toString();
        
    }
    //  -------------------------------------------------------------------------------
    
    private void parse(String paramCodeString)
    {
       
        setPe(paramCodeString.substring(0,2));
        _shefDur = paramCodeString.substring(2,3);
        
        _ihfsDur = getIhfsDurationCode(_shefDur);
        
        setTypeSource(paramCodeString.substring(3,5));
        setExtremum(paramCodeString.substring(5,6));
        
        return;
    }
    //  -------------------------------------------------------------------------------
    private static void initDurationCodeMaps()
    {
        
        /*
      0 | I       | Instantaneous
        1 | U       | 1 Minute
       15 | C       | 15 Minutes
       30 | J       | 30 Minutes
     1001 | H       | 1 Hour
     1002 | B       | 2 Hour
     1003 | T       | 3 Hour
     1004 | F       | 4 Hour
     1006 | Q       | 6 Hour
     1008 | A       | 8 Hour
     1012 | K       | 12 Hour
     1018 | L       | 18 Hour
     2001 | D       | 1 Day
     2007 | W       | 1 Week
     3001 | M       | 1 Month
     4001 | Y       | 1 Year
     5000 | Z       | Unspecified
     5001 | S       | Seasonal
     5002 | R       | Period of Record
     5004 | P       | Total Since 7 AM
     5005 | X       | Unknown
        
    */  
        
        
        String code = "?";
        
        String charCodeString = "IUCJHBTFQAKLDWMYZSRPX";
        
        int intCodeArray[] = { 0, 1, 15, 30,
                   1001, 1002, 1003, 1004, 1006, 1008, 1012, 1018, 
                   2001, 2007,
                   3001,
                   4001,
                   5000, 5001, 5002, 5004, 5005  };
        
                   
                  
        int i = 0;
        
        for (i = 0; i < charCodeString.length(); i++)
        {
            String shefCode = charCodeString.substring(i, i+1);
            Integer intCode = new Integer(intCodeArray[i]);
            
            _shefDurCodeToIhfsDurCodeMap.put(shefCode, intCode);
            _ihfsDurCodeToShefDurCodeMap.put(intCode, shefCode);
        }
        
        return;

    }
    //  -------------------------------------------------------------------------------
    public static int getIhfsDurationCode(String shefDurationCode)
    {
       int ihfsCode = -1;
       Integer ihfsCodeInteger = (Integer) _shefDurCodeToIhfsDurCodeMap.get(shefDurationCode);   
        
       if (ihfsCodeInteger != null)
       {
           ihfsCode = ihfsCodeInteger.intValue();
       }
       
       return ihfsCode;
    }
    //  -------------------------------------------------------------------------------
    public static String getShefDurationCode(int ihfsDurationCode)
    {
       String shefDurationCode = "?";
       
       Integer ihfsDurationCodeInteger = new Integer(ihfsDurationCode);
       
       String shefDurationCodeFromMap =  
               (String) _ihfsDurCodeToShefDurCodeMap.get(ihfsDurationCodeInteger);  
        
       if (shefDurationCodeFromMap != null)
       {
           shefDurationCode = shefDurationCodeFromMap;
       }
       
       return shefDurationCode;
    }
    /**
     * @param pe The pe to set.
     */
    public void setPe(String pe)
    {
        _pe = pe;
    }
    /**
     * @return Returns the pe.
     */
    public String getPe()
    {
        return _pe;
    }
    /**
     * @param typeSource The typeSource to set.
     */
    public void setTypeSource(String typeSource)
    {
        _typeSource = typeSource;
    }
    /**
     * @return Returns the typeSource.
     */
    public String getTypeSource()
    {
        return _typeSource;
    }
    /**
     * @param ihfsDur The ihfsDur to set.
     */
    public void setIhfsDur(int ihfsDur)
    {
        _ihfsDur = ihfsDur;
    }
    /**
     * @return Returns the ihfsDur.
     */
    public int getIhfsDur()
    {
        return _ihfsDur;
    }
    /**
     * @param shefDur The shefDur to set.
     */
    public void setShefDur(String shefDur)
    {
        _shefDur = shefDur;
    }
    /**
     * @return Returns the shefDur.
     */
    public String getShefDur()
    {
        return _shefDur;
    }
    /**
     * @param extremum The extremum to set.
     */
    public void setExtremum(String extremum)
    {
        _extremum = extremum;
    }
    /**
     * @return Returns the extremum.
     */
    public String getExtremum()
    {
        return _extremum;
    }
    
    //  -------------------------------------------------------------------------------
    public static void main(String[] argArray)
    {
        ParamCode code = null;
        
        code = new ParamCode("HGIRGZ");
        System.out.println("code = " + code.toString());
        
        
        code = new ParamCode("HPIRZX");
        System.out.println("code = " + code.toString());
        
    }
    
    //  -------------------------------------------------------------------------------
    
} //end ParamCode
