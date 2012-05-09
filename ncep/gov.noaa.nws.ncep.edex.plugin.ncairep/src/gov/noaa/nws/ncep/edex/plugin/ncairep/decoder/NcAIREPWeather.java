/**
 * This software was modified from Raytheon's airep plugin by
 * NOAA/NWS/NCEP/NCO to order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.ncairep.decoder;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

/*a
 * TODO Enter a description here.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * 04/27/2011              F.J.Yen       Initial creation from airep.
 * </pre>
 *  * 
 * @author F. J. Yen
 * @version 1.0
 */
public class NcAIREPWeather
{
    private static final String VALID_CHARS = "/0123456789";
    private static final int WX_LENGTH = 3;
    
    private static final int WX_HAZARD	= 0;
    private static final int WX_WEATHER = 1;
    private static final int WX_FLIGHT	= 2;
    
    private static final int IMISSD = IDecoderConstantsN.INTEGER_MISSING;
    
    private final String theRawData;

    // Decoded flight hazards - See table 3.3
//    private Integer theHazard = null;
    private Integer theHazard = IMISSD;
//    private Integer theWeather = null;
    private Integer theWeather = IMISSD;
//    private Integer theFlightConditions = null;
    private Integer theFlightConditions = IMISSD;

    /**
     * 
     * @param aType
     */
    public NcAIREPWeather(String aWeatherGroup)
    {
        theRawData = aWeatherGroup;
        decodeElement();
    } // NcAIREPWeather()
 
    public Integer getHazard()
    {
        return theHazard;
    } // getHazard()
    
    public Integer getWeather()
    {
        return theWeather;
    } // getWeather()
    
    public Integer getFlightConditions()
    {
        return theFlightConditions;
    } // getFlightConditions()
    
 
    //***********************************************
    //  Table 3.3. AIREP Hazards (H).
    //    Code  Figure Explanation
    //      0   None
    //      1   Light Turbulence
    //      2   Moderate Turbulence
    //      3   Severe Turbulence
    //      4   Extreme Turbulence
    //      5   Trace of Icing
    //      6   Light Icing
    //      7   Moderate Icing
    //      8   Severe Icing
    //      9   Hail
    //
    //  Table 3.4. AIREP Weather (W).
    //    Code  Figure Explanation
    //      0   Clear
    //      1   Scattered Clouds
    //      2   Broken Clouds
    //      3   Continuous Layers
    //      4   Lightning
    //      5   Drizzle
    //      6   Continuous Rain
    //      7   Continuous Snow
    //      8   Rain or Snow Showers
    //      9   Thunderstorms
    //
    //
    //  Table 3.5. AIREP Flight Conditions (FC).
    //     Code Figure Explanation
    //      0   Clear
    //      1   Above Clouds (tops less than 10,000 ft)
    //      2   Above Clouds (tops 10,000 to 18,000 ft)
    //      3   Above Clouds (tops over 18,000 ft)
    //      4   Below Clouds (bases less than 10,000 ft)
    //      5   Below Clouds (bases 10,000 to 18,000 ft)
    //      6   Below Clouds (bases above 18,000 ft)
    //      7   Between Broken or Overcast Layers
    //      8   In Clouds
    //      9   In and Out of Clouds
    //***********************************************
    
    private void decodeElement()
    {
        if(theRawData.length() == WX_LENGTH)
        {
            int pos = VALID_CHARS.indexOf(theRawData.charAt(WX_HAZARD)); 
            if(pos >= 0)
            {
                theHazard = pos-1;                
            }
            pos = VALID_CHARS.indexOf(theRawData.charAt(WX_WEATHER)); 
            if(pos >= 0)
            {
                theWeather = pos-1;                
            }
            pos = VALID_CHARS.indexOf(theRawData.charAt(WX_FLIGHT)); 
            if(pos >= 0)
            {
                theFlightConditions = pos-1;                
            }
        }
    } // decodeElement()
    
    public String toString()
    {
        StringBuffer retData = new StringBuffer();
        retData.append((theHazard >= 0) ? String.valueOf(theHazard) : "/");
        retData.append((theWeather >= 0) ? String.valueOf(theWeather) : "/");
        retData.append((theFlightConditions >= 0) ? String.valueOf(theFlightConditions) : "/");
        
        
        return retData.toString();
    } // toString()
}
