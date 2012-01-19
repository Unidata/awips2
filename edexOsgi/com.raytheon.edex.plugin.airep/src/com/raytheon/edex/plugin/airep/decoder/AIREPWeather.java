/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.airep.decoder;

/**
 * TODO Enter a description here.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * 20080103            384 jkorman     Initial Coding.
 * </pre>
 */
public class AIREPWeather
{
    private static final String VALID_CHARS = "/0123456789";
    private static final int WX_LENGTH = 3;
    
    private static final int WX_HAZARD	= 0;
    private static final int WX_WEATHER = 1;
    private static final int WX_FLIGHT	= 2;
    
    private final String theRawData;

    // Decoded flight hazards - See table 3.3
    private Integer theHazard = null;
    private Integer theWeather = null;
    private Integer theFlightConditions = null;

    /**
     * 
     * @param aType
     */
    public AIREPWeather(String aWeatherGroup)
    {
        theRawData = aWeatherGroup;
        decodeElement();
    } // AIREPWeather()
 
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
