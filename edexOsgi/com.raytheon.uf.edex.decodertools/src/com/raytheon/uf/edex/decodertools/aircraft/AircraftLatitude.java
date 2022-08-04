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
package com.raytheon.uf.edex.decodertools.aircraft;

import java.util.regex.Pattern;

import com.raytheon.uf.edex.decodertools.core.BasePoint;


/**
 * Parse various forms of Latitude data and decode these into numeric form.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * Feb 28, 2005        753 jkorman       Initial creation.
 * May  4, 2005       1906 jkorman       Correct decode of longitude minutes
 * 20071227            384 jkorman       Ported to edex.
 * Sep 18, 2014       3627 mapeters      Removed unused splitLatLon().
 * </pre>
 */
public class AircraftLatitude
{
    private static final int MINUTES_PER_DEGREE = 60;

    private static final Pattern LA_DDH     = Pattern.compile("^\\d{2}[NS]{1}");
    private static final Pattern LA_DDDDH   = Pattern.compile("^\\d{2}\\d{2}[NS]{1}");
    private static final Pattern LA_DDdDDH  = Pattern.compile("^\\d{2}\\.\\d{2}[NS]{1}");

    private static final Pattern LA_HDD     = Pattern.compile("^[NS]{1}\\d{2}");
    private static final Pattern LA_HDDDD   = Pattern.compile("^[NS]{1}\\d{2}\\d{2}");
    private static final Pattern LA_HDDdDD  = Pattern.compile("^[NS]{1}\\d{2}\\.\\d{2}");

    // This isn't legal, but some folks like to use it!
    private static final String EQUATOR = "EQ";
    // Once set the obs data cannot be changed!
    private final String theRawData;
    // The decoded latitude data.
    private Double latitude = null;
    
    /**
     * Construct a Latitude decoder for the specifed data and perform the decode
     * operation. 
     * @param aLatitude A string latitude value to decode. 
     */
    public AircraftLatitude(String aLatitude)
    {
        theRawData = aLatitude;
        decode(); 
    } // AircraftLatitude()
    
    /**
     * Get the decoded latitude value from the decoder.
     * @return The decoded latitude. If the decode failed a null instance will
     * be returned.
     */
    public Double getLatitude()
    {
        return latitude;
    } // getLatitude()
    
    /**
     * Convert latitude in various formats to a numeric value. Any pattern of
     * latitude with an explicit decimal point is assumed to a decimal number
     * otherwise the value is considered to be degrees and optional minutes.
     * @return The decoded latitude value. If the decode failed return null.
     */
    private void decode()
    {
        latitude = decodeLatitude(theRawData); 
    } // decode()
    
    /**
     * Convert latitude in various formats to a numeric value. Any pattern of
     * latitude with an explicit decimal point is assumed to a decimal number
     * otherwise the value is considered to be degrees and optional minutes.
     * @return The decoded latitude value. If the decode failed return null.
     */
    public Double decodeLatitude()
    {
        decode(); 
        return latitude;
    } // decodeLatitude()

    /**
     * Attempt to construct an instance of AircraftLatitude with a given string
     * value. If the decode fails then a null instance is returned. 
     * @param aLatitude A string latitude value to decode. 
     * @return The decoded latitude value. If the decode failed return null.
     */
    public static AircraftLatitude aircraftLatitudeFactory(String aLatitude)
    {
        AircraftLatitude retValue = new AircraftLatitude(aLatitude);
        // Did the decode succeed? If not we will return a null.
        if(retValue.latitude == null)
        {
            retValue = null;
        }
        return retValue;
    } // aircraftLatitudeFactory()

    /**
     * Convert latitude in various formats to a numeric value. Any pattern of
     * latitude with an explicit decimal point is assumed to a decimal number
     * otherwise the value is considered to be degrees and optional minutes.
     * @param aLatitude A string latitude value to decode. 
     * @return The decoded latitude value. If the decode failed return null.
     */
    public static Double decodeLatitude(String aLatitude)
    {
        Double retValue = null;

        // Set to NaN so we can check if we've decoded something.
        double latitude = Double.NaN;

        // Special check. Some people like to encode the equator as "EQ"
        if(EQUATOR.equals(aLatitude))
        {
            latitude = 0.0;
        }
        else if(LA_DDH.matcher(aLatitude).matches())
        {
            latitude = Double.parseDouble(aLatitude.substring(0,2));
            if(aLatitude.charAt(2) == 'S')
            {
                latitude *= -1;
            }
        }
        else if(LA_DDDDH.matcher(aLatitude).matches())
        {
            latitude = Double.parseDouble(aLatitude.substring(0,2));
            latitude += Double.parseDouble(aLatitude.substring(2,4)) / MINUTES_PER_DEGREE;
            if(aLatitude.charAt(4) == 'S')
            {
                latitude *= -1;
            }
        }
        else if(LA_HDD.matcher(aLatitude).matches())
        {
            latitude = Double.parseDouble(aLatitude.substring(1,3));
            if(aLatitude.charAt(0) == 'S')
            {
                latitude *= -1;
            }
        }
        else if(LA_HDDDD.matcher(aLatitude).matches())
        {
            latitude = Double.parseDouble(aLatitude.substring(1,3));
            latitude += Double.parseDouble(aLatitude.substring(3)) / MINUTES_PER_DEGREE;
            if(aLatitude.charAt(0) == 'S')
            {
                latitude *= -1;
            }
        }
        else if(LA_DDdDDH.matcher(aLatitude).matches())
        {
            latitude = Double.parseDouble(aLatitude.substring(0,5));
            if(aLatitude.charAt(5) == 'S')
            {
                latitude *= -1;
            }
        }
        else if(LA_HDDdDD.matcher(aLatitude).matches())
        {
            latitude = Double.parseDouble(aLatitude.substring(1));
            if(aLatitude.charAt(0) == 'S')
            {
                latitude *= -1;
            }
        }
        // If the latitude is still NaN, this will fail!
        if(latitude == latitude)
        {
            retValue = createLatitude(latitude);
        }
        return retValue;
    } // decodeLatitude()
    
    /**
     * Create a Latitude instance from a given value. If the is invalid, return
     * a null.
     * @param aLatitude A latitude value to create.
     * @return The created Latitude instance or null.
     */
    public static Double createLatitude(double aLatitude)
    {
        Double retValue = null;
        try {
            retValue = new Double(aLatitude);
        } catch(Exception nothing) {
        }
        return retValue;
    } // createLatitude()

    /**
     * Create a Latitude instance from a LatLonPoint instance.
     * @param aPoint A LatLongPoint instance containing a latitude.
     * @return The created Latitude instance or null.
     */
    public static Double createLatitude(BasePoint aPoint)
    {
        Double retValue = null;
        if(aPoint != null) {
            try {
                retValue = aPoint.getLatitude();
            } catch(Exception nothing) {
            }
        }
        return retValue;
    } // createLatitude()
    
} // AircraftLatitude
