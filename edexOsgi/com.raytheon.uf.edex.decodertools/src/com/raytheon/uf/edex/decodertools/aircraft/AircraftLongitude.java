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
 * Parse various forms of Longitude data and decode these into numeric form.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date           PTR#     Engineer      Description
 * ------------   -------- ------------- -------------------------------------
 * Feb 28, 2005        753 jkorman       Initial creation.
 * May  4, 2005       1906 jkorman       Correct decode of longitude minutes
 * 20071227            384 jkorman       Ported to edex.
 * </pre>
 */
public class AircraftLongitude
{
    private static final int MINUTES_PER_DEGREE = 60;
    // Once set the obs data cannot be changed!
    private final String theRawData;
    // The decoded longitude data.
    private Double longitude = null;
    
    //********************
    private static final Pattern LO_HDDD
    	= Pattern.compile("^[EW]{1}\\d{3}");
    private static final Pattern LO_HDDDDD
    	= Pattern.compile("^[EW]{1}\\d{3}\\d{2}");
    private static final Pattern LO_HDDDdDD
    	= Pattern.compile("^[EW]{1}\\d{3}\\.\\d{2}");

    private static final Pattern LO_DDDH
    	= Pattern.compile("^\\d{3}[EW]{1}");
    private static final Pattern LO_DDDDDH
    	= Pattern.compile("^\\d{3}\\d{2}[EW]{1}");
    private static final Pattern LO_DDDdDDH
    	= Pattern.compile("^\\d{3}\\.\\d{2}[EW]{1}");

    /**
     * Construct a Longitude decoder for the specifed data and perform the decode
     * operation. 
     * @param aLongitude A string longitude value to decode. 
     */
    public AircraftLongitude(String aLongitude) {
        theRawData = aLongitude;
        decode();
    } // AircraftLongitude()

    /**
     * Convert longitude in various formats to a numeric value. Any pattern of
     * latitude with an explicit decimal point is assumed to a decimal number
     * otherwise the value is considered to be degrees and optional minutes.
     * @return The decoded latitude value. If the decode failed return null.
     */
    private void decode() {
        longitude = decodeLongitude(theRawData); 
    } // decode()
    
    /**
     * Get the decoded longitude value from the decoder.
     * @return The decoded longitude. If the decode failed a null instance will
     * be returned.
     */
    public Double getLongitude() {
        return longitude;
    } // getLongitude()

    /**
     * Convert longitude in various formats to a numeric value. Any pattern of
     * longitude with an explicit decimal point is assumed to a decimal number
     * otherwise the value is considered to be degrees and optional minutes.
     * @return The decoded longitude value. If the decode failed return null.
     */
    public Double decodeLongitude() {
        decode(); 
        return longitude;
    } // decodeLongitude()
    
    /**
     * Attempt to construct an instance of AircraftLongitude with a given string
     * value. If the decode fails then a null instance is returned. 
     * @param aLongitude A string longitude value to decode. 
     * @return The decoded longitude value. If the decode failed return null.
     */
    public static AircraftLongitude aircraftLongitudeFactory(String aLongitude) {
        AircraftLongitude retValue = new AircraftLongitude(aLongitude);
        retValue.longitude = retValue.decodeLongitude(); 
        if(retValue.longitude == null) {
            retValue = null;
        }
        return retValue;
    } // aircraftLongitudeFactory()

   /**
    * Convert longitude in various formats to a numeric value. Any pattern of
    * longitude with an explicit decimal point is assumed to a decimal number
    * otherwise the value is considered to be degrees and optional minutes.
    * @param aLongitude A string longitude value to decode. 
    * @return The decoded longitude value. If the decode failed return null.
    */
    public static Double decodeLongitude(String aLongitude) {
        Double retValue = null;
        // Set to NaN so we can check if we've decoded something.
        double longitude = Double.NaN;
        if(LO_DDDH.matcher(aLongitude).matches()) {
            longitude = Double.parseDouble(aLongitude.substring(0,3));
            if(aLongitude.charAt(3) == 'W') {
                longitude *= -1;
            }
        }
        else if(LO_DDDDDH.matcher(aLongitude).matches()) {
            longitude = Double.parseDouble(aLongitude.substring(0,3));
            longitude += Double.parseDouble(aLongitude.substring(3,5)) / MINUTES_PER_DEGREE;
             
            if(aLongitude.charAt(5) == 'W') {
                longitude *= -1;
            }
        }
        else if(LO_HDDD.matcher(aLongitude).matches()) {
            longitude = Double.parseDouble(aLongitude.substring(1));
            if(aLongitude.charAt(0) == 'W') {
                longitude *= -1;
            }
        }
        else if(LO_HDDDDD.matcher(aLongitude).matches()) {
            longitude = Double.parseDouble(aLongitude.substring(1,4));
            longitude += Double.parseDouble(aLongitude.substring(4)) / MINUTES_PER_DEGREE;
            if(aLongitude.charAt(0) == 'W') {
                longitude *= -1;
            }
        }
        else if(LO_DDDdDDH.matcher(aLongitude).matches()) {
            longitude = Double.parseDouble(aLongitude.substring(0,6));
            if(aLongitude.charAt(6) == 'W') {
                longitude *= -1;
            }
        }
        else if(LO_HDDDdDD.matcher(aLongitude).matches()) {
            // We have an explicit decimal point so assume decimal degrees.
            longitude = Double.parseDouble(aLongitude.substring(1));
            if(aLongitude.charAt(0) == 'W') {
                longitude *= -1;
            }
        }
        // If the longitude is still NaN, this will fail!
        if(longitude == longitude) {
           retValue = createLongitude(longitude);
        }
        return retValue;
    } // decodeLongitude()

    /**
     * Create a Longitude instance from a given value. If the is invalid, return
     * a null.
     * @param aLongitude A longitude value to create.
     * @return The created Longitude instance or null.
     */
    public static Double createLongitude(double aLongitude)
    {
        Double retValue = null;
        try {
            retValue = new Double(aLongitude);
        } catch(Exception nothing) {
        }
        return retValue;
    } // createLongitude()
    
    /**
     * Create a Longitude instance from a LatLonPoint instance.
     * @param aPoint A LatLongPoint instance containing a longitude.
     * @return The created Longitude instance or null.
     */
    public static Double createLongitude(BasePoint aPoint) {
        Double retValue = null;
        if(aPoint != null) {
            try {
                retValue = aPoint.getLongitude();
            } catch(Exception nothing) {
            }
        }
        return retValue;
    } // createLatitude()
    
} // AircraftLongitude
