/**
 * NcUairShipMobile - process ship and dropsonde data
 * 
 * This java class intends to serve as an utility only for UAIR decoder.
 * 
 * HISTORY
 *
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2010      210				L. Lin     	Initial coding
 * 09/2011      457             S. Gurung   Renamed H5 to Nc and h5 to nc
 * 09/02/2014                   Chin Chen   fix surface height missing on some stations issue
 * 
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ncuair.util;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class NcUairShipMobile {

    private static float xlat;

    private static float xlon;

    private static String stnId;

    private static String codeMessage;

    private static String obsUTC;

    private static String UTC;

    private static int iutc;

    private static String stationNumber;

    private static int surfaceHeight;

    private static int elevation;

    private static Calendar obsTime;

    private static Calendar synopticTime;

    private static int topwind;

    private static Boolean windKnot;

    public static float getXlat() {
        return xlat;
    }

    public static void setXlat(float xlat) {
        NcUairShipMobile.xlat = xlat;
    }

    public static float getXlon() {
        return xlon;
    }

    public static void setXlon(float xlon) {
        NcUairShipMobile.xlon = xlon;
    }

    public static String getStnId() {
        return stnId;
    }

    public static void setStnId(String stnId) {
        NcUairShipMobile.stnId = stnId;
    }

    public static String getCodeMessage() {
        return codeMessage;
    }

    public static void setCodeMessage(String codeMessage) {
        NcUairShipMobile.codeMessage = codeMessage;
    }

    public static String getUTC() {
        return UTC;
    }

    public static void setUTC(String utc) {
        UTC = utc;
    }

    public static int getIutc() {
        return iutc;
    }

    public static void setIutc(int iutc) {
        NcUairShipMobile.iutc = iutc;
    }

    public static String getStationNumber() {
        return stationNumber;
    }

    public static void setStationNumber(String stationNumber) {
        NcUairShipMobile.stationNumber = stationNumber;
    }

    public static int getSurfaceHeight() {
        return surfaceHeight;
    }

    public static void setSurfaceHeight(int surfaceHeight) {
        NcUairShipMobile.surfaceHeight = surfaceHeight;
    }

    public static int getElevation() {
        return elevation;
    }

    public static void setElevation(int elevation) {
        NcUairShipMobile.elevation = elevation;
    }

    public static String getObsUTC() {
        return obsUTC;
    }

    public static void setObsUTC(String obsUTC) {
        NcUairShipMobile.obsUTC = obsUTC;
    }

    public static Calendar getObsTime() {
        return obsTime;
    }

    public static void setObsTime(Calendar obsTime) {
        NcUairShipMobile.obsTime = obsTime;
    }

    public static Calendar getSynopticTime() {
        return synopticTime;
    }

    public static void setSynopticTime(Calendar synopticTime) {
        NcUairShipMobile.synopticTime = synopticTime;
    }

    public static int getTopwind() {
        return topwind;
    }

    public static void setTopwind(int topwind) {
        NcUairShipMobile.topwind = topwind;
    }

    public static Boolean getWindKnot() {
        return windKnot;
    }

    public static void setWindKnot(Boolean windKnot) {
        NcUairShipMobile.windKnot = windKnot;
    }

    /**
     * Decodes ship or dropsonde data
     * 
     * @param report
     *            The input report
     * @param dataType
     *            The input dataType
     * @return
     */
    public static void ShipMobileField(String report, String dataType) {

        Boolean drop = false;
        Boolean ship = false;

        xlat = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        xlon = IDecoderConstantsN.UAIR_FLOAT_MISSING;
        stnId = null;
        codeMessage = null;
        obsUTC = null;
        UTC = null;
        iutc = -1;
        stationNumber = null;
        surfaceHeight = 0; // Chin: sfcElevSolution
                           // IDecoderConstantsN.UAIR_INTEGER_MISSING;
        elevation = 0; // Chin: sfcElevSolution
                       // IDecoderConstantsN.UAIR_INTEGER_MISSING;
        topwind = IDecoderConstantsN.UAIR_INTEGER_MISSING;
        windKnot = false;

        int day = IDecoderConstantsN.UAIR_INTEGER_MISSING;

        final String SHIP = "( )?([A-Z]{4}[0-9]{2}|[A-Z]{4}[0-9]{1}|[A-Z]{4}) (\\d{5}|\\d{4}/) (\\d{5}) (\\d{5}) (\\d{5}) (\\d{5})(.*)";
        Pattern shipPattern = Pattern.compile(SHIP, Pattern.DOTALL);
        Matcher shipMatcher = shipPattern.matcher(report);

        final String MOBILE = "( )?(\\d{5}|\\d{4}/) (\\d{5}) (\\d{5}) (\\d{5})(.*)";
        Pattern mobilePattern = Pattern.compile(MOBILE, Pattern.DOTALL);
        Matcher mobileMatcher = mobilePattern.matcher(report);
        int quadrant = 0;
        if (shipMatcher.find()) {
            ship = true;
            stnId = shipMatcher.group(2);
            xlat = (float) Integer.parseInt(shipMatcher.group(4)
                    .substring(2, 5)) / (float) 10;
            xlon = (float) Integer.parseInt(shipMatcher.group(5)
                    .substring(1, 5)) / (float) 10;
            quadrant = Integer.parseInt(shipMatcher.group(5).substring(0, 1));
            codeMessage = shipMatcher.group(7) + shipMatcher.group(8);
            obsUTC = shipMatcher.group(3).substring(2, 4);
            day = Integer.parseInt(shipMatcher.group(3).substring(0, 2));

            // elevation = 0;
            String heightGroup = shipMatcher.group(7);
            if (heightGroup.substring(0, 3).equals("999")) {
                surfaceHeight = Integer.parseInt(heightGroup.substring(3, 5));
                // Chin: sfcElevSolution
                elevation = surfaceHeight;
            }

            String topwindIndicator = shipMatcher.group(3).substring(4, 5);
            if (topwindIndicator.equals("/")) {
                topwind = IDecoderConstantsN.UAIR_INTEGER_MISSING;
            } else {
                topwind = Integer.parseInt(topwindIndicator);
            }

        } else if (mobileMatcher.find()) {
            xlat = (float) Integer.parseInt(mobileMatcher.group(3).substring(2,
                    5))
                    / (float) 10;
            xlon = (float) Integer.parseInt(mobileMatcher.group(4).substring(1,
                    5))
                    / (float) 10;
            quadrant = Integer.parseInt(mobileMatcher.group(4).substring(0, 1));
            codeMessage = mobileMatcher.group(6);
            obsUTC = mobileMatcher.group(2).substring(2, 4);
            day = Integer.parseInt(mobileMatcher.group(2).substring(0, 2));

            String topwindIndicator = mobileMatcher.group(2).substring(4, 5);
            if (topwindIndicator.equals("/")) {
                topwind = IDecoderConstantsN.UAIR_INTEGER_MISSING;
            } else {
                topwind = Integer.parseInt(topwindIndicator);
            }

            drop = true;
        }

        if (quadrant == 7) {
            xlon = -xlon;
        } else if (quadrant == 5) {
            xlon = -xlon;
            xlat = -xlat;
        } else if (quadrant == 3) {
            xlat = -xlat;
        }

        if (day > 50) {
            windKnot = true;
            day = day - 50;
        }

        if (ship) {
            stnId = stnId + obsUTC;
        }
        // Adjusts the time to the nearest 3-hourly interval.
        int hour = Integer.parseInt(obsUTC);
        int idiff = hour % 3;
        if (idiff == 1) {
            hour = hour - 1;
        } else if (idiff == 2) {
            /*
             * If hour is one hour early, add hour and change day if hour
             */
            hour = hour + 1;
            if (hour == 24) {
                hour = 0;
            }
        }
        iutc = hour;
        UTC = Integer.toString(hour);
        if (hour < 10) {
            UTC = "0" + UTC;
        }

        // Get observation time
        if (obsUTC != null && day > 0 && day <= 31) {
            String oday = Integer.toString(day);
            if (day < 10) {
                oday = "0" + oday;
            }
            String obsDate = oday + obsUTC + "00";
            Calendar cal = null;
            obsTime = UtilN.findDataTime(obsDate, cal);
        }

        // Get synoptic time
        if (UTC != null && day > 0 && day <= 31) {
            String sday = Integer.toString(day);
            if (day < 10) {
                sday = "0" + sday;
            }
            String synopticDate = sday + UTC + "00";
            Calendar cal = null;
            synopticTime = UtilN.findDataTime(synopticDate, cal);

            if (obsUTC.equals("23")) {
                // Adjust day to next day
                synopticTime.set(Calendar.DAY_OF_MONTH, day + 1);
            }
        }

        if (drop) {
            // Decode 61616 and 62626 group
            Dropsonde(report);
        }
    }

    /**
     * Decodes dropsonde data
     * 
     * @param report
     *            The input report
     * @return
     */
    public static void Dropsonde(String report) {

        final String DROP = "61616 (AF|NOAA|NASA)(\\d{1,3}) (.*) OB (\\d{2}) 62626";
        Pattern dropPattern = Pattern.compile(DROP, Pattern.DOTALL);
        Matcher dropMatcher = dropPattern.matcher(report);

        // Chin: sfcElevSolution start
        // Chin: the information is actually not provided
        // final String DROP2 = "62626 (.*) LST WND (\\d3) MBL WIND ";
        // final String DROP2 = "62626 (.*) LST WND (\\d{3})";
        // Pattern drop2Pattern = Pattern.compile(DROP2, Pattern.DOTALL);
        // Matcher drop2Matcher = drop2Pattern.matcher(report);
        // end sfcElevSolution
        UTC = obsUTC;

        if (dropMatcher.find()) {
            stationNumber = dropMatcher.group(4) + obsUTC
                    + dropMatcher.group(2);
            stnId = dropMatcher.group(1) + dropMatcher.group(2)
                    + dropMatcher.group(4);
        }
        // Chin: sfcElevSolution start
        // if (drop2Matcher.find()) {
        // elevation = Integer.parseInt(drop2Matcher.group(2));
        // }
        // end sfcElevSolution
    }

}
