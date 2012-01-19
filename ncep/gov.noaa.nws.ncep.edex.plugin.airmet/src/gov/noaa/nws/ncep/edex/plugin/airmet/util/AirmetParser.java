/**
 * Airmet DecoderUtil
 * 
 * This java class intends to serve as a decoder utility for AIRMET.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 05/2009      39				L. Lin     	Initial coding
 * 07/2009		39				L. Lin		Migration to TO11
 * 09/2009		39				L. Lin		Add latitude/longitude to location table
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.airmet.util;

import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetReport;
import gov.noaa.nws.ncep.edex.tools.decoder.LatLonLocTbl;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

public class AirmetParser {

    public static final Log logger = LogFactory.getLog(AirmetParser.class);

    /**
     * Constructor
     */
    public AirmetParser() {
    }

    /**
     * Parse the WMO line and store WMO header, OfficeID, issue time,
     * designatorBBB,...
     * 
     * @param wmoline
     *            The bulletin message
     * 
     * @return an AirmetRecord
     */
    public static AirmetRecord processWMO(String wmoline, Headers headers) {

        AirmetRecord record = null;
        // Regular expression for WMO/ICAO, station ID, and issue date (and
        // maybe designator BBB)
        final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( ([A-Z]{3}))?";

        // Pattern used for extracting WMO header, officeID, product purge time,
        // and issue date and designatorBBB
        final Pattern wmoPattern = Pattern.compile(WMO_EXP);
        Matcher theMatcher = wmoPattern.matcher(wmoline);

        if (theMatcher.find()) {
            record = new AirmetRecord();

            record.setWmoHeader(theMatcher.group(1));
            record.setIssueOffice(theMatcher.group(2));
            record.setDesignatorBBB(theMatcher.group(5));

            // Decode the issue time.
            Calendar issueTime = TimeTools.findDataTime(theMatcher.group(3), headers);

            record.setIssueTime(issueTime);
            DataTime dataTime = new DataTime(issueTime);
            record.setDataTime(dataTime);
        }
        return record;
    }

    /**
     * Obtains reportName as: SIERRA, TANGO or ZULU from a bulletin.
     * 
     * @param bullMessage
     *            The bulletin message
     * @return a string for report type
     */
    public static String getReportName(String bullMessage) {

        // Regular expression AIRMET reportName
        final String REPORTNAME_EXP = "AIRMET (SIERRA|TANGO|ZULU) ";

        // Pattern used for extracting reportName
        final Pattern reportNamePattern = Pattern.compile(REPORTNAME_EXP);
        Matcher theMatcher = reportNamePattern.matcher(bullMessage);

        if (theMatcher.find()) {
            return theMatcher.group(1);
        } else {
            return " ";
        }
    }

    /**
     * Obtains updateNumber from a bulletin.
     * 
     * @param bullMessage
     *            The bulletin message
     * @return an integer for update number
     */
    public static Integer getUpdateNumber(String bullMessage) {
        // default update number
        Integer retUpdate = 0;

        // Regular expression update number
        final String UPDATE_EXP = "AIRMET (SIERRA|TANGO|ZULU) UPDT ([0-9]{1}) ";

        // Pattern used for extracting update number
        final Pattern updatePattern = Pattern.compile(UPDATE_EXP);
        Matcher theMatcher = updatePattern.matcher(bullMessage);

        if (theMatcher.find()) {
            return Integer.parseInt(theMatcher.group(2).substring(0, 1).trim());
        } else {
            return retUpdate;
        }
    }

    /**
     * Obtains correctionFlag from a report
     * 
     * @param bullMessage
     *            The bulletin message
     * @return an integer for correctionFlag 0 for normal, 1 for COR or CC, 2
     *         for AMD, 3 for TEST, and 4 for NIL/EXPIRE report
     */
    public static Integer getCorrectionFlag(String bullMessage) {

        Integer retCorrection = 0;

        // Regular expression correction
        final String CORRECTION_EXP = "(\\x1e)([A-Z]{4}) ([A-Z]{2}) ([0-9]{6}) ([A-Z]{3})";

        // Pattern used for extracting correctionFlag
        final Pattern correctionPattern = Pattern.compile(CORRECTION_EXP);
        Matcher theMatcher = correctionPattern.matcher(bullMessage);

        if (theMatcher.find()) {
            if (theMatcher.group(5).equals("COR")) {
                retCorrection = 1;
            } else if (theMatcher.group(5).equals("AMD")) {
                retCorrection = 2;
            } else if ((theMatcher.group(5).substring(0, 2)).equals("CC")) {
                retCorrection = 1;
            }
        }

        // Regular expression TEST message
        final String TEST_EXP = " TEST ";
        // Pattern used for extracting TEST message
        final Pattern testPattern = Pattern.compile(TEST_EXP);
        Matcher testMatcher = testPattern.matcher(bullMessage);
        if (testMatcher.find()) {
            retCorrection = 3;
        }

        return retCorrection;
    }

    /**
     * Obtains cancelFlag from a report
     * 
     * @param bullMessage
     *            The bulletin message
     * @return an integer for cancelFlag 0 for normal and 1 for cancel
     */
    public static Integer getCancelFlag(String report) {

        Integer retCancel = 0;

        // Regular expression cancelFlag
        final String CANCEL_EXP = "(CANCEL|CNCL) AIRMET";

        // Pattern used for extracting cancel
        final Pattern cancelPattern = Pattern.compile(CANCEL_EXP);
        Matcher theMatcher = cancelPattern.matcher(report);

        if (theMatcher.find()) {
            retCancel = 1;
        }
        return retCancel;
    }

    /**
     * Obtains hazardType as: IFR (instrument flight rules), MO (mountain
     * obscuration), TB (turbulence), IC (icing), SW (sustained winds), or WS
     * (low level wind shear) from a report.
     * 
     * @param theReport
     *            Input report message.
     * @return a string for class type
     */
    public static String getHazardType(String theReport) {

        String retHazardType = " ";
        // Regular expression AIRMET classType
        final String HAZARDTYPE_EXP = "AIRMET (IFR|MTN OBSCN|TURB|ICE|STG SFC WNDS)";
        final String TYPE2_EXP = "(LLWS POTENTIAL)";
        final String TYPE3_EXP = "(ICE|TURB|CIG BLW|VIS BLW|MTNS|LLWS|SUSTAINED)";
        final String TYPE4_EXP = "(CANCEL|CNCL)";

        // Pattern used for extracting hazardType
        final Pattern classTypePattern = Pattern.compile(HAZARDTYPE_EXP);
        final Pattern type2Pattern = Pattern.compile(TYPE2_EXP);
        final Pattern type3Pattern = Pattern.compile(TYPE3_EXP);
        final Pattern type4Pattern = Pattern.compile(TYPE4_EXP);

        Matcher theMatcher = classTypePattern.matcher(theReport);
        Matcher type2Matcher = type2Pattern.matcher(theReport);
        Matcher type3Matcher = type3Pattern.matcher(theReport);
        Matcher type4Matcher = type4Pattern.matcher(theReport);

        if (theMatcher.find()) {
            if (theMatcher.group(1).equals("IFR")) {
                retHazardType = "INSTRUMENT FLIGHT RULES";
            } else if (theMatcher.group(1).equals("MTN OBSCN")) {
                retHazardType = "MOUNTAIN OBSCURATION";
            } else if (theMatcher.group(1).equals("TURB")) {
                retHazardType = "TURBULENCE";
            } else if (theMatcher.group(1).equals("ICE")) {
                retHazardType = "ICING";
            } else if (theMatcher.group(1).equals("STG SFC WNDS")) {
                retHazardType = "SUSTAINED SFC WINDS";
            }
        } else if (type2Matcher.find()) {
            if (type2Matcher.group(1).equals("LLWS")) {
                retHazardType = "LOW LEVEL WIND SHEAR";
            }
        } else if (type3Matcher.find()) {
            if (type3Matcher.group(1).equals("ICE")) {
                retHazardType = "ICING";
            } else if (type3Matcher.group(1).equals("TURB")) {
                retHazardType = "TURBULENCE";
            } else if (type3Matcher.group(1).equals("CIG BLW")) {
                retHazardType = "INSTRUMENT FLIGHT RULES";
            } else if (type3Matcher.group(1).equals("VIS BLW")) {
                retHazardType = "INSTRUMENT FLIGHT RULES";
            } else if (type3Matcher.group(1).equals("MTNS")) {
                retHazardType = "MOUNTAIN OBSCURATION";
            } else if (type3Matcher.group(1).equals("LLWS")) {
                retHazardType = "LOW LEVEL WIND SHEAR";
            } else if (type3Matcher.group(1).equals("SUSTAINED")) {
                retHazardType = "SUSTAINED SFC WINDS";
            }
        } else if (type4Matcher.find()) {
            retHazardType = "CANCEL";
        }

        return retHazardType;
    }

    /**
     * Obtains forecast region as: S for SIERRA, T for TANGO or Z for ZULU from
     * an AirmetRecord.
     * 
     * @param record
     *            The AirmetRecord
     * @return a string for forecast region
     */
    public static String getRegion(String reportName) {

        String retRegion = null;
        String forecastRegion = reportName;

        if (forecastRegion.equals("SIERRA")) {
            retRegion = "S";
        } else if (forecastRegion.equals("TANGO")) {
            retRegion = "T";
        } else if (forecastRegion.equals("ZULU")) {
            retRegion = "Z";
        }
        return retRegion;
    }

    /**
     * process regular section of an AIRMET report
     * 
     * @param theReport
     *            The reports from bulletin message
     * @return an AirmetReport table
     */
    public static AirmetReport processReport(String theReport) {

        // Regular expression for flight levels
        final String FLIGHT_EXP = " (BTN) (FL)?(FRZLVL|[0-9]{3})( AND |-)FL([0-9]{3})";
        final String BLW_EXP = " (BLW) FL([0-9]{3})";

        // Pattern used for extracting flight levels
        final Pattern flightPattern = Pattern.compile(FLIGHT_EXP);
        final Pattern blwPattern = Pattern.compile(BLW_EXP);

        Matcher flightMatcher = flightPattern.matcher(theReport);
        Matcher blwMatcher = blwPattern.matcher(theReport);

        AirmetReport currentReport = new AirmetReport();
        // Find and set the flight levels.
        if (flightMatcher.find()) {
            if (flightMatcher.group(1).equals("BTN")) {
                if ((flightMatcher.group(3).equals("FRZLVL"))) {
                    currentReport.setFlightLevel1("FRZLVL");
                } else {
                    currentReport.setFlightLevel1(flightMatcher.group(3)
                            .toString());
                }
                currentReport
                        .setFlightLevel2(flightMatcher.group(5).toString());
            }
        } else if (blwMatcher.find()) {
            currentReport.setFlightLevel2(blwMatcher.group(2).toString());
        }
        // Get and set reportType
        currentReport.setHazardType(getHazardType(theReport));

        // Set reportIndicator
        currentReport.setReportIndicator("AIRMET");

        // Get and set cancel flag
        currentReport.setCancelFlag(AirmetParser.getCancelFlag(theReport));

        // Replace the special characters
        currentReport.setSegment(theReport.replace('\r', ' ')
                .replace('\036', ' ').replace('\003', ' ').replace('\000', ' ')
                .replace('\001', ' '));

        // Decode the locations
        if (!AirmetParser.processLocation(theReport, currentReport)) {
            currentReport = null;
        }

        return currentReport;
    }

    /**
     * process the outlook of an AIRMET report.
     * 
     * @param theOutlook
     *            The outlook section lines from bulletin message
     * @return an AirmetReport table
     */
    public static AirmetReport processOutLook(String theOutlook,
            String forecastRegion) {

        AirmetReport currentOutLook = new AirmetReport();

        currentOutLook.setSegment(theOutlook.replace('\r', ' ')
                .replace('\036', ' ').replace('\003', ' ').replace('\000', ' ')
                .replace('\001', ' '));

        // Get and set hazard type
        currentOutLook.setHazardType(getHazardType(theOutlook));
        // Set report indicator.
        currentOutLook.setReportIndicator("OUTLOOK");

        // Decode locations
        if (AirmetParser.processLocation(theOutlook, currentOutLook)) {
            final String SID_EXP = "AREA ([0-9]{1})";

            // Pattern used for extracting the number of sequence ID.
            final Pattern sidPattern = Pattern.compile(SID_EXP);

            Matcher theMatcher = sidPattern.matcher(theOutlook);

            // Default sequenceID as: 1S, 1T, or 1Z if locations have been
            // found.
            String sequenceID = "1".concat(forecastRegion);
            if (theMatcher.find()) {
                // Multiple areas such as: "AREA 1", "AREA 2", ...etc
                sequenceID = theMatcher.group(1).concat(forecastRegion);
            }
            currentOutLook.setSequenceID(sequenceID);
        } else {
            // Default sequenceID as: 0W, 0C, or 0E if outlook section is a NIL
            // report.
            String sequenceID = "0".concat(forecastRegion);
            currentOutLook.setSequenceID(sequenceID);
        }

        // Get and set cancel flag
        currentOutLook.setCancelFlag(AirmetParser.getCancelFlag(theOutlook));

        return currentOutLook;
    }

    /**
     * Obtains start time from input bulletin.
     * 
     * @param theReport
     *            The bulletin message
     * @return a calendar for start time
     */
    public static Calendar getStartTime(String theBulletin, Headers headers) {

        // Regular expression for start time
        final String STARTTIME_EXP = "(\\x1e)([A-Z]{4}) ([A-Z]{2}) ([0-9]{6})( [A-Z]{3})?";

        // Pattern used for extracting the starting time
        final Pattern starttimePattern = Pattern.compile(STARTTIME_EXP);

        // Calendar mndTime = Calendar.getInstance();

        Matcher theMatcher = starttimePattern.matcher(theBulletin);

        if (theMatcher.find()) {
            // Get start time
            return TimeTools.findDataTime(theMatcher.group(4), headers);
        } else {
            return null;
        }
    }

    /**
     * Obtains the valid day from input bulletin.
     * 
     * @param theBulletin
     *            The input bulletin
     * @return a string for valid day.
     */
    public static String getValidDay(String theBulletin) {

        // Regular expression for the day
        final String DAY_EXP = " VALID UNTIL ([0-9]{2})([0-9]{4})";
        // Pattern used for extracting the day
        final Pattern dayPattern = Pattern.compile(DAY_EXP);
        Matcher theMatcher = dayPattern.matcher(theBulletin);

        String retDay;

        // Get sequence number according to 4-digit of valid time
        if (theMatcher.find()) {
            retDay = theMatcher.group(1);
        } else {
            Calendar cal = Calendar.getInstance();
            retDay = Integer.toString(cal.get(Calendar.DAY_OF_MONTH));
        }
        return retDay;
    }

    /**
     * Obtains the sequence ID which is composed by an identifier, a sequence
     * number and a series number.
     * 
     * @param theBulletin
     *            The input bulletin
     * @param series
     *            the series number of the report
     * @return a string for sequence ID
     */
    public static String getSequenceID(String theBulletin, Integer series) {

        // Regular expression for the sequence number
        final String SEQUENCEID_EXP = " VALID UNTIL ([0-9]{2})([0-9]{4})";
        // Pattern used for extracting the sequence number
        final Pattern idPattern = Pattern.compile(SEQUENCEID_EXP);
        Matcher theMatcher = idPattern.matcher(theBulletin);

        Integer timeGroup;
        String seqNumber;
        String identifier = "???";
        // Get sequence number according to 4-digit of valid time
        if (theMatcher.find()) {
            timeGroup = Integer.parseInt(theMatcher.group(2));
            if (timeGroup == 800 | timeGroup == 900) {
                seqNumber = "0";
            } else if (timeGroup == 1400 | timeGroup == 1500) {
                seqNumber = "2";
            } else if (timeGroup == 2000 | timeGroup == 2100) {
                seqNumber = "4";
            } else if (timeGroup == 200 | timeGroup == 300) {
                seqNumber = "6";
            } else {
                seqNumber = "8";
            }
        } else {
            seqNumber = "8";
        }
        // System.out.println("seqNumber=" + seqNumber);

        // Regular expression for identifier
        final String IDENTIFIER_EXP = "([A-Z]{3})(S|Z|T) WA ([0-9]{6})( [A-Z]{3})?";
        ;
        // Pattern used for extracting the sequence number
        final Pattern identifierPattern = Pattern.compile(IDENTIFIER_EXP);
        Matcher idMatcher = identifierPattern.matcher(theBulletin);
        // Get identifier
        if (idMatcher.find()) {
            identifier = idMatcher.group(1);
        }

        return identifier.concat(seqNumber).concat(Integer.toString(series));
    }

    /**
     * Get the end time
     * 
     * @param theBulletin
     *            The bulletin which contains end time
     * @return a calendar for end time
     */
    public static Calendar getEndTime(String theBulletin, Headers headers) {

        // Regular expression for end time
        final String ENDTIME_EXP = "VALID UNTIL ([0-9]{6})";

        // Pattern used for extracting the end time
        final Pattern endtimePattern = Pattern.compile(ENDTIME_EXP);

        // Calendar mndTime = Calendar.getInstance();

        Matcher theMatcher = endtimePattern.matcher(theBulletin);

        if (theMatcher.find()) {
            // get the end time.
            return TimeTools.findDataTime(theMatcher.group(1), headers);
        } else {
            return null;
        }
    }

    /**
     * Find and set valid start time and end time for outlook report.
     * 
     * @param theReport
     *            The outlook section contains the valid times group
     */
    public static void processValidTime(String theReport,
            AirmetReport currentOutLook, String validDay, Headers headers) {

        // Regular expression for start time
        final String TIMES_EXP = "OTLK VALID ([0-9]{4})-([0-9]{4})";

        // Pattern used for extracting the starting time and end time
        final Pattern starttimePattern = Pattern.compile(TIMES_EXP);

        Calendar startTime = null;
        Calendar endTime = null;

        Matcher theMatcher = starttimePattern.matcher(theReport);

        if (theMatcher.find()) {
            // Decode the start time and end time; then set them.
            startTime = TimeTools.findDataTime(validDay.concat(theMatcher.group(1)),
                    headers);

            currentOutLook.setStartTime(startTime);
            endTime = TimeTools.findDataTime(validDay.concat(theMatcher.group(2)),
                    headers);

            if (endTime.before(startTime)) {
                endTime.add(Calendar.DAY_OF_MONTH, 1);
            }
            currentOutLook.setEndTime(endTime);
        } else {
            currentOutLook.setStartTime(startTime);
            currentOutLook.setEndTime(endTime);
        }

    }

    /**
     * Parse the location lines and add location table to the report table if
     * any
     * 
     * @param theReport
     *            The report from bulletin message
     * @param reportTable
     *            The report Table
     * @return true if finds any location line
     */
    public static Boolean processLocation(String theReport,
            AirmetReport reportTable) {

        Boolean hasLocationLine = true;
        String locationDelimiter = "-| TO ";

        ArrayList<String> terminationList = new ArrayList<String>();
        terminationList.addAll(Arrays.asList(new String[] { "CIG", "MTNS",
                "ICE", "MOD", "LLWS", "SUSTAINED", "VIS", "TURB", "CANCEL",
                "CNCL" }));

        Scanner scLines = new Scanner(theReport)
                .useDelimiter("FROM|BOUNDED BY");

        LatLonPoint point = null;

        // throws away the first section which is not the "FROM" location
        String locationReport = scLines.next();

        if (scLines.hasNext()) {
            locationReport = scLines.next();

            Scanner scLocationLine = new Scanner(locationReport)
                    .useDelimiter("\\x0d\\x0d\\x0a");
            String lines = " ";
            String curLine = null;
            ArrayList<String> locationList = new ArrayList<String>();
            locationList.clear();
            Boolean notBreak = true;

            while (scLocationLine.hasNext() && notBreak) {
                // Get next location line
                curLine = scLocationLine.next();

                Scanner scLocationToken = new Scanner(curLine);
                if (scLocationToken.hasNext()) {
                    // Check the first token from each line
                    String firstToken = scLocationToken.next();
                    if (terminationList.contains(firstToken)) {
                        // terminate
                        notBreak = false;
                        break;
                    }
                }
                lines = lines.concat(" ").concat(curLine);
            }

            // Clean up the leading space
            lines = UtilN.removeLeadingWhiteSpaces(lines);
            // Parse the location lines by a "-" or "TO"
            Scanner scLocation = new Scanner(lines)
                    .useDelimiter(locationDelimiter);
            locationList.clear();
            // Get all locations
            while (scLocation.hasNext()) {
                locationList.add(scLocation.next());
            }

            // set locations to data base
            Integer idxLocation = 0;
            if (locationList.size() > 1) {
                for (String Location : locationList) {
                    AirmetLocation currentLocation = new AirmetLocation();
                    currentLocation.setLocationLine(lines);
                    currentLocation.setLocation(Location);

                    // Get a latLonPoint for this station ID from "vors"
                    // location table
                    point = LatLonLocTbl.getLatLonPoint(Location, "vors");
                    if (point == null) {
                        hasLocationLine = false;
                        break;
                    }
                    currentLocation.setLatitude(point
                            .getLatitude(LatLonPoint.INDEGREES));
                    currentLocation.setLongitude(point
                            .getLongitude(LatLonPoint.INDEGREES));

                    currentLocation.setIndex(idxLocation + 1);
                    idxLocation++;

                    reportTable.addAirmetLocation(currentLocation);
                }
            } else {
                hasLocationLine = false;
            }

        } else {
            hasLocationLine = false;
        }

        return hasLocationLine;
    }
}
