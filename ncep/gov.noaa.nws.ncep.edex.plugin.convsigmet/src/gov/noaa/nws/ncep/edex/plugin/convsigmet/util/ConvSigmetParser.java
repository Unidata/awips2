/**
 * Convsigmet DecoderUtil
 * 
 * This java class intends to serve as a decoder utility for Convsigmet.
 * 
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009      87/114			L. Lin     	Initial coding
 * 06/2009		87/114			L. Lin		Generalize the method processLocation.
 * 07/2009		87/114			L. Lin		Migration to TO11
 * 09/2009		87/114			L. Lin		Add latitude/longitude to location table
 * 07/2011		87/114			F. J. Yen	Fix the day of the end time when it is 
 * 											not the same as the day of the start time.
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.convsigmet.util;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;
import gov.noaa.nws.ncep.edex.tools.decoder.LatLonLocTbl;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

public class ConvSigmetParser {

    /**
     * Constructor
     */
    public ConvSigmetParser() {
    }

    /**
     * Parse the WMO line and store WMO header, OfficeID, issue time,
     * designatorBBB,...
     * 
     * @param wmoline
     *            The bulletin message
     * 
     * @return a ConvsigmetRecord
     */
    public static ConvSigmetRecord processWMO(String wmoline, Headers headers) {

        ConvSigmetRecord record = null;
        // Regular expression for WMO/ICAO, station ID, and issue date (and
        // maybe designator BBB)
        final String WMO_EXP = "([A-Z]{4}[0-9]{2}) ([A-Z]{4}) ([0-9]{6})( ([A-Z]{3}))?";

        // Pattern used for extracting WMO header, officeID, product purge time,
        // and issue date and designatorBBB
        final Pattern wmoPattern = Pattern.compile(WMO_EXP);
        Matcher theMatcher = wmoPattern.matcher(wmoline);

        if (theMatcher.find()) {
            record = new ConvSigmetRecord();

            record.setWmoHeader(theMatcher.group(1));
            record.setIssueOffice(theMatcher.group(2));
            record.setDesignatorBBB(theMatcher.group(5));

            // Decode the issue time.
            Calendar issueTime = TimeTools.findDataTime(theMatcher.group(3),
                    headers);
            record.setIssueTime(issueTime);

            DataTime dataTime = new DataTime(issueTime);
            record.setDataTime(dataTime);
        }
        return record;
    }

    /**
     * Obtains forecast region as: SIGW, SIGC, or SIGE from a report
     * 
     * @param bullMessage
     *            The bulletin message
     * @return a string for forecast region
     */
    public static String processFcstRegion(String bullMessage) {

        // Regular expression for "SIG" forecast region
        final String FCSTREGION_EXP = "SIG([A-Z]{1}) ( )*";

        // Pattern used for extracting forecast region
        final Pattern fcstregionPattern = Pattern.compile(FCSTREGION_EXP);
        Matcher theMatcher = fcstregionPattern.matcher(bullMessage);

        if (theMatcher.find()) {
            return theMatcher.group(1);
        } else {
            return " ";
        }
    }

    /**
     * process regular section of a convective sigmet report
     * 
     * @param theSection
     *            The section lines from bulletin message
     * @param issueTime
     *            The bulletin issue time
     * @param startTime
     *            The start time for this section
     * @param forecastRegion
     *            as "W", "C", or "E"
     * @return a ConvsigmetSection table
     */
    public static ConvSigmetSection processSection(String theSection,
            Calendar issueTime, Calendar startTime, String forecastRegion,
            Headers headers) {

        // Default equal to one hour if there is no valid time in report
        final int validPeriod = 1;

        // Decode the phenomena description
        ConvSigmetSection currentSection = ConvSigmetParser
                .processPhenomena(theSection);

        // Replace the special characters
        currentSection.setSegment(theSection.replace('\r', ' ')
                .replace('\036', ' ').replace('\003', ' ').replace('\000', ' ')
                .replace('\001', ' '));

        // Decode the locations
        ConvSigmetParser.processLocation(theSection, currentSection);

        // Decode the sequence ID
        String sequenceID = ConvSigmetParser.processSequenceID(theSection);
        if (sequenceID.equals(" ")) {
            // Default as: 0W, 0C, or 0E if no sequenceID has been found.
            sequenceID = "0".concat(forecastRegion);
        }
        currentSection.setSequenceID(sequenceID);

        // Set the starting time
        if (startTime == null) {
            currentSection.setStartTime(issueTime);
        } else {
            currentSection.setStartTime(startTime);
        }

        // Decode the end time
        Calendar endTime = ConvSigmetParser.processEndTime(theSection,
                currentSection, headers);
        if (endTime == null) {
            /*
             * if no end time available, end time will be the issue time plus a
             * valid period; the default is one hour for now.
             */
            endTime = TimeTools.copy(issueTime);
            endTime.add(Calendar.HOUR, validPeriod);
            currentSection.setEndTime(endTime);
        } else {
            currentSection.setEndTime(endTime);
        }

        int startMonth = startTime.get(Calendar.MONTH);
        int endMonth = endTime.get(Calendar.MONTH);
        int startDay = startTime.get(Calendar.DAY_OF_MONTH);
        int endDay = endTime.get(Calendar.DAY_OF_MONTH);
        if ((startMonth == endMonth) && (startDay > endDay)) {
            // In case the end time needs to advance one month
            endTime.add(Calendar.MONTH, 1);
        }

        return currentSection;
    }

    /**
     * process the outlook section of a convective sigmet report.
     * 
     * @param theOutlook
     *            The outlook section lines from bulletin message
     * @param forecastRegion
     *            as "W", "C", or "E"
     * @return a ConvsigmetSection table
     */
    public static ConvSigmetSection processOutLook(String theOutlook,
            String forecastRegion, Headers headers) {

        ConvSigmetSection currentOutLook = new ConvSigmetSection();

        currentOutLook.setSegment(theOutlook.replace('\r', ' ')
                .replace('\036', ' ').replace('\003', ' ').replace('\000', ' ')
                .replace('\001', ' '));
        currentOutLook.setClassType("OUTLOOK");

        // Decode start time and end time
        ConvSigmetParser.processValidTime(theOutlook, currentOutLook, headers);

        // Decode locations
        if (ConvSigmetParser.processLocation(theOutlook, currentOutLook)) {
            final String SID_EXP = "AREA ([0-9]{1})...FROM";

            // Pattern used for extracting the number of sequence ID.
            final Pattern sidPattern = Pattern.compile(SID_EXP);

            Matcher theMatcher = sidPattern.matcher(theOutlook);

            // Default sequenceID as: 1W, 1C, or 1E if locations have been
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

        return currentOutLook;
    }

    /**
     * Obtains start time from input report
     * 
     * @param theSection
     *            The bulletin message
     * @return a calendar for start time
     */
    public static Calendar processStartTime(String theSection, Headers headers) {

        // Regular expression for start time
        final String STARTTIME_EXP = "(\\x1e)([A-Z]{4}) ([A-Z]{3}) ([0-9]{6})( [A-Z]{3})?";

        // attern used for extracting the starting time
        final Pattern starttimePattern = Pattern.compile(STARTTIME_EXP);

        // Calendar mndTime = Calendar.getInstance();

        Matcher theMatcher = starttimePattern.matcher(theSection);

        if (theMatcher.find()) {
            // Get start time
            Calendar mndTime = null;
            return TimeTools.findDataTime(theMatcher.group(4), headers);
        } else {
            return null;
        }
    }

    /**
     * Obtains correction flag
     * 
     * @param theSection
     *            The bulletin message
     * @return true if finds a correction flag
     */
    public static Boolean processCorrectionFlag(String theSection) {

        Boolean correctionFlag = false;

        // Regular expression for the correction flag
        final String CORFLAG_EXP = "(\\x1e)([A-Z]{4}) ([A-Z]{3}) ([0-9]{6})( [A-Z]{3})?";

        // Pattern used for extracting the correction flag
        final Pattern corflagPattern = Pattern.compile(CORFLAG_EXP);

        Matcher theMatcher = corflagPattern.matcher(theSection);

        if (theMatcher.find()) {
            Scanner scCorrection = new Scanner(theMatcher.group(0));

            // check if this contains a correction flag
            while (scCorrection.hasNext()) {
                if (scCorrection.next().equals("COR")) {
                    correctionFlag = true;
                }
            }

        }
        return correctionFlag;
    }

    /**
     * Obtains the sequence ID
     * 
     * @param inSegment
     *            The segment which contains this sequence ID
     * @return a string for sequence ID
     */
    public static String processSequenceID(String inSegment) {

        // Regular expression for the sequence ID
        final String SEQUENCEID_EXP = "CONVECTIVE SIGMET (([0-9]{2}|[0-9]{1})[A-Z]{1})";

        // Pattern used for extracting the sequence ID
        final Pattern sequenceidPattern = Pattern.compile(SEQUENCEID_EXP);
        Matcher theMatcher = sequenceidPattern.matcher(inSegment);

        if (theMatcher.find()) {
            return theMatcher.group(1);
        } else {
            return " ";
        }
    }

    /**
     * Get the end time
     * 
     * @param theSegment
     *            The segment which contains end time
     * @param section
     *            The section table which contains the start time
     * @return a calendar for end time
     */
    public static Calendar processEndTime(String theSegment,
            ConvSigmetSection section, Headers headers) {

        // Regular expression for end time
        final String ENDTIME_EXP = "VALID UNTIL ([0-9]{4})Z";

        // Pattern used for extracting the end time
        final Pattern endtimePattern = Pattern.compile(ENDTIME_EXP);

        // Calendar mndTime = Calendar.getInstance();

        Matcher theMatcher = endtimePattern.matcher(theSegment);

        if (theMatcher.find()) {
            Calendar startTime = section.getStartTime();
            
            String endTimeGroup = Integer.toString(
                    startTime.get(Calendar.DAY_OF_MONTH)).concat(
                    theMatcher.group(1));
            if (startTime.get(Calendar.DAY_OF_MONTH) < 10) {
                // add a "0" if the day of month less than 10
                endTimeGroup = "0".concat(endTimeGroup);
            }
            // Determine the end time.
            Calendar endTime = TimeTools.findDataTime(endTimeGroup, headers);
            int startHrMn = startTime.get(Calendar.HOUR_OF_DAY) * 100 +
            				startTime.get(Calendar.MINUTE);
            int endHrMn = Integer.parseInt(theMatcher.group(1));
            if (endHrMn < startHrMn) {
            	/*   Increment for the next day */
            	endTime.add(Calendar.DATE,1); 
            }
            return endTime;
        } else {
            return null;
        }
    }

    /**
     * Parse the phenomena...
     * 
     * @param theFlight
     *            The flight level line
     * @return a section record
     */
    public static ConvSigmetSection processPhenomena(String theFlight) {

        String classType = null;

        // Regular expression for the flight line
        final String FL_EXP = "((DMSHG|DSIPTG|INTSFYG|DVLPG) )?(LINE|AREA|ISOL)(\\S|\\s)*FROM ([0-9]{3})?([0-9]{2})KT. ((TOP|TOPS) (TO|ABV)) FL([0-9]{3})";

        // Pattern used for extracting the direction, speed, distance, and
        // flight level
        final Pattern flPattern = Pattern.compile(FL_EXP);
        Matcher theMatcher = flPattern.matcher(theFlight);

        // Regular expression for the classType line
        final String CLASS_EXP = "((DMSHG|DSIPTG|INTSFYG|DVLPG) )?(LINE|AREA|ISOL)(\\S|\\s)*((TOP|TOPS) (TO|ABV)) FL([0-9]{3})";

        // Pattern used for extracting the class type and flight level
        final Pattern classPattern = Pattern.compile(CLASS_EXP);
        Matcher classMatcher = classPattern.matcher(theFlight);

        ConvSigmetSection currentSection = new ConvSigmetSection();

        if (theMatcher.find()) {
            classType = theMatcher.group(3);

            int direction = Integer.parseInt(theMatcher.group(5));
            int speed = Integer.parseInt(theMatcher.group(6));
            int flightLevel = Integer.parseInt(theMatcher.group(10));
            String flightLevelTop = theMatcher.group(7);
            String intensity = theMatcher.group(1);

            currentSection.setFlightLevel(flightLevel);
            currentSection.setDirection(direction);
            currentSection.setSpeed(speed);
            currentSection.setClassType(classType);
            currentSection.setIntensity(intensity);
            currentSection.setCloudTop(flightLevelTop);

            if (classType.equals("LINE")) {
                final String DISTANCE_EXP = "LINE (EMBD )?(SEV )?TS ([0-9]{2})";
                final Pattern distancePattern = Pattern.compile(DISTANCE_EXP);
                Matcher disMatcher = distancePattern.matcher(theFlight);
                if (disMatcher.find()) {
                    int distance = Integer.parseInt(disMatcher.group(3));
                    currentSection.setDistance(distance);
                }
            } else if (classType.equals("ISOL")) {
                final String ISOLDISTANCE_EXP = "ISOL (\\S|\\s)* TS D([0-9]{2})";
                final Pattern isoldistancePattern = Pattern
                        .compile(ISOLDISTANCE_EXP);
                Matcher isoldisMatcher = isoldistancePattern.matcher(theFlight);
                if (isoldisMatcher.find()) {
                    int distance = Integer.parseInt(isoldisMatcher.group(2));
                    currentSection.setDistance(distance);
                }
                // get ISOL location table
                getIsolLocation(theFlight, currentSection);
            }

        } else if (classMatcher.find()) {
            classType = classMatcher.group(3);

            int flightLevel = Integer.parseInt(classMatcher.group(8));
            String flightLevelTop = classMatcher.group(5);
            String intensity = classMatcher.group(1);

            currentSection.setFlightLevel(flightLevel);
            currentSection.setClassType(classType);
            currentSection.setIntensity(intensity);
            currentSection.setCloudTop(flightLevelTop);

            if (classType.equals("LINE")) {
                final String DISTANCE_EXP = "LINE (EMBD )?(SEV )?TS ([0-9]{2})";
                final Pattern distancePattern = Pattern.compile(DISTANCE_EXP);
                Matcher disMatcher = distancePattern.matcher(theFlight);
                if (disMatcher.find()) {
                    int distance = Integer.parseInt(disMatcher.group(3));
                    currentSection.setDistance(distance);
                }
            } else if (classType.equals("ISOL")) {
                final String ISOLDISTANCE_EXP = "ISOL (\\S|\\s)* TS D([0-9]{2})";
                final Pattern isoldistancePattern = Pattern
                        .compile(ISOLDISTANCE_EXP);
                Matcher isoldisMatcher = isoldistancePattern.matcher(theFlight);
                if (isoldisMatcher.find()) {
                    int distance = Integer.parseInt(isoldisMatcher.group(2));
                    currentSection.setDistance(distance);
                }
                // get ISOL location table
                getIsolLocation(theFlight, currentSection);
            }

        } else {
            // Finds no "flight level line", "CS" as default for a NIL report.
            currentSection.setClassType("CS");
            currentSection.setIntensity(null);
        }

        return currentSection;
    }

    /**
     * Obtains the start time and end time
     * 
     * @param theSection
     *            The outlook section contains the valid times group
     */
    public static void processValidTime(String theSection,
            ConvSigmetSection currentOutLook, Headers headers) {

        // Regular expression for start time
        final String TIMES_EXP = "OUTLOOK VALID ([0-9]{6})-([0-9]{6})";

        // Pattern used for extracting the starting time and end time
        final Pattern starttimePattern = Pattern.compile(TIMES_EXP);

        Calendar startTime = null;
        Calendar endTime = null;
        // Calendar mndTime = Calendar.getInstance();

        Matcher theMatcher = starttimePattern.matcher(theSection);

        if (theMatcher.find()) {
            // Decode the start time and end time; then set them.
            startTime = TimeTools.findDataTime(theMatcher.group(1), headers);
            currentOutLook.setStartTime(startTime);
            endTime = TimeTools.findDataTime(theMatcher.group(2), headers);

            int startMonth = startTime.get(Calendar.MONTH);
            int endMonth = endTime.get(Calendar.MONTH);
            int startDay = startTime.get(Calendar.DAY_OF_MONTH);
            int endDay = endTime.get(Calendar.DAY_OF_MONTH);

            if ((startMonth == endMonth) && (startDay > endDay)) {
                // Roll over a month
                endTime.add(Calendar.MONTH, 1);
            }

            currentOutLook.setEndTime(endTime);
        } else {
            currentOutLook.setStartTime(startTime);
            currentOutLook.setEndTime(endTime);
        }

    }

    /**
     * Obtains the location information for ISOL.
     * 
     * @param theSection
     *            The section lines from bulletin message
     * @param sectionTable
     *            The section Table
     */
    public static void getIsolLocation(String theSection,
            ConvSigmetSection sectionTable) {

        String line = null;
        LatLonPoint point = null;

        final String ISOLLOCATION_EXP = "\\x0d\\x0d\\x0a(([0-9]{2}|[0-9]{3})([ENSW])* [A-Z]{3})\\x0d\\x0d\\x0a";
        final Pattern isollocationPattern = Pattern.compile(ISOLLOCATION_EXP);
        Matcher isollocMatcher = isollocationPattern.matcher(theSection);
        if (isollocMatcher.find()) {
            line = isollocMatcher.group(1);
        }

        if (line != null) {
            ConvSigmetLocation currentLocation = new ConvSigmetLocation();
            currentLocation.setLocationLine(line);
            currentLocation.setLocation(line);
            currentLocation.setIndex(1);
            // Get a latLonPoint for this station ID from "vors" location table
            point = LatLonLocTbl.getLatLonPoint(line, "vors");
            currentLocation.setLatitude(point
                    .getLatitude(LatLonPoint.INDEGREES));
            currentLocation.setLongitude(point
                    .getLongitude(LatLonPoint.INDEGREES));
            sectionTable.addConvSigmetLocation(currentLocation);
        }
    }

    /**
     * Parse the location lines and add location table to the section table if
     * any
     * 
     * @param theSection
     *            The section lines from bulletin message
     * @param sectionTable
     *            The section Table
     * @return true if finds a location line
     */
    public static Boolean processLocation(String theSection,
            ConvSigmetSection sectionTable) {

        Boolean hasLocationLine = true;
        String locationDelimiter = "-";

        ArrayList<String> terminationList = new ArrayList<String>();
        terminationList.addAll(Arrays
                .asList(new String[] { "WST", "REF", "LINE", "AREA", "ISOL",
                        "DMSHG", "DVLPG", "DSIPTG", "INTSFYG" }));

        LatLonPoint point = null;

        Scanner sclocations = new Scanner(theSection).useDelimiter("FROM");

        // throws away the first section which is not the "FROM" location
        String locationSection = sclocations.next();

        if (sclocations.hasNext()) {
            locationSection = sclocations.next();

            Scanner scLocationLine = new Scanner(locationSection)
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
            // Parse the location lines by a "-"
            Scanner scLocation = new Scanner(lines)
                    .useDelimiter(locationDelimiter);
            locationList.clear();
            // Get all locations
            while (scLocation.hasNext()) {
                locationList.add(scLocation.next());
            }

            // set locations to data base
            if (locationList.size() > 1) {
                Integer idxLocation = 0;
                for (String Location : locationList) {

                    ConvSigmetLocation currentLocation = new ConvSigmetLocation();
                    currentLocation.setLocationLine(lines);
                    currentLocation.setLocation(Location);

                    // Get a latLonPoint for this station ID from "vors"
                    // location table
                    point = LatLonLocTbl.getLatLonPoint(Location, "vors");
                    currentLocation.setLatitude(point
                            .getLatitude(LatLonPoint.INDEGREES));
                    currentLocation.setLongitude(point
                            .getLongitude(LatLonPoint.INDEGREES));

                    currentLocation.setIndex(idxLocation + 1);
                    idxLocation++;

                    sectionTable.addConvSigmetLocation(currentLocation);
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
