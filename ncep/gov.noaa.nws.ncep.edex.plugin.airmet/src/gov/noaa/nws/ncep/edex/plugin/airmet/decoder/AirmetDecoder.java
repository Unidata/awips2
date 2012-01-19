/**
 * 
 * Airmet Decoder
 * 
 * This java class decodes AIRMET raw data.
 * HISTORY
 *
 * * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 05/2009		39				L. Lin		Initial creation
 * 06/2009      39				L. Lin      Set updateNumber before constructing dataURI.	
 * 07/2009		39				L. Lin		Migration to TO11
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.airmet.decoder;

import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetReport;
import gov.noaa.nws.ncep.edex.plugin.airmet.util.AirmetParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

public class AirmetDecoder extends AbstractDecoder {

    private final String pluginName;

    /**
     * Constructor
     * 
     * @param name
     *            The name (usually pluginName) for this decoder.
     * @throws DecoderException
     */
    public AirmetDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    /**
     * 
     * @param data
     * @param headers
     * @return
     * @throws DecoderException
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        String sectionDelim = "OTLK|AIRMET IFR|AIRMET MTN OBSCN|AIRMET TURB|AIRMET ICE|AIRMET STG SFC WNDS|LLWS POTENTIAL]";
        String etx = IDecoderConstants.ETX;
        String theBulletin = null;

        Calendar startTime = null;
        byte[] messageData = null;

        AirmetRecord record = null;
        // Default equal to six hours from start time if there is no valid time
        // in report
        final int validPeriod = 6;

        AirmetSeparator sep = AirmetSeparator.separate(data, headers);
        messageData = sep.next();
        String theMessage = new String(messageData);

        /*
         * May have multiple duplicate bulletins, only get the first bulletin
         * and eliminate the remaining bulletins after the first bulletin.
         */
        Scanner cc = new Scanner(theMessage).useDelimiter(etx);
        if (cc.hasNext()) {
            theBulletin = cc.next();
        } else {
            theBulletin = theMessage;
        }

        // record = new AirmetRecord();
        // Decode and set WMO line
        record = AirmetParser.processWMO(theBulletin, headers);

        Calendar issueTime = record.getIssueTime();

        // Decode the reportName: ZULU, TANGO, or SIERRA
        String reportName = AirmetParser.getReportName(theBulletin);

        /*
         * Check the Airmet record object. If not, throws exception.
         */
        if (record != null) {
            record.setTraceId(traceId);
            record.setPluginName(pluginName);
            record.setReportType(pluginName);
            record.setReportName(reportName);
            // Decode and set the update number
            record.setUpdateNumber(AirmetParser.getUpdateNumber(theBulletin));
            try {
                record.constructDataURI();
            } catch (PluginException e) {
                logger.error("Error constructing dataURI", e);
            }
        }

        if (record != null) {
            try {
                // Replace special characters to a blank so that it may be
                // readable
                record.setBullMessage(UtilN
                        .removeLeadingWhiteSpaces((theBulletin.substring(5))
                                .replace('\036', ' ').replace('\r', ' ')
                                .replace('\003', ' ').replace('\000', ' ')
                                .replace('\001', ' ')));

                // Decode the starting time
                startTime = AirmetParser.getStartTime(theBulletin, headers);
                if (startTime == null) {
                    startTime = issueTime;
                }

                // Decode the end time
                Calendar endTime = AirmetParser
                        .getEndTime(theBulletin, headers);
                if (endTime == null) {
                    /*
                     * if no end time available, end time will be the start time
                     * plus a valid period; the default is six hours for now.
                     */
                    endTime = startTime;
                    endTime.add(Calendar.HOUR, validPeriod);
                }

                Calendar validStartTime = TimeTools.copy(endTime);
                validStartTime.add(Calendar.HOUR, -validPeriod);

                // Airmet report valid time lasts only six hours from start to
                // end
                if (startTime.before(validStartTime)) {
                    startTime = validStartTime;
                }

                // Decode the correction flag and set
                record.setCorrectionFlag(AirmetParser
                        .getCorrectionFlag(theBulletin));

                /*
                 * Break the bulletin message into sections by a "OTLK" or
                 * "AIRMET [IFR|MTN OBSCN|TURB|ICE|STG SFC WNDS|LLWS PTENTIAL]".
                 */
                Scanner sc = new Scanner(theBulletin)
                        .useDelimiter(sectionDelim);

                ArrayList<String> segmentList = new ArrayList<String>();
                segmentList.clear();
                // throw away the prefix section
                String segPrefix = sc.next();

                while (sc.hasNext()) {
                    String segment = sc.next();
                    segmentList.add(segment);
                }

                if (segmentList.size() == 0) {
                    // This is NIL/EXPIRE AIRMET report containing header only.
                    record.setCorrectionFlag(4);
                } else {
                    /*
                     * Process each report.
                     */
                    Integer series = 1;
                    // Get forecastRegion
                    String forecastRegion = AirmetParser.getRegion(reportName);
                    // Get valid day
                    String validDay = AirmetParser.getValidDay(theBulletin);

                    // System.out.println("Process a report=\n" + segment);
                    for (String segment : segmentList) {
                        // starts a new section
                        Scanner sc2 = new Scanner(segment);
                        String whatReport = sc2.next();

                        if (whatReport.equals("VALID")) {
                            segment = "OTLK".concat(segment);

                            // process this section which starts with
                            // "OTLK VALID".
                            Scanner scOutlook = new Scanner(segment)
                                    .useDelimiter("AREA ");
                            ArrayList<String> outlookList = new ArrayList<String>();
                            outlookList.clear();

                            // Check if "OUTLOOK" section contains more than one
                            // "AREA".
                            while (scOutlook.hasNext()) {
                                String outlookSegment = scOutlook.next();
                                outlookList.add(outlookSegment);
                            }

                            if (outlookList.size() <= 1) {
                                // only one outlook report
                                AirmetReport outlook = AirmetParser
                                        .processOutLook(segment, forecastRegion);
                                AirmetParser.processValidTime(segment, outlook,
                                        validDay, headers);
                                record.addAirmetReport(outlook);
                            } else {
                                /*
                                 * Here are more than one outlook areas Store
                                 * the header section and remove it from the
                                 * outlook list
                                 */
                                String outlookHeader = outlookList.get(0);
                                outlookList.remove(outlookHeader);
                                // process multiple outlook sections
                                for (String outlookReport : outlookList) {
                                    outlookReport = outlookHeader.concat(
                                            "AREA ").concat(outlookReport);
                                    AirmetReport outlook = AirmetParser
                                            .processOutLook(outlookReport,
                                                    forecastRegion);
                                    AirmetParser.processValidTime(
                                            outlookReport, outlook, validDay,
                                            headers);
                                    record.addAirmetReport(outlook);
                                }
                            }
                        } else {
                            // Regular expression AIRMET report
                            final String HAZARD_EXP = "(ICE|TURB|CIG BLW|VIS BLW|MTNS|LLWS|SUSTAINED) ";

                            // Pattern used for extracting hazard
                            final Pattern hazardTypePattern = Pattern
                                    .compile(HAZARD_EXP);
                            Matcher theMatcher = hazardTypePattern
                                    .matcher(segment);

                            if (theMatcher.find()) {
                                // prefix the report type
                                if (theMatcher.group(1).equals("ICE")) {
                                    segment = "AIRMET ICE".concat(segment);
                                } else if (theMatcher.group(1).equals("TURB")) {
                                    segment = "AIRMET TURB".concat(segment);
                                } else if (theMatcher.group(1)
                                        .equals("CIG BLW")) {
                                    segment = "AIRMET IFR".concat(segment);
                                } else if (theMatcher.group(1)
                                        .equals("VIS BLW")) {
                                    segment = "AIRMET IFR".concat(segment);
                                } else if (theMatcher.group(1).equals("MTNS")) {
                                    segment = "AIRMET MTN OBSCN"
                                            .concat(segment);
                                } else if (theMatcher.group(1).equals(
                                        "SUSTAINED")) {
                                    segment = "AIRMET STG SFC WNDS"
                                            .concat(segment);
                                } else if (theMatcher.group(1).equals("LLWS")) {
                                    segment = "LLWS POTENTIAL".concat(segment);
                                }
                            }

                            // process this report which starts with "AIRMET" or
                            // "LLWS".
                            AirmetReport report = AirmetParser
                                    .processReport(segment);
                            if (report != null) {
                                // Decode and set the sequenceID
                                report.setSequenceID(AirmetParser
                                        .getSequenceID(theBulletin, series));
                                report.setStartTime(startTime);
                                report.setEndTime(endTime);
                                record.addAirmetReport(report);
                            } else {
                                logger.error("Error decoding segment "
                                        + segment);
                            }
                        }
                        series++;
                    }
                }

            } catch (Exception e) {
                logger.error("Error postprocessing airmet", e);
                record = null;
            }
        }

        /*
         * Return the AirmetRecord record object.
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }

    }

}
