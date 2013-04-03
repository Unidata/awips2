/**
 * 
 * Convective Sigmet Decoder
 * 
 * This java class decodes CONVSIGMET (convective sigmet) raw data.
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009      87/114			L. Lin     	Initial coding
 * 06/2009		87/114			L. Lin		Increase size of locationLine and location
 *                          				and generalize method "processLocation".
 * 07/2009		87/114			L. Lin		Migration to TO11
 * 07/2011		87/114			F. J. Yen	Fix for RTN TTR 9973--ConvSigmet Decoder Ignoring
 * 											time range--implemented fix from Steven Harris,
 * 											RTN to set the end time range
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.convsigmet.decoder;

import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;
import gov.noaa.nws.ncep.edex.plugin.convsigmet.util.ConvSigmetParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Scanner;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

public class ConvSigmetDecoder extends AbstractDecoder {

    private final String pluginName;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public ConvSigmetDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        String sectionDelim = "OUTLOOK|CONVECTIVE";
        String etx = IDecoderConstants.ETX;
        String theBulletin = null;

        String region = null;
        Calendar startTime = null;
        byte[] messageData = null;

        ConvSigmetRecord record = null;

        ConvSigmetSeparator sep = ConvSigmetSeparator.separate(data, headers);
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

        // record = new ConvsigmetRecord();
        // Decode and set WMO line
        record = ConvSigmetParser.processWMO(theBulletin, headers);

        // Decode the forecast region such as: W, C, or E
        region = ConvSigmetParser.processFcstRegion(theBulletin);
        String forecastRegion = "SIG".concat(region);

        /*
         * Check the Convsigmet record object. If not, throws exception.
         */
        if (record != null) {
            record.setTraceId(traceId);
            record.setPluginName(pluginName);
            record.setReportType(pluginName);
            record.setForecastRegion(forecastRegion);
            try {
                record.constructDataURI();
            } catch (PluginException e) {
                logger.error("Error constructing dataURI", e);
                record = null;
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
                startTime = ConvSigmetParser.processStartTime(theBulletin,
                        headers);

                // Decode the correction flag and set
                Boolean correctionFlag = ConvSigmetParser
                        .processCorrectionFlag(theBulletin);
                Integer correction = 0;
                if (correctionFlag) {
                    correction = 1;
                }
                record.setCorrectionFlag(correction);

                /*
                 * Break the bulletin message into sections by a "OUTLOOK" or
                 * "CONVECTIVE".
                 */
                Scanner sc = new Scanner(theBulletin)
                        .useDelimiter(sectionDelim);

                ArrayList<String> segmentList = new ArrayList<String>();
                segmentList.clear();
                while (sc.hasNext()) {
                    String segment = sc.next();
                    segmentList.add(segment);
                }

                Calendar issueTime = record.getIssueTime();
                /*
                 * process each section in a order of section and location line
                 */
                Calendar min = null;
                Calendar max = null;
                for (String segment : segmentList) {

                    // starts a new section
                    Scanner sc2 = new Scanner(segment);
                    String whatSection = sc2.next();
                    if (whatSection.equals("SIGMET")
                            || whatSection.equals("SIGMET...NONE")) {
                        segment = "CONVECTIVE".concat(segment);

                        // process this section which starts with
                        // "CONVECTIVE SIGMET".
                        ConvSigmetSection section = ConvSigmetParser
                                .processSection(segment, issueTime, startTime,
                                        region, headers);
                        record.addConvSigmetSection(section);

                        // keep track of time range
                        if (min == null || min.getTime().after(section.getStartTime().getTime())) {
                        	min = section.getStartTime();
                        }
                        if (max == null || max.getTime().before(section.getEndTime().getTime())) {
                        	max = section.getEndTime();
                        }
                    } else if (whatSection.equals("VALID")) {
                        segment = "OUTLOOK".concat(segment);

                        // process this section which starts with
                        // "OUTLOOK VALID".
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
                            // only one "AREA" or NIL report
                            ConvSigmetSection outlook = ConvSigmetParser
                                    .processOutLook(segment, region, headers);
                            record.addConvSigmetSection(outlook);
                            if (min == null || min.getTime().after(outlook.getStartTime().getTime())) {
                            	min = outlook.getStartTime();
                            }
                            if (max == null || max.getTime().before(outlook.getEndTime().getTime())) {
                            	max = outlook.getEndTime();
                            }
                        } else {
                            /*
                             * Here are more than one outlook areas Store the
                             * header section and remove it from the outlook
                             * list
                             */
                            String outlookHeader = outlookList.get(0);
                            outlookList.remove(outlookHeader);
                            // process multiple outlook sections
                            for (String outlookSection : outlookList) {
                                outlookSection = outlookHeader.concat("AREA ")
                                        .concat(outlookSection);
                                ConvSigmetSection outlook = ConvSigmetParser
                                        .processOutLook(outlookSection, region,
                                                headers);
                                record.addConvSigmetSection(outlook);
                                if (min == null || min.getTime().after(
                                		outlook.getStartTime().getTime())) {
                                	min = outlook.getStartTime();
                                }
                                if (max == null || max.getTime().before(
                                		outlook.getEndTime().getTime())) {
                                	max = outlook.getEndTime();
                                }
                            }
                        }
                    }
                }
                record.setDataTime(new DataTime(record.getDataTime().getRefTimeAsCalendar(),
                		new TimeRange(min.getTime(),
                		max.getTimeInMillis() - min.getTimeInMillis())));
            } catch (Exception e) {
                logger.error("Error processing decoded sigmet", e);
                record = null;
            }
        }

        /*
         * Return the ConvsigmetRecord record object.
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }

    }

}
