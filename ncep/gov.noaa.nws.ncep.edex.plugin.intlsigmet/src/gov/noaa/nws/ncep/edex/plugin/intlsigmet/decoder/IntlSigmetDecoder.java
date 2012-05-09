/**
 * 
 * IntlSigmet Decoder
 * 
 * This java class decodes INTLSIGMET raw data.
 * HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 06/2009		113				L. Lin		Initial creation
 * 07/2009		113				L. Lin		Migration to TO11
 * 05/2010      113             L. Lin      Migration to TO11DR11
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.intlsigmet.decoder;

import gov.noaa.nws.ncep.common.dataplugin.intlsigmet.IntlSigmetRecord;
import gov.noaa.nws.ncep.edex.plugin.intlsigmet.util.IntlSigmetParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;
import java.util.Scanner;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

public class IntlSigmetDecoder extends AbstractDecoder {

    private final String pluginName;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public IntlSigmetDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        String etx = IDecoderConstants.ETX;
        String theBulletin = null;

        Calendar startTime = null;
        byte[] messageData = null;

        IntlSigmetRecord record = null;
        // Default equal to four hours from start time if there is no valid time
        // in report
        final int validPeriod = 4;

        IntlSigmetSeparator sep = IntlSigmetSeparator.separate(data, headers);
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

        // Decode and set WMO line
        record = IntlSigmetParser.processWMO(theBulletin, headers);

        /*
         * Check the IntlSigmet record object. If not, throws exception.
         */
        if (record != null) {
            record.setTraceId(traceId);
            record.setPluginName(pluginName);
            record.setReportType(pluginName);
            record.setHazardType(IntlSigmetParser.getHazardType(theBulletin));
            // Decode and set the messageID
            record.setMessageID(IntlSigmetParser.getMessageID(theBulletin));
            // Decode and set the sequence number
            record.setSequenceNumber(IntlSigmetParser
                    .getSequenceNumber(theBulletin));
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

                if (!record.getHazardType().equals("NIL")) {

                    // Decode the issue time
                    Calendar issueTime = record.getIssueTime();

                    // Decode the starting time
                    startTime = IntlSigmetParser.getStartTime(theBulletin,
                            headers);
                    if (startTime == null) {
                        startTime = issueTime;
                    }

                    // Decode the end time
                    Calendar endTime = IntlSigmetParser.getEndTime(theBulletin,
                            headers);
                    if (endTime == null) {
                        /*
                         * if no end time available, end time will be the start
                         * time plus a valid period; the default is four hours
                         * for now.
                         */
                        endTime = TimeTools.copy(startTime);
                        endTime.add(Calendar.HOUR, validPeriod);
                    }

                    // Set start time and end time
                    record.setStartTime(startTime);
                    record.setEndTime(endTime);

                    // Decode and set the atsu
                    record.setAtsu(IntlSigmetParser.getAtsu(theBulletin));

                    // Decode and set the intensity
                    record.setIntensity(IntlSigmetParser
                            .getIntensity(theBulletin));

                    // Decode and set the flight levels
                    IntlSigmetParser.processFlightLevels(theBulletin, record);

                    // Decode and set the omwo
                    record.setOmwo(IntlSigmetParser.getOmwo(theBulletin));

                    // Decode and set the direction
                    record.setDirection(IntlSigmetParser
                            .getDirection(theBulletin));

                    // Decode and set the speed
                    record.setSpeed(IntlSigmetParser.getSpeed(theBulletin));
                    
                    // Decode and set distance
                    record.setDistance(IntlSigmetParser
                            .getDistance(theBulletin));

                    // Decode and set nameLocation
                    record.setNameLocation(IntlSigmetParser
                            .getNameLocation(theBulletin));

                    // Decode and set the remarks
                    record.setRemarks(IntlSigmetParser.getRemarks(theBulletin));

                    // Decode and set polyGonExtend
                    record.setPolygonExtent(IntlSigmetParser.getPolygonExtent(theBulletin));
                }
            } catch (Exception e) {
                logger.error("Error postprocessing " + pluginName, e);
                record = null;
            }
        }

        /*
         * Return the IntlSigmetRecord record object.
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }

    }

}
