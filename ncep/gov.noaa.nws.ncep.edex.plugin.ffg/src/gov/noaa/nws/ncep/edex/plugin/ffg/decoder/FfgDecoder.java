/*
 * 
 * FfgDecoder
 * 
 * This java class decodes FFG (Flash Flood Guidance) data.
 *  
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date         Ticket#  Engineer    Description
 * ------------ -------- ----------- --------------------------
 * 08/2008      14       T. Lee      Initial coding
 * 12/2008      14       T. Lee      Changed FfgSeparator to
 *                                   DecoderSeparator
 * 03/2009      14       T. Lee      Migration to TO10
 * 07/2009      14       T. Lee      Migration to TO11
 * 11/2009      14       T. Lee      Migration to TO11D6
 * 05/2010      14       T. Lee      Migration to TO11DR11
 * 06/2010      14       T. Lee      Added traceId output
 * Aug 30, 2013 2298     rjpeter     Make getPluginName abstract
 * Jan 07, 2013          njensen     Null check on traceId
 * </pre>
 *
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ffg.decoder;

import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgRecord;
import gov.noaa.nws.ncep.edex.plugin.ffg.util.FfgParser;
import gov.noaa.nws.ncep.edex.tools.decoder.MndTime;

import java.util.Calendar;
import java.util.Scanner;

import org.apache.log4j.Logger;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

public class FfgDecoder extends AbstractDecoder {
    private FfgRecord record;

    private Calendar mndTime = null;

    private final Logger log = Logger.getLogger(getClass().getName());

    private String lastTraceId = "";

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        byte[] messageData = null;
        String theBulletin = null;
        String traceId = "";
        String etx = IDecoderConstants.ETX;
        Boolean output = false;

        if (headers != null) {
            traceId = (String) headers.get("traceId");
            if (traceId != null && traceId.compareTo(lastTraceId) != 0) {
                System.out
                        .println(" Start decode FFG file: " + traceId + " \n");
                lastTraceId = traceId;
                output = true;
            }
        }
        /*
         * Check if there are more bulletins
         */
        FfgSeparator sep = FfgSeparator.separate(data, null);
        if (sep.hasNext()) {
            messageData = sep.next();
            String theMessage = new String(messageData);
            record = new FfgRecord();

            /*
             * Exclude the duplicate report after the first ^C
             */
            Scanner cc = new Scanner(theMessage).useDelimiter(etx);
            if (cc.hasNext()) {
                theBulletin = cc.next();
            } else {
                theBulletin = theMessage;
            }

            /*
             * Set MND (Mass News Disseminator) time string and convert it into
             * Calendar object
             */
            MndTime mt = new MndTime(messageData);
            record.setMndTime(mt.getMndTimeString());
            mndTime = mt.getMndTime();

            /*
             * Process WMO header
             */
            try {
                FfgParser.processWMO(messageData, record, mndTime);
            } catch (Exception e) {
                if (log.isInfoEnabled()) {
                    log.info("Process WMO header errors");
                }
            }

            /*
             * Set AWIPS identifier
             */
            record.setAwipsID(FfgParser.processAwipsID(theBulletin));

            /*
             * Set bulletin message
             */
            record.setBullMessage(theBulletin);

            /*
             * Set report type (upper case)
             */
            record.setReportType("FFG");
        }

        /*
         * Return the FfgRecord record object.
         */
        if (record != null) {
            try {
                record.setTraceId(traceId);
                record.constructDataURI();
            } catch (PluginException e) {
                throw new DecoderException("Unable to construct dataURI", e);
            }
        }

        /*
         * Decode the precipitation data and return record object
         */
        FfgParser.processPrecip(theBulletin, record);

        if (output) {
            System.out.println(" Finish decode FFG file: " + traceId);
        }

        if (record == null) {
            return new PluginDataObject[0];
        }
        return new PluginDataObject[] { record };
    }
}
