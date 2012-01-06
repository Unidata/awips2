/*
 * 
 * TcmDecoder
 * 
 * This java class decodes TCM (Tropical Cyclone Message) data.
 *  
 * <pre> 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ----- 		------- 	--------	-----------
 * 06/2009		128			T. Lee		Creation
 * 07/2009		128			T. Lee		Migrated to TO11
 * 11/2009		128			T. Lee		Migrated to TO11D6
 * 06/2010		128			T. Lee		Migrated to TO11DR11
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0       
 */

package gov.noaa.nws.ncep.edex.plugin.tcm.decoder;

import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmRecord;
import gov.noaa.nws.ncep.edex.plugin.tcm.util.TcmParser;
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

public class TcmDecoder extends AbstractDecoder {
    private static String pluginName;

    private TcmRecord record;

    private final Logger log = Logger.getLogger(getClass().getName());

    /**
     * Default constructor
     * 
     * @param name
     *            plugin name
     * @throws DecoderException
     */
    public TcmDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        byte[] messageData = null;
        String traceId = null;
        String theBulletin = null;
        Calendar mndTime = null;
        String etx = IDecoderConstants.ETX;

        /*
         * Check if there are more bulletins
         */
        TcmSeparator sep = TcmSeparator.separate(data, headers);
        if (sep.hasNext()) {
            messageData = sep.next();
            String theMessage = new String(messageData);
            record = new TcmRecord();

            /*
             * Exclude the duplicate report after the first
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
            TcmParser tp = new TcmParser();
            try {
                tp.processWMO(messageData, record, mndTime);
            } catch (Exception e) {
                if (log.isInfoEnabled()) {
                    log.info("Process WMO header errors");
                }
            }

            /*
             * Set bulletin message
             */
            record.setBullMessage(theBulletin);

            /*
             * Set report type (upper case)
             */
            record.setReportType("TCM");
        }

        /*
         * Process TCM
         */
        TcmParser tp = new TcmParser();
        tp.processTcm(theBulletin, record);

        /*
         * Return the TcmRecord record object.
         */
        if (record != null) {
            try {
                if (headers != null) {
                    traceId = (String) headers.get("traceId");
                }
                record.setTraceId(traceId);
                record.setPluginName(pluginName);
                record.constructDataURI();

            } catch (PluginException e) {
                throw new DecoderException("Unable to construct dataURI", e);
            }
        }

        /*
         * Decode the precipitation data and return record object
         */
        if (record == null) {
            return new PluginDataObject[0];
        }
        return new PluginDataObject[] { record };
    }
}