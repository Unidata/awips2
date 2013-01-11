/**
 * 
 * Non-Convective Sigmet Decoder
 * 
 * This java class decodes NONCONVSIGMET (non-convective sigmet) raw data. 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 06/2009		Uma Josyula		Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.edex.plugin.nonconvsigmet.decoder;

import gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet.NonConvSigmetRecord;
import gov.noaa.nws.ncep.edex.plugin.nonconvsigmet.util.NonConvSigmetParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Scanner;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

public class NonConvSigmetDecoder extends AbstractDecoder {

    private final String pluginName;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public NonConvSigmetDecoder(String name) throws DecoderException {
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
            byte[] messageData = null;
        NonConvSigmetRecord currentRecord = null;
            NonConvSigmetSeparator sep = NonConvSigmetSeparator.separate(data,
                    headers);
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
            /*
             * Decode by calling the NonconvSigmetParser method processRecord
             */
            currentRecord = NonConvSigmetParser.processRecord(theBulletin, headers);
            if (currentRecord != null) {
                currentRecord.setReportType(pluginName);
                /*
                 * Replace special characters to a blank so that it may be readable
                 */
                currentRecord.setBullMessage(UtilN
                        .removeLeadingWhiteSpaces((theBulletin.substring(5))
                                .replace('\036', ' ').replace('\r', ' ')
                                .replace('\003', ' ').replace('\000', ' ')
                                .replace('\001', ' ')));
                /*
                 * Check the NonConvsigmet record object. If not, throws exception.
                 */
                currentRecord.setTraceId(traceId);
                currentRecord.setPluginName(pluginName);
                try {
                    currentRecord.constructDataURI();

                } catch (PluginException e) {
                    logger.error(traceId + "- Unable to construct dataURI", e);
                    currentRecord = null;
                }
            }
        /*
         * Return the NonConvsigmetRecord record object.
         */
        if (currentRecord == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { currentRecord };
        }

    }

}