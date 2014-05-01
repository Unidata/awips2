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
package com.raytheon.edex.plugin.bufrmos;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.plugin.bufrmos.common.BufrMosData;
import com.raytheon.edex.plugin.bufrmos.decoder.BufrMOSDataAdapter;
import com.raytheon.uf.common.dataplugin.PluginDataObject;

/**
 * Decoder strategy for BUFR Model Output Statistics data. Most common usage is
 * as follows.</BR><code>
 *   BufrMosDecoder dec = new BufrMosDecoder();
 *   dec.setMessage();
 *   while(dec.hasNext())
 *   {
 *      PluginDataObject r = dec.decode();
 *      // do something with record.
 *   }
 * </code>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2008 862        jkorman     Initial Coding.
 * Apr 08, 2008 1039       jkorman     Added traceId for tracing data.
 * Dec 08, 2008            chammack    Camel Refactor
 * May 14, 2013 1869       bsteffen    Remove DataURI column from bufrmos.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BufrMosDecoder extends AbstractDecoder {

    // Name of the plugin controlling this decoder.
    public static final String PLUGIN_NAME = "bufrmos";

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    /**
     * Empty constructor required by DecoderFactory.
     * 
     * @throws DecoderException
     */
    public BufrMosDecoder() throws DecoderException {
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] messageData, Headers headers)
            throws DecoderException {
        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        PluginDataObject[] decodedData = null;

        BufrMosSeparator separator = new BufrMosSeparator();
        try {
            if (messageData != null) {
                separator.setData(messageData, headers);

                List<BufrMosData> pdoList = new ArrayList<BufrMosData>(
                        separator.size());
                MOSPointDataState pds = new MOSPointDataState();

                while (separator.hasNext()) {
                    try {
                        BufrMosData fcstData = BufrMOSDataAdapter
                                .createMOSData(separator, pds);

                        if (fcstData != null) {
                            fcstData.setTraceId(traceId);
                            fcstData.setPersistenceTime(new Date());
                            pdoList.add(fcstData);
                        }
                    } catch (Exception e) {
                        logger.error("Error decoding bufrmos data", e);
                    }
                }
                if (pdoList.size() > 0) {
                    decodedData = pdoList.toArray(new PluginDataObject[pdoList
                            .size()]);
                } else {
                    decodedData = new PluginDataObject[0];
                }
            } else {
                decodedData = new PluginDataObject[0];
                logger.info(traceId
                        + "-No data to decode while creating BufrMosSeparator");
            }
        } catch (Exception e) {
            decodedData = new PluginDataObject[0];
            logger.error(traceId + "- Error decoding data", e);
        }

        return decodedData;

    }

}
