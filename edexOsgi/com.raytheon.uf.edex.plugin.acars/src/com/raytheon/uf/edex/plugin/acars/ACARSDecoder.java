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
package com.raytheon.uf.edex.plugin.acars;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.edex.decodertools.core.IDecoderInput;
import com.raytheon.uf.edex.plugin.acars.decoder.ACARSDataAdapter;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009       1939 jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSDecoder extends AbstractDecoder {

    private Log logger = LogFactory.getLog(getClass());

    private String pluginName = "acars";

    /**
     * 
     * @param name
     */
    public ACARSDecoder(String name) {
        this.pluginName = name;
    }

    /**
     * Get the next decoded data record.
     * 
     * @return One record of decoded data.
     * @throws DecoderException
     *             Thrown if no data is available.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {

        PluginDataObject[] decodedData = null;

        ACARSDataAdapter adapter = new ACARSDataAdapter(pluginName);

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        try {
            decodedData = adapter.getACARSData(data, traceId, headers);
            if (decodedData != null) {
                logger.info(String.format("%s - Decoded %d obs", traceId,
                        decodedData.length));
            } else {
                logger.info(String.format("%s - Decoded no obs", traceId));
            }
        } catch (Exception e) {
            logger.error("Error in ACARSDecoder", e);
        } finally {
            if (decodedData == null) {
                decodedData = new PluginDataObject[0];
            }
        }

        return decodedData;
    }

    /**
     * @param input
     * @param decodeProperties
     */
    public PluginDataObject[] decodeInput(IDecoderInput input) {
        logger.error("decodeInput method not supported");
        return new PluginDataObject[0];
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    public static final void main(String [] args) {
        
        com.raytheon.edex.util.Util.isEmptyString(" ");
        
        ACARSRecord r = new ACARSRecord("/acars/2011-03-21_17:58:32.0/2TPYR4JA/null/42.85/-84.92/9754");
        System.out.println(r.getLatitude());
    }

}
