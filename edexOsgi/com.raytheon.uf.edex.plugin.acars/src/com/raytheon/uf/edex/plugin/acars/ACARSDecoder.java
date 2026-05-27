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

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.acars.decoder.ACARSDataAdapter;

/**
 * ACARS Decoder.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009  1939       jkorman     Initial creation
 * Mar 27, 2014  2811       skorolev    Updated logger.
 * Aug 11, 2016  5757       nabowle     Handle null traceId. Drop deprecated base class.
 *
 * </pre>
 *
 * @author jkorman
 */

public class ACARSDecoder {

    private IUFStatusHandler logger = UFStatus.getHandler(ACARSDecoder.class);

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
     * @param data
     * @param headers
     * @return One record of decoded data.
     */
    public PluginDataObject[] decode(byte[] data, Headers headers) {

        PluginDataObject[] decodedData = null;

        ACARSDataAdapter adapter = new ACARSDataAdapter();

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
            if (traceId != null && !traceId.isEmpty()) {
                traceId = traceId + " - ";
            } else {
                traceId = "";
            }
        }
        try {
            decodedData = adapter.getACARSData(data, traceId, headers);
            if (decodedData != null) {
                logger.info(String.format("%sDecoded %d obs", traceId,
                        decodedData.length));
            } else {
                logger.info(String.format("%sDecoded no obs", traceId));
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
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }
}
