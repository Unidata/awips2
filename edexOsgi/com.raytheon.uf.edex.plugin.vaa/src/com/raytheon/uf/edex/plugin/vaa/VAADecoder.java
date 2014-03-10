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
package com.raytheon.uf.edex.plugin.vaa;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.vaa.decoder.VAAParser;

/**
 * Decoder for Volcanic Ash Advisory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 04, 2009       3267 jkorman     Initial creation
 * Nov 26, 2013       2582 njensen     Cleanup
 * Mar 10, 2014       2807 skorolev    Added MalformedDataException for VAA decoding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class VAADecoder {

    private IUFStatusHandler logger = UFStatus.getHandler(VAADecoder.class);

    /**
     * 
     * @param name
     */
    public VAADecoder() {

    }

    /**
     * 
     * @param data
     * @param headers
     * @return
     */
    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws MalformedDataException, IOException {

        String traceId = null;

        PluginDataObject[] decodedData = null;

        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        logger.debug(traceId + " - Decoding data");

        if (data != null && data.length > 0) {
            List<PluginDataObject> obsList = new ArrayList<PluginDataObject>();
            VAAParser parser = new VAAParser(data, traceId, headers);
            for (VAARecord record : parser) {
                if (record != null) {
                    // overwrite will only happen if a correction is issued
                    // within the same minute as the original
                    record.setOverwriteAllowed(true);
                    obsList.add(record);
                }
            }

            if ((obsList != null) && (obsList.size() > 0)) {
                decodedData = obsList.toArray(new PluginDataObject[obsList
                        .size()]);
            } else {
                decodedData = new PluginDataObject[0];
            }
        } else {
            logger.info(traceId + "- No data in file");
            decodedData = new PluginDataObject[0];
        }

        return decodedData;
    }

}
